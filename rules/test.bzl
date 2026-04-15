"Test rules for Scala multiplatform projects."

load("//rules:providers.bzl", "ScalaInfo")
load("//rules:versions.bzl", "scalac_main_class", "scala_major")

def _scala_jvm_test_impl(ctx):
    """JVM test: run ScalaTest Runner with auto-discovered test suites."""
    test_jar = ctx.attr.compiled_tests[ScalaInfo].output_jar

    # Full runtime classpath
    runtime_jars = []
    runtime_jars.append(test_jar)
    runtime_jars.extend(ctx.attr.compiled_tests[ScalaInfo].runtime_jars.to_list())
    for dep in ctx.attr.runtime_deps:
        if JavaInfo in dep:
            runtime_jars.extend(dep[JavaInfo].transitive_runtime_jars.to_list())
        elif ScalaInfo in dep:
            runtime_jars.extend(dep[ScalaInfo].runtime_jars.to_list())
        else:
            runtime_jars.extend(dep.files.to_list())

    runtime_jars = _dedup_files(runtime_jars)
    java_runtime = ctx.toolchains["@bazel_tools//tools/jdk:toolchain_type"].java.java_runtime

    classpath_str = ":".join(["$RUNFILES_DIR/" + f.short_path for f in runtime_jars])

    script_content = """#!/usr/bin/env bash
set -e
if [[ -d "$0.runfiles" ]]; then
    RUNFILES_DIR="$0.runfiles/_main"
elif [[ -n "$RUNFILES_DIR" ]]; then
    RUNFILES_DIR="$RUNFILES_DIR/_main"
else
    RUNFILES_DIR="$(dirname "$0")/../_main"
fi
TEST_CLASSES_DIR=$(mktemp -d)
trap "rm -rf $TEST_CLASSES_DIR" EXIT
(cd "$TEST_CLASSES_DIR" && "$RUNFILES_DIR/{jar}" xf "$RUNFILES_DIR/{test_jar}")
exec "$RUNFILES_DIR/{java}" -cp "{classpath}" \\
    org.scalatest.tools.Runner \\
    -R "$TEST_CLASSES_DIR" \\
    -oDF
""".format(
        java = java_runtime.java_executable_exec_path,
        jar = java_runtime.java_home + "/bin/jar",
        test_jar = test_jar.short_path,
        classpath = classpath_str,
    )

    script = ctx.actions.declare_file(ctx.label.name + ".sh")
    ctx.actions.write(script, script_content, is_executable = True)

    runfiles = ctx.runfiles(files = runtime_jars)
    runfiles = runfiles.merge(ctx.runfiles(transitive_files = java_runtime.files))

    return [DefaultInfo(executable = script, runfiles = runfiles)]

scala_jvm_test = rule(
    implementation = _scala_jvm_test_impl,
    test = True,
    attrs = {
        "compiled_tests": attr.label(mandatory = True),
        "runtime_deps": attr.label_list(),
    },
    toolchains = ["@bazel_tools//tools/jdk:toolchain_type"],
)

def _scala_linked_test_impl(ctx):
    """JS/Native test: discover suites, generate runner, compile, link, execute.

    Pipeline:
    1. Scan compiled test JAR for ScalaTest suite class names
    2. Generate a TestRunner.scala that invokes Runner.main with discovered suites
    3. Compile the runner
    4. Link (JS → scalajsld, Native → scala-native-cli)
    5. Execute (JS → node, Native → run binary)
    """
    platform = ctx.attr.platform
    scala_version = ctx.attr.scala_version
    test_jar = ctx.attr.compiled_tests[ScalaInfo].output_jar

    java_runtime = ctx.toolchains["@bazel_tools//tools/jdk:toolchain_type"].java.java_runtime
    java_executable = java_runtime.java_executable_exec_path
    jar_tool = java_runtime.java_home + "/bin/jar"

    # ── Step 1: Discover concrete test class names from the compiled JAR ──
    # Uses javap to check ACC_ABSTRACT flag, filtering out abstract classes and traits.
    test_classes_file = ctx.actions.declare_file(ctx.label.name + "_test_classes.txt")
    javap_tool = java_runtime.java_home + "/bin/javap"
    ctx.actions.run_shell(
        outputs = [test_classes_file],
        inputs = [test_jar],
        tools = [java_runtime.files],
        command = """
            CANDIDATES=$({jar} tf {test_jar} \
                | grep '\\.class$' \
                | grep -v '\\$' \
                | grep -v 'package' \
                | sed 's|/|.|g;s|\\.class$||' \
                | grep -iE '(Test|Spec|Suite)$')
            > {out}
            for cls in $CANDIDATES; do
                if ! {javap} -cp {test_jar} "$cls" 2>/dev/null | head -5 | grep -qE 'abstract class|interface |trait '; then
                    echo "$cls" >> {out}
                fi
            done
        """.format(
            jar = jar_tool,
            javap = javap_tool,
            test_jar = test_jar.path,
            out = test_classes_file.path,
        ),
        mnemonic = "DiscoverTests",
        progress_message = "Discovering test suites in %s" % ctx.label,
    )

    # ── Step 2: Generate TestRunner.scala ──
    runner_source = ctx.actions.declare_file(ctx.label.name + "_TestRunner.scala")
    if platform == "js":
        _generate_js_runner(ctx, test_classes_file, runner_source)
    else:
        _generate_native_runner(ctx, test_classes_file, runner_source)

    # ── Step 3: Compile the runner ──
    runner_classes_dir = ctx.actions.declare_directory(ctx.label.name + "_runner_classes")
    runner_jar = ctx.actions.declare_file(ctx.label.name + "_runner.jar")

    # Compiler classpath
    compiler_cp_files = []
    for target in ctx.attr.compiler_classpath:
        if JavaInfo in target:
            compiler_cp_files.extend(target[JavaInfo].transitive_runtime_jars.to_list())
        else:
            compiler_cp_files.extend(target.files.to_list())

    # Compile classpath: test jar + ALL its deps (ScalaInfo + JavaInfo + platform libs)
    compile_cp_files = [test_jar]
    compile_cp_files.extend(ctx.attr.compiled_tests[ScalaInfo].compile_jars.to_list())
    compile_cp_files.extend(ctx.attr.compiled_tests[ScalaInfo].runtime_jars.to_list())
    for target in ctx.attr.scala_library_classpath:
        if JavaInfo in target:
            compile_cp_files.extend(target[JavaInfo].transitive_runtime_jars.to_list())
        else:
            compile_cp_files.extend(target.files.to_list())

    # Plugins (JS needs scalajs plugin, Native needs nscplugin)
    plugin_files = []
    for target in ctx.attr.plugins:
        plugin_files.extend(target.files.to_list())

    main_class = scalac_main_class(scala_version)
    compiler_cp_str = ctx.configuration.host_path_separator.join([f.path for f in compiler_cp_files])
    compile_cp_str = ctx.configuration.host_path_separator.join([f.path for f in _dedup_files(compile_cp_files)])
    plugin_args = " ".join(["-Xplugin:" + f.path for f in plugin_files])
    extra_opts = " ".join(ctx.attr.runner_scalac_opts)

    all_compile_inputs = [runner_source] + compiler_cp_files + compile_cp_files + plugin_files

    ctx.actions.run_shell(
        outputs = [runner_classes_dir],
        inputs = depset(all_compile_inputs),
        tools = [java_runtime.files],
        command = '{java} -cp "{compiler_cp}" {main} -d {out} -classpath "{cp}" {plugins} {opts} {src}'.format(
            java = java_executable,
            compiler_cp = compiler_cp_str,
            main = main_class,
            out = runner_classes_dir.path,
            cp = compile_cp_str,
            plugins = plugin_args,
            opts = extra_opts,
            src = runner_source.path,
        ),
        mnemonic = "CompileTestRunner",
        progress_message = "Compiling test runner for %s" % ctx.label,
    )

    # Package runner classes into JAR
    ctx.actions.run_shell(
        outputs = [runner_jar],
        inputs = [runner_classes_dir],
        tools = [java_runtime.files],
        command = '{jar} cf {out} -C {dir} .'.format(
            jar = jar_tool, out = runner_jar.path, dir = runner_classes_dir.path,
        ),
        mnemonic = "PackageTestRunner",
    )

    # ── Step 4: Link ──
    # Collect all runtime JARs for linking
    link_jars = [runner_jar, test_jar]
    link_jars.extend(ctx.attr.compiled_tests[ScalaInfo].runtime_jars.to_list())
    for target in ctx.attr.scala_library_classpath:
        if JavaInfo in target:
            link_jars.extend(target[JavaInfo].transitive_runtime_jars.to_list())
        else:
            link_jars.extend(target.files.to_list())
    link_jars = _dedup_files(link_jars)

    # Linker tool classpath
    linker_cp_files = []
    for target in ctx.attr.linker_classpath:
        if JavaInfo in target:
            linker_cp_files.extend(target[JavaInfo].transitive_runtime_jars.to_list())
        else:
            linker_cp_files.extend(target.files.to_list())

    if platform == "js":
        return _link_and_run_js(ctx, java_runtime, linker_cp_files, link_jars, runner_jar)
    else:
        return _link_and_run_native(ctx, java_runtime, linker_cp_files, link_jars, runner_jar)

def _generate_js_runner(ctx, test_classes_file, runner_source):
    """Generate a JS test runner that instantiates and runs suites directly.

    org.scalatest.tools.Runner is JVM-only. For JS, we instantiate each suite
    and call run() with a custom reporter to detect failures.
    """
    ctx.actions.run_shell(
        outputs = [runner_source],
        inputs = [test_classes_file],
        command = """
            {{
                echo 'package generated'
                echo 'object TestRunner {{'
                echo '  def main(args: Array[String]): Unit = {{'
                echo '    val suites = List[org.scalatest.Suite]('
                first=true
                while IFS= read -r cls; do
                    if [ -n "$cls" ]; then
                        if [ "$first" = true ]; then
                            first=false
                        else
                            echo ','
                        fi
                        printf "      new %s()" "$cls"
                    fi
                done < {classes}
                echo ''
                echo '    )'
                echo '    var failed = 0'
                echo '    var passed = 0'
                echo '    suites.foreach {{ suite =>'
                echo '      val tracker = new org.scalatest.Tracker'
                echo '      var suiteFailed = false'
                echo '      val reporter = new org.scalatest.Reporter {{'
                echo '        def apply(event: org.scalatest.events.Event): Unit = event match {{'
                echo '          case e: org.scalatest.events.TestFailed =>'
                echo '            suiteFailed = true'
                echo '            failed += 1'
                echo '            println(s"FAILED: ${{e.testName}}")'
                echo '            e.throwable.foreach(_.printStackTrace())'
                echo '          case e: org.scalatest.events.TestSucceeded =>'
                echo '            passed += 1'
                echo '          case e: org.scalatest.events.RunAborted =>'
                echo '            suiteFailed = true'
                echo '            failed += 1'
                echo '            println(s"ABORTED: ${{e.message}}")'
                echo '          case _ =>'
                echo '        }}'
                echo '      }}'
                echo '      println(s"Running: ${{suite.getClass.getName}}")'
                echo '      suite.run(None, org.scalatest.Args(reporter, tracker = tracker))'
                echo '    }}'
                echo '    println(s"Tests: $$passed passed, $$failed failed")'
                echo '    if (failed > 0) {{'
                echo '      println("TEST SUITE FAILED")'
                echo '      throw new RuntimeException(s"$$failed tests failed")'
                echo '    }}'
                echo '  }}'
                echo '}}'
            }} > {out}
        """.format(classes = test_classes_file.path, out = runner_source.path),
        mnemonic = "GenerateJSTestRunner",
    )

def _generate_native_runner(ctx, test_classes_file, runner_source):
    """Generate a Native test runner using direct suite instantiation."""
    # Same approach as JS — Runner.main doesn't exist in native ScalaTest either.
    _generate_js_runner(ctx, test_classes_file, runner_source)

def _link_and_run_js(ctx, java_runtime, linker_cp_files, link_jars, runner_jar):
    """Link JS test and create Node.js runner script."""
    java_executable = java_runtime.java_executable_exec_path
    output_dir = ctx.actions.declare_directory(ctx.label.name + "_js_linked")

    linker_cp_str = ctx.configuration.host_path_separator.join([f.path for f in linker_cp_files])
    jar_args = " ".join([f.path for f in link_jars])

    all_link_inputs = linker_cp_files + link_jars + [runner_jar]

    ctx.actions.run_shell(
        outputs = [output_dir],
        inputs = depset(all_link_inputs),
        tools = [java_runtime.files],
        command = '{java} -cp "{cp}" org.scalajs.cli.Scalajsld --outputDir {out} --fastOpt --moduleKind CommonJSModule --mainMethod generated.TestRunner.main {jars}'.format(
            java = java_executable,
            cp = linker_cp_str,
            out = output_dir.path,
            jars = jar_args,
        ),
        mnemonic = "ScalaJSLinkTests",
        progress_message = "Linking JS tests %s" % ctx.label,
    )

    # Create Node.js test runner script
    script_content = """#!/usr/bin/env bash
set -e
if [[ -d "$0.runfiles" ]]; then
    RUNFILES_DIR="$0.runfiles/_main"
elif [[ -n "$RUNFILES_DIR" ]]; then
    RUNFILES_DIR="$RUNFILES_DIR/_main"
else
    RUNFILES_DIR="$(dirname "$0")/../_main"
fi
exec node "$RUNFILES_DIR/{linked_dir}/main.js"
""".format(linked_dir = output_dir.short_path)

    script = ctx.actions.declare_file(ctx.label.name + ".sh")
    ctx.actions.write(script, script_content, is_executable = True)

    runfiles = ctx.runfiles(files = [output_dir])
    return [DefaultInfo(executable = script, runfiles = runfiles)]

def _link_and_run_native(ctx, java_runtime, linker_cp_files, link_jars, runner_jar):
    """Link Native test and create runner."""
    java_executable = java_runtime.java_executable_exec_path
    output_binary = ctx.actions.declare_file(ctx.label.name + "_bin")

    linker_cp_str = ctx.configuration.host_path_separator.join([f.path for f in linker_cp_files])
    jar_args = " ".join([f.path for f in link_jars])

    all_link_inputs = linker_cp_files + link_jars + [runner_jar]

    ctx.actions.run_shell(
        outputs = [output_binary],
        inputs = depset(all_link_inputs),
        tools = [java_runtime.files],
        use_default_shell_env = True,
        command = '{java} -cp "{cp}" scala.scalanative.cli.ScalaNativeLd -o {out} --gc immix --mode debug --compile-option -fexceptions --linking-option -lstdc++ --linking-option -Wl,--allow-multiple-definition --main generated.TestRunner {jars}'.format(
            java = java_executable,
            cp = linker_cp_str,
            out = output_binary.path,
            jars = jar_args,
        ),
        mnemonic = "ScalaNativeLinkTests",
        progress_message = "Linking Native tests %s" % ctx.label,
    )

    # Create test runner script that executes the native binary
    script_content = """#!/usr/bin/env bash
set -e
if [[ -d "$0.runfiles" ]]; then
    RUNFILES_DIR="$0.runfiles/_main"
elif [[ -n "$RUNFILES_DIR" ]]; then
    RUNFILES_DIR="$RUNFILES_DIR/_main"
else
    RUNFILES_DIR="$(dirname "$0")/../_main"
fi
exec "$RUNFILES_DIR/{binary}"
""".format(binary = output_binary.short_path)

    script = ctx.actions.declare_file(ctx.label.name + ".sh")
    ctx.actions.write(script, script_content, is_executable = True)

    runfiles = ctx.runfiles(files = [output_binary])
    return [DefaultInfo(executable = script, runfiles = runfiles)]

def _dedup_files(files):
    seen = {}
    result = []
    for f in files:
        if f.path not in seen:
            seen[f.path] = True
            result.append(f)
    return result

scala_linked_test = rule(
    implementation = _scala_linked_test_impl,
    test = True,
    attrs = {
        "compiled_tests": attr.label(mandatory = True),
        "platform": attr.string(mandatory = True, values = ["js", "native"]),
        "scala_version": attr.string(mandatory = True),
        "compiler_classpath": attr.label_list(),
        "scala_library_classpath": attr.label_list(),
        "plugins": attr.label_list(),
        "linker_classpath": attr.label_list(),
        "runner_scalac_opts": attr.string_list(default = []),
    },
    toolchains = ["@bazel_tools//tools/jdk:toolchain_type"],
)
