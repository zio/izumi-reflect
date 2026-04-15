"Core Scala compilation rule for JVM targets."

load("//rules:providers.bzl", "ScalaInfo")
load("//rules:versions.bzl", "scalac_main_class", "scala_major")

def _scala_library_impl(ctx):
    scala_version = ctx.attr.scala_version
    main_class = scalac_main_class(scala_version)

    # Collect source files
    srcs = ctx.files.srcs
    if not srcs:
        # Empty library (no sources) — produce an empty jar
        output_jar = ctx.actions.declare_file(ctx.label.name + ".jar")
        ctx.actions.run_shell(
            outputs = [output_jar],
            command = "echo PK\\x03\\x04 | head -c 4 > {out} && truncate -s 22 {out} || (cd /tmp && zip {out} -x '*' 2>/dev/null || true)".format(out = output_jar.path),
            mnemonic = "EmptyJar",
        )
        compile_jars = depset(
            direct = [output_jar],
            transitive = [dep[ScalaInfo].compile_jars for dep in ctx.attr.deps if ScalaInfo in dep],
        )
        runtime_jars = depset(
            direct = [output_jar],
            transitive = [dep[ScalaInfo].runtime_jars for dep in ctx.attr.deps if ScalaInfo in dep],
        )
        return [
            DefaultInfo(files = depset([output_jar])),
            ScalaInfo(
                output_jar = output_jar,
                compile_jars = compile_jars,
                runtime_jars = runtime_jars,
                scala_version = scala_version,
                platform = ctx.attr.platform,
            ),
        ]

    # Output artifacts
    classes_dir = ctx.actions.declare_directory(ctx.label.name + "_classes")
    output_jar = ctx.actions.declare_file(ctx.label.name + ".jar")

    # Compiler classpath (scalac itself) — need transitive deps for the compiler to run
    compiler_cp_files = []
    for target in ctx.attr.compiler_classpath:
        if JavaInfo in target:
            compiler_cp_files.extend(target[JavaInfo].transitive_runtime_jars.to_list())
        else:
            compiler_cp_files.extend(target.files.to_list())

    # Compilation classpath (deps) — need transitive deps for Maven targets
    dep_jars = []
    for target in ctx.attr.scala_library_classpath:
        if JavaInfo in target:
            dep_jars.extend(target[JavaInfo].transitive_runtime_jars.to_list())
        else:
            dep_jars.extend(target.files.to_list())
    for dep in ctx.attr.deps:
        if ScalaInfo in dep:
            dep_jars.extend(dep[ScalaInfo].compile_jars.to_list())
        elif JavaInfo in dep:
            dep_jars.extend(dep[JavaInfo].transitive_compile_time_jars.to_list())
        else:
            dep_jars.extend(dep.files.to_list())

    # Runtime classpath — full JARs with .sjsir/.nir content (not header jars)
    # Used for ScalaInfo.runtime_jars, needed by linkers.
    runtime_dep_jars = []
    for target in ctx.attr.scala_library_classpath:
        if JavaInfo in target:
            runtime_dep_jars.extend(target[JavaInfo].transitive_runtime_jars.to_list())
        else:
            runtime_dep_jars.extend(target.files.to_list())
    for dep in ctx.attr.deps:
        if ScalaInfo in dep:
            runtime_dep_jars.extend(dep[ScalaInfo].runtime_jars.to_list())
        elif JavaInfo in dep:
            runtime_dep_jars.extend(dep[JavaInfo].transitive_runtime_jars.to_list())
        else:
            runtime_dep_jars.extend(dep.files.to_list())

    # Compiler plugins — the plugin JAR for -Xplugin, plus transitive deps on compiler classpath
    plugin_jars = []
    plugin_cp_jars = []
    for target in ctx.attr.plugins:
        plugin_jars.extend(target.files.to_list())
        if JavaInfo in target:
            plugin_cp_jars.extend(target[JavaInfo].transitive_runtime_jars.to_list())
    # Add plugin transitive deps to compiler classpath
    if plugin_cp_jars:
        compiler_cp_files.extend(plugin_cp_jars)

    # Build scalac arguments
    args = ctx.actions.args()
    args.add("-d", classes_dir.path)

    if dep_jars:
        args.add("-classpath")
        args.add_joined(dep_jars, join_with = ctx.configuration.host_path_separator, map_each = _file_path)

    for plugin_jar in plugin_jars:
        args.add("-Xplugin:" + plugin_jar.path)

    args.add_all(ctx.attr.scalac_opts)
    args.add_all(srcs)

    # Write compiler classpath to an argfile to avoid command-line length limits
    compiler_cp_argfile = ctx.actions.declare_file(ctx.label.name + "_compiler_cp.txt")
    ctx.actions.write(
        output = compiler_cp_argfile,
        content = ctx.configuration.host_path_separator.join([f.path for f in compiler_cp_files]),
    )

    # Write scalac args to argfile
    scalac_argfile = ctx.actions.declare_file(ctx.label.name + "_scalac_args.txt")
    scalac_args_content = []
    if dep_jars:
        scalac_args_content.append("-d")
        scalac_args_content.append(classes_dir.path)
        scalac_args_content.append("-classpath")
        scalac_args_content.append(ctx.configuration.host_path_separator.join([f.path for f in dep_jars]))
    else:
        scalac_args_content.append("-d")
        scalac_args_content.append(classes_dir.path)

    for plugin_jar in plugin_jars:
        scalac_args_content.append("-Xplugin:" + plugin_jar.path)

    scalac_args_content.extend(ctx.attr.scalac_opts)
    scalac_args_content.extend([f.path for f in srcs])

    ctx.actions.write(
        output = scalac_argfile,
        content = "\n".join(scalac_args_content),
    )

    # Run scalac
    all_inputs = srcs + compiler_cp_files + dep_jars + plugin_jars + [scalac_argfile, compiler_cp_argfile]
    java_executable = ctx.toolchains["@bazel_tools//tools/jdk:toolchain_type"].java.java_runtime.java_executable_exec_path

    # Pre-create scoverage data directories. Instrumented library code writes
    # measurements to hardcoded paths during macro expansion, so the dirs must
    # exist even during compilation of dependents.
    mkdir_cmds = "mkdir -p /tmp/scoverage/{izumi-reflect-thirdparty-boopickle-shaded,izumi-reflect}_{jvm,js,native}_{2.11,2.12,2.13,3} 2>/dev/null; "
    for opt in ctx.attr.scalac_opts:
        if opt.startswith("-P:scoverage:dataDir:") or opt.startswith("-coverage-out:"):
            d = opt.split(":")[-1]
            mkdir_cmds += "mkdir -p '" + d + "' && "

    # Resolve @@NPROC@@ placeholder in argfile to actual CPU count (capped at 16, min 1)
    resolved_argfile = ctx.actions.declare_file(ctx.label.name + "_scalac_args_resolved.txt")

    ctx.actions.run_shell(
        outputs = [classes_dir, resolved_argfile],
        inputs = depset(all_inputs),
        tools = [ctx.toolchains["@bazel_tools//tools/jdk:toolchain_type"].java.java_runtime.files],
        command = (
            "{mkdir}" +
            "NPROC=$(( $(nproc 2>/dev/null || echo 8) - 1 )); " +
            "NPROC=$(( NPROC > 16 ? 16 : NPROC )); " +
            "NPROC=$(( NPROC < 1 ? 1 : NPROC )); " +
            'sed "s/@@NPROC@@/$NPROC/g" {argfile} > {resolved} && ' +
            '{java} -cp "$(cat {cp_file})" {main_class} "@{resolved}" '
        ).format(
            mkdir = mkdir_cmds,
            java = java_executable,
            cp_file = compiler_cp_argfile.path,
            main_class = main_class,
            argfile = scalac_argfile.path,
            resolved = resolved_argfile.path,
        ),
        mnemonic = "ScalaCompile",
        progress_message = "Compiling Scala (%s/%s) %s" % (ctx.attr.platform, ctx.attr.scala_version, ctx.label),
    )

    # Package into JAR with Automatic-Module-Name manifest
    _create_jar(ctx, classes_dir, output_jar, ctx.attr.automatic_module_name)

    # Build providers
    compile_jars = depset(
        direct = [output_jar],
        transitive = [dep[ScalaInfo].compile_jars for dep in ctx.attr.deps if ScalaInfo in dep],
    )
    runtime_jars = depset(
        direct = [output_jar],
        transitive = [depset(runtime_dep_jars)],
    )

    return [
        DefaultInfo(files = depset([output_jar])),
        ScalaInfo(
            output_jar = output_jar,
            compile_jars = compile_jars,
            runtime_jars = runtime_jars,
            scala_version = scala_version,
            platform = ctx.attr.platform,
        ),
    ]

def _file_path(f):
    return f.path

def _create_jar(ctx, classes_dir, output_jar, automatic_module_name = ""):
    """Create a JAR from a classes directory, optionally with Automatic-Module-Name manifest."""
    jar_tool = ctx.toolchains["@bazel_tools//tools/jdk:toolchain_type"].java.java_runtime.java_home + "/bin/jar"
    manifest_cmd = ""
    if automatic_module_name:
        manifest_cmd = (
            "MANIFEST=/tmp/manifest_$$; " +
            "echo 'Manifest-Version: 1.0' > $MANIFEST; " +
            "echo 'Automatic-Module-Name: " + automatic_module_name + "' >> $MANIFEST; "
        )
    jar_flag = "cfm" if automatic_module_name else "cf"
    manifest_arg = "$MANIFEST " if automatic_module_name else ""
    cleanup = "rm -f $MANIFEST; " if automatic_module_name else ""

    ctx.actions.run_shell(
        outputs = [output_jar],
        inputs = [classes_dir],
        tools = [ctx.toolchains["@bazel_tools//tools/jdk:toolchain_type"].java.java_runtime.files],
        command = (
            "{manifest_cmd}" +
            'if [ -z "$(ls -A {dir})" ]; then ' +
            "echo PK > /tmp/_empty_$$; " +
            "{jar} {jar_flag} {out} {manifest_arg}-C /tmp _empty_$$; " +
            "rm /tmp/_empty_$$; " +
            "else " +
            "{jar} {jar_flag} {out} {manifest_arg}-C {dir} .; " +
            "fi; " +
            "{cleanup}"
        ).format(
            manifest_cmd = manifest_cmd,
            jar = jar_tool,
            jar_flag = jar_flag,
            out = output_jar.path,
            manifest_arg = manifest_arg,
            dir = classes_dir.path,
            cleanup = cleanup,
        ),
        mnemonic = "ScalaJar",
        progress_message = "Packaging JAR %s" % ctx.label,
    )

# Common attributes shared by all scala rules
_COMMON_ATTRS = {
    "srcs": attr.label_list(allow_files = [".scala", ".java"]),
    "deps": attr.label_list(),
    "scala_version": attr.string(mandatory = True),
    "platform": attr.string(default = "jvm"),
    "scalac_opts": attr.string_list(default = []),
    "compiler_classpath": attr.label_list(
        doc = "JARs constituting the Scala compiler classpath.",
    ),
    "scala_library_classpath": attr.label_list(
        doc = "JARs for the Scala standard library (compile classpath).",
    ),
    "plugins": attr.label_list(
        doc = "Compiler plugin JARs.",
    ),
    "automatic_module_name": attr.string(
        default = "",
        doc = "Java 9+ Automatic-Module-Name manifest attribute.",
    ),
}

scala_library = rule(
    implementation = _scala_library_impl,
    attrs = _COMMON_ATTRS,
    toolchains = ["@bazel_tools//tools/jdk:toolchain_type"],
)
