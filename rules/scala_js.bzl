"Scala.js linking rule — links compiled .sjsir JARs into JavaScript output."

load("@rules_java//java:defs.bzl", "JavaInfo")
load("//rules:providers.bzl", "ScalaInfo")

def _scala_js_link_impl(ctx):
    output_dir = ctx.actions.declare_directory(ctx.label.name + "_linked")

    # Linker tool classpath (scalajs-cli + scalajs-linker)
    linker_cp_files = []
    for target in ctx.attr.linker_classpath:
        if JavaInfo in target:
            linker_cp_files.extend(target[JavaInfo].transitive_runtime_jars.to_list())
        else:
            linker_cp_files.extend(target.files.to_list())

    # Input JARs to link (compiled code + all transitive deps)
    input_jars = []
    for dep in ctx.attr.deps:
        if ScalaInfo in dep:
            input_jars.extend(dep[ScalaInfo].runtime_jars.to_list())
        elif JavaInfo in dep:
            input_jars.extend(dep[JavaInfo].transitive_runtime_jars.to_list())
        else:
            input_jars.extend(dep.files.to_list())

    # Deduplicate
    seen = {}
    unique_input_jars = []
    for f in input_jars:
        if f.path not in seen:
            seen[f.path] = True
            unique_input_jars.append(f)
    input_jars = unique_input_jars

    java_runtime = ctx.toolchains["@bazel_tools//tools/jdk:toolchain_type"].java.java_runtime
    java_executable = java_runtime.java_executable_exec_path

    # Build command directly (scalajs-cli does not support @argfile)
    linker_cp_str = ctx.configuration.host_path_separator.join([f.path for f in linker_cp_files])
    jar_args = " ".join([f.path for f in input_jars])

    mode_flag = "--fullOpt" if ctx.attr.full_opt else "--fastOpt"
    main_flag = ""
    if ctx.attr.main_method:
        main_flag = "--mainMethod " + ctx.attr.main_method

    all_inputs = linker_cp_files + input_jars

    ctx.actions.run_shell(
        outputs = [output_dir],
        inputs = depset(all_inputs),
        tools = [java_runtime.files],
        command = '{java} -cp "{cp}" org.scalajs.cli.Scalajsld --outputDir {out} {mode} --moduleKind {kind} {main} {jars}'.format(
            java = java_executable,
            cp = linker_cp_str,
            out = output_dir.path,
            mode = mode_flag,
            kind = ctx.attr.module_kind,
            main = main_flag,
            jars = jar_args,
        ),
        mnemonic = "ScalaJSLink",
        progress_message = "Linking Scala.js %s" % ctx.label,
    )

    return [DefaultInfo(files = depset([output_dir]))]

scala_js_link = rule(
    implementation = _scala_js_link_impl,
    attrs = {
        "deps": attr.label_list(
            mandatory = True,
            doc = "Compiled Scala.js library targets to link.",
        ),
        "linker_classpath": attr.label_list(
            doc = "JARs for the Scala.js linker tool (scalajs-cli + scalajs-linker).",
        ),
        "module_kind": attr.string(
            default = "CommonJSModule",
            values = ["NoModule", "ESModule", "CommonJSModule"],
        ),
        "full_opt": attr.bool(default = False),
        "main_method": attr.string(
            default = "",
            doc = "Entry point method, e.g. 'com.example.Main.main'. Empty for libraries.",
        ),
    },
    toolchains = ["@bazel_tools//tools/jdk:toolchain_type"],
)
