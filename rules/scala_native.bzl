"Scala Native linking rule — links compiled .nir JARs into a native executable."

load("//rules:providers.bzl", "ScalaInfo")

def _scala_native_link_impl(ctx):
    output = ctx.actions.declare_file(ctx.label.name)

    # Linker tool classpath (scala-native-cli)
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

    # Build command directly (scala-native-cli does not support @argfile)
    linker_cp_str = ctx.configuration.host_path_separator.join([f.path for f in linker_cp_files])
    jar_args = " ".join([f.path for f in input_jars])

    lto_flag = ""
    if ctx.attr.lto != "none":
        lto_flag = "--lto " + ctx.attr.lto

    all_inputs = linker_cp_files + input_jars

    ctx.actions.run_shell(
        outputs = [output],
        inputs = depset(all_inputs),
        tools = [java_runtime.files],
        command = '{java} -cp "{cp}" scala.scalanative.cli.ScalaNativeLd -o {out} --gc {gc} --mode {mode} {lto} --compile-option -fexceptions --linking-option -lstdc++ --linking-option -Wl,--allow-multiple-definition --main {main} {jars}'.format(
            java = java_executable,
            cp = linker_cp_str,
            out = output.path,
            gc = ctx.attr.gc,
            mode = ctx.attr.mode,
            lto = lto_flag,
            main = ctx.attr.main_class,
            jars = jar_args,
        ),
        mnemonic = "ScalaNativeLink",
        progress_message = "Linking Scala Native %s" % ctx.label,
    )

    return [DefaultInfo(
        files = depset([output]),
        executable = output,
    )]

scala_native_link = rule(
    implementation = _scala_native_link_impl,
    attrs = {
        "deps": attr.label_list(
            mandatory = True,
            doc = "Compiled Scala Native library targets to link.",
        ),
        "linker_classpath": attr.label_list(
            doc = "JARs for the Scala Native linker tool (scala-native-cli).",
        ),
        "main_class": attr.string(
            mandatory = True,
            doc = "Main class for the executable.",
        ),
        "gc": attr.string(
            default = "immix",
            values = ["immix", "commix", "boehm", "none"],
        ),
        "mode": attr.string(
            default = "debug",
            values = ["debug", "release-fast", "release-full"],
        ),
        "lto": attr.string(
            default = "none",
            values = ["none", "thin", "full"],
        ),
    },
    executable = True,
    toolchains = ["@bazel_tools//tools/jdk:toolchain_type"],
)
