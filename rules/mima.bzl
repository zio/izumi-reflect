"MiMa (Migration Manager) binary compatibility checking."

load("//rules:providers.bzl", "ScalaInfo")
load("//rules:versions.bzl", "scala_binary_version")

# Previous release versions to check against, per Scala binary version.
_PREVIOUS_VERSIONS = {
    "default": ["2.2.5", "2.1.0", "1.0.0"],
    "3": ["2.2.5", "2.1.0"],
}

# MiMa exclusion filters. Each entry is a (problem_type, pattern) tuple.
# Patterns support wildcards (*) matching any suffix.
MIMA_EXCLUSIONS = [
    "Problem:izumi.reflect.macrortti.LightTypeTag$ParsedLightTypeTag*",
    "IncompatibleResultTypeProblem:izumi.reflect.macrortti.LightTypeTagRef#FullReference._1",
    "InheritedNewAbstractMethodProblem:izumi.reflect.macrortti.LightTypeTagRef*",
    "ReversedMissingMethodProblem:izumi.reflect.macrortti.LTTRenderables.*",
    "ReversedMissingMethodProblem:izumi.reflect.macrortti.LightTypeTag.binaryFormatVersion",
    "ReversedMissingMethodProblem:izumi.reflect.macrortti.LightTypeTagRef.*",
    "ReversedMissingMethodProblem:izumi.reflect.macrortti.LightTypeTagRef#AppliedNamedReference.*",
    "ReversedMissingMethodProblem:izumi.reflect.AnyTag.*",
    "Problem:izumi.reflect.TagMacro.*",
    "Problem:izumi.reflect.macrortti.LightTypeTagImpl*",
    "Problem:izumi.reflect.dottyreflection.*",
    "Problem:izumi.reflect.thirdparty.*",
    "Problem:izumi.reflect.internal.*",
    "Problem:izumi.reflect.ReflectionUtil*",
    "DirectMissingMethodProblem:izumi.reflect.macrortti.LightTypeTagImpl.*",
    "DirectMissingMethodProblem:izumi.reflect.macrortti.LightTypeTagInheritance.CtxExt",
    "MissingFieldProblem:izumi.reflect.macrortti.LightTypeTagInheritance.CtxExt",
    "FinalClassProblem:izumi.reflect.macrortti.LightTypeTagInheritance$CtxExt",
    "MissingTypesProblem:izumi.reflect.macrortti.LightTypeTagInheritance$Ctx*",
    "Problem:izumi.reflect.macrortti.LightTypeTagInheritance#Ctx*",
    "Problem:izumi.reflect.macrortti.LightTypeTagRef*",
    "Problem:izumi.reflect.macrortti.LightTypeTagUnpacker*",
]

def _maven_jar_url(group, artifact, version):
    return "https://repo1.maven.org/maven2/{}/{}/{}/{}-{}.jar".format(
        group.replace(".", "/"),
        artifact,
        version,
        artifact,
        version,
    )

def _mima_previous_jars_impl(module_ctx):
    """Download previous release JARs for MiMa comparison."""
    for bv in ["2.11", "2.12", "2.13", "3"]:
        versions = _PREVIOUS_VERSIONS.get(bv, _PREVIOUS_VERSIONS["default"])
        for v in versions:
            artifact = "izumi-reflect_" + bv
            name = "mima_prev_" + artifact.replace(".", "_").replace("-", "_") + "_" + v.replace(".", "_")
            _download_jar(
                name = name,
                url = _maven_jar_url("dev.zio", artifact, v),
            )

def _download_jar_impl(repository_ctx):
    repository_ctx.download(
        url = repository_ctx.attr.url,
        output = "prev.jar",
    )
    repository_ctx.file("BUILD.bazel", 'exports_files(["prev.jar"])\n')

_download_jar = repository_rule(
    implementation = _download_jar_impl,
    attrs = {"url": attr.string(mandatory = True)},
)

mima_previous_jars = module_extension(_mima_previous_jars_impl)

def _mima_check_impl(ctx):
    """Run MiMa to check binary compatibility."""
    current_jar = ctx.attr.current[ScalaInfo].output_jar
    scala_version = ctx.attr.current[ScalaInfo].scala_version

    # MiMa tool classpath
    mima_cp_files = []
    for target in ctx.attr._mima_cli:
        if JavaInfo in target:
            mima_cp_files.extend(target[JavaInfo].transitive_runtime_jars.to_list())
        else:
            mima_cp_files.extend(target.files.to_list())

    java_runtime = ctx.toolchains["@bazel_tools//tools/jdk:toolchain_type"].java.java_runtime
    mima_cp_str = ":".join(["$R/" + f.short_path for f in mima_cp_files])

    # Build exclusion filter grep pattern.
    # MiMa CLI output format: "fully.qualified.Name: ProblemType"
    # Exclusion format: "ProblemType:fully.qualified.Pattern*"
    grep_patterns = []
    for exc in ctx.attr.exclusions:
        parts = exc.split(":", 1)
        if len(parts) == 2:
            ptype = parts[0]
            pattern = parts[1].replace(".", "[.]").replace("*", ".*").replace("$", "[$]")
            if ptype == "Problem":
                # "Problem" is the base type — matches any problem
                grep_patterns.append(pattern + ": ")
            else:
                ptype_re = ptype.replace("*", ".*").replace("Problem", "")
                grep_patterns.append(pattern + ": " + ptype_re)
        else:
            grep_patterns.append(exc.replace(".", "[.]").replace("*", ".*"))
    filter_cmd = ""
    if grep_patterns:
        filter_cmd = " | grep -vE '" + "|".join(grep_patterns) + "'"

    # Build check commands for each previous version
    prev_jars = ctx.files.previous_jars
    checks = []
    for prev_jar in prev_jars:
        cmd = (
            'echo "Checking: {prev_name} -> current"\n' +
            'PROBLEMS=$("$R/{java}" -cp "{cp}" com.typesafe.tools.mima.cli.Main "$R/{prev}" "$R/{current}" 2>&1{filter} || true)\n' +
            'if [ -n "$PROBLEMS" ]; then\n' +
            '    echo "$PROBLEMS"\n' +
            '    echo "BINARY INCOMPATIBILITY against {prev_name}"\n' +
            '    FAILED=1\n' +
            "fi\n"
        ).format(
            java = java_runtime.java_executable_exec_path,
            cp = mima_cp_str,
            prev = prev_jar.short_path,
            prev_name = prev_jar.basename,
            current = current_jar.short_path,
            filter = filter_cmd,
        )
        checks.append(cmd)

    script_content = (
        "#!/usr/bin/env bash\n" +
        'if [[ -d "$0.runfiles" ]]; then R="$0.runfiles/_main"\n' +
        'elif [[ -n "$RUNFILES_DIR" ]]; then R="$RUNFILES_DIR/_main"\n' +
        'else R="$(dirname "$0")/../_main"; fi\n' +
        "FAILED=0\n" +
        "\n".join(checks) +
        'if [ "$FAILED" = "1" ]; then echo "MiMa check FAILED"; exit 1; fi\n' +
        'echo "MiMa check passed"\n'
    )

    script = ctx.actions.declare_file(ctx.label.name + ".sh")
    ctx.actions.write(script, script_content, is_executable = True)

    all_files = [current_jar] + prev_jars + mima_cp_files
    runfiles = ctx.runfiles(files = all_files)
    runfiles = runfiles.merge(ctx.runfiles(transitive_files = java_runtime.files))

    return [DefaultInfo(executable = script, runfiles = runfiles)]

mima_check_test = rule(
    implementation = _mima_check_impl,
    test = True,
    attrs = {
        "current": attr.label(mandatory = True, doc = "Current library target (must provide ScalaInfo)."),
        "previous_jars": attr.label_list(allow_files = [".jar"], doc = "Previous release JARs to check against."),
        "exclusions": attr.string_list(default = MIMA_EXCLUSIONS),
        "_mima_cli": attr.label_list(default = ["@scala_tools//:com_typesafe_mima_cli_2_13"]),
    },
    toolchains = ["@bazel_tools//tools/jdk:toolchain_type"],
)

def previous_jar_label(scala_binary_version, release_version):
    """Return the label for a previous release JAR."""
    artifact = "izumi_reflect_" + scala_binary_version.replace(".", "_")
    version = release_version.replace(".", "_")
    return "@mima_prev_" + artifact + "_" + version + "//:prev.jar"

def previous_jar_labels(full_version):
    """Return all previous JAR labels for a given Scala version."""
    bv = scala_binary_version(full_version)
    versions = _PREVIOUS_VERSIONS.get(bv, _PREVIOUS_VERSIONS["default"])
    return [previous_jar_label(bv, v) for v in versions]
