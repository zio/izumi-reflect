"Multiplatform macros: generate targets for all platform x version combinations."

load("//rules:scala.bzl", "scala_library")
load("//rules:test.bzl", "scala_jvm_test", "scala_linked_test")
load("//rules:sources.bzl", "versioned_srcs")
load(
    "//rules:versions.bzl",
    "ALL_PLATFORMS",
    "PLATFORM_SCALA_VERSIONS",
    "SCALAJS_LINKER_LABEL",
    "SCALANATIVE_LINKER_LABEL",
    "kind_projector_label",
    "maven_label",
    "scala_compiler_labels",
    "scala_library_labels",
    "scala_major",
    "scala_short_version",
    "scalajs_compiler_plugin_label",
    "scalajs_library_labels",
    "scalanative_compiler_plugin_label",
    "scalanative_library_labels",
    "scalatest_label",
    "SCALAC_CI_DOWNGRADE",
    "SCALAC_CI_UPGRADE_212",
    "SCALAC_CI_UPGRADE_213",
    "SCALAC_RELEASE_OPTS_212",
    "SCALAC_RELEASE_OPTS_213",
    "SCALA_212",
    "SCALA_213",
    "scoverage_plugin_label",
    "scoverage_runtime_label",
    "version_scalac_opts",
)

_GROUP_ID = "dev.zio"

def _target_name(name, platform, full_version):
    """Generate target name: e.g. 'izumi-reflect_jvm_2.13'."""
    return name + "_" + platform + "_" + scala_short_version(full_version)

def _resolve_multiplatform_dep(dep, platform, full_version):
    """Resolve a multiplatform dep label to the correct variant.

    Examples:
        ":foo" -> ":foo_jvm_2.13"
        "//pkg/foo" -> "//pkg/foo:foo_jvm_2.13"
        "//pkg/foo:bar" -> "//pkg/foo:bar_jvm_2.13"
        "@scala_2_13//:lib" -> "@scala_2_13//:lib" (unchanged)
    """
    if dep.startswith("@"):
        return dep

    sv = scala_short_version(full_version)
    suffix = "_" + platform + "_" + sv

    # Check if this already has a platform suffix
    for p in ALL_PLATFORMS:
        for v in ["2.11", "2.12", "2.13", "3"]:
            if dep.endswith("_" + p + "_" + v):
                return dep

    if ":" in dep:
        return dep + suffix
    elif dep.startswith("//"):
        pkg_name = dep.split("/")[-1]
        return dep + ":" + pkg_name + suffix
    else:
        return ":" + dep + suffix

def scala_multiplatform_library(
        name,
        srcs_root = "src/main",
        base_dir = "",
        deps = [],
        extra_scalac_opts = [],
        remove_scalac_opts = [],
        platforms = None,
        scala_versions = None,
        release_mode = False,
        visibility = None,
        tags = []):
    """Generate scala_library targets for all platform x version combinations.

    Args:
        name: Base target name. Generated targets: {name}_{platform}_{version}.
        srcs_root: Source root directory, e.g. "src/main".
        base_dir: Path prefix from BUILD file to module root (e.g. "izumi-reflect/izumi-reflect").
        deps: Multiplatform deps, auto-resolved to matching variant.
        extra_scalac_opts: Additional scalac options beyond version defaults.
        remove_scalac_opts: Scalac options to remove from version defaults.
        platforms: Platforms to build for. Defaults to ALL_PLATFORMS.
        scala_versions: Dict of platform -> [full_version]. Defaults to PLATFORM_SCALA_VERSIONS.
        release_mode: Enable release-mode optimizations (inlining).
        visibility: Bazel visibility.
        tags: Bazel tags.
    """
    if platforms == None:
        platforms = ALL_PLATFORMS
    if scala_versions == None:
        scala_versions = PLATFORM_SCALA_VERSIONS

    all_targets = []

    for platform in platforms:
        versions = scala_versions.get(platform, [])
        for full_version in versions:
            target_name = _target_name(name, platform, full_version)
            all_targets.append(target_name)

            # Source files
            srcs = native.glob(versioned_srcs(srcs_root, full_version, platform, base_dir))

            # Resolve deps
            resolved_deps = [_resolve_multiplatform_dep(d, platform, full_version) for d in deps]

            # Auto-add provided deps based on Scala version
            if scala_major(full_version) == 2:
                resolved_deps.append(maven_label("org.scala-lang:scala-reflect:" + full_version, full_version))
            else:
                resolved_deps.append(maven_label("org.scala-lang:scala3-compiler_3:" + full_version, full_version))

            # Compiler classpath
            compiler_cp = scala_compiler_labels(full_version)

            # Library classpath
            lib_cp = scala_library_labels(full_version)

            # Compiler plugins
            plugins = []
            if scala_major(full_version) == 2:
                kp = kind_projector_label(full_version)
                if kp:
                    plugins.append(kp)

            # Scalac options
            scalac_opts = version_scalac_opts(full_version, release_mode = release_mode)
            scalac_opts = scalac_opts + extra_scalac_opts
            if remove_scalac_opts:
                scalac_opts = [o for o in scalac_opts if o not in remove_scalac_opts]

            # Platform-specific adjustments
            if platform == "js":
                js_plugin = scalajs_compiler_plugin_label(full_version)
                if js_plugin:
                    plugins.append(js_plugin)
                else:
                    # Scala 3: built-in scalajs support
                    scalac_opts = scalac_opts + ["-scalajs"]

                lib_cp = lib_cp + scalajs_library_labels(full_version)
                compiler_cp = compiler_cp + _scalajs_compiler_support_labels(full_version)

            elif platform == "native":
                native_plugin = scalanative_compiler_plugin_label(full_version)
                if native_plugin:
                    plugins.append(native_plugin)

                lib_cp = lib_cp + scalanative_library_labels(full_version)

            # CI mode: promote warnings to errors (Scala 2.12/2.13 only)
            ci_opts = []
            ci_remove = []
            if full_version == SCALA_212:
                ci_opts = SCALAC_CI_UPGRADE_212
                ci_remove = SCALAC_CI_DOWNGRADE
            elif full_version == SCALA_213:
                ci_opts = SCALAC_CI_UPGRADE_213
                ci_remove = SCALAC_CI_DOWNGRADE

            # Release mode: inlining optimizations (Scala 2.12/2.13 only)
            release_opts = []
            if full_version == SCALA_212:
                release_opts = SCALAC_RELEASE_OPTS_212
            elif full_version == SCALA_213:
                release_opts = SCALAC_RELEASE_OPTS_213

            # Scoverage: conditionally add plugin + runtime (JVM only, Scala 2 only)
            scov_plugins = []
            scov_deps = []
            scov_opts = []
            if platform == "jvm":
                sp = scoverage_plugin_label(full_version)
                sr = scoverage_runtime_label(full_version)
                if sp and sr:
                    scov_plugins = [sp]
                    scov_deps = [sr]
                    scov_opts = [
                        "-P:scoverage:dataDir:/tmp/scoverage/" + target_name,
                        "-P:scoverage:sourceRoot:.",
                    ]
                elif scala_major(full_version) == 3:
                    scov_opts = ["-coverage-out:/tmp/scoverage/" + target_name]

            # Automatic-Module-Name: dev.zio.{module-name-with-dots}
            module_name_attr = _GROUP_ID.replace("-", ".") + "." + name.replace("-", ".")

            # Combine conditional opts
            all_opts = scalac_opts
            if ci_opts:
                all_opts = all_opts + select({
                    "//:ci": [o for o in ci_opts if o not in ci_remove],
                    "//conditions:default": [],
                })
                # In CI, also remove the -Wconf:any:warning that's in the base opts
                # (handled by replacing it — ci_opts already contains -Wconf:any:error)
            if release_opts:
                all_opts = all_opts + select({
                    "//:release": release_opts,
                    "//conditions:default": [],
                })
            if scov_opts:
                all_opts = all_opts + select({
                    "//:scoverage": scov_opts,
                    "//conditions:default": [],
                })

            scala_library(
                name = target_name,
                srcs = srcs,
                deps = resolved_deps + select({
                    "//:scoverage": scov_deps,
                    "//conditions:default": [],
                }) if scov_deps else resolved_deps,
                scala_version = full_version,
                platform = platform,
                scalac_opts = all_opts,
                compiler_classpath = compiler_cp,
                scala_library_classpath = lib_cp,
                plugins = plugins + select({
                    "//:scoverage": scov_plugins,
                    "//conditions:default": [],
                }) if scov_plugins else plugins,
                automatic_module_name = module_name_attr,
                visibility = visibility,
                tags = tags,
            )

    # Default alias (jvm + last Scala version in list)
    if all_targets:
        default_target = _target_name(name, "jvm", PLATFORM_SCALA_VERSIONS["jvm"][-1])
        if default_target in all_targets:
            native.alias(
                name = name,
                actual = ":" + default_target,
                visibility = visibility,
            )

def _scalajs_compiler_support_labels(full_version):
    """Additional JARs needed on the compiler classpath for Scala.js."""
    # Scala 3 has built-in scalajs support, no extra compiler JARs needed.
    # Scala 2 gets the scalajs-compiler plugin via the `plugins` attribute.
    return []

def scala_multiplatform_test(
        name,
        srcs_root = "src/test",
        base_dir = "",
        lib_deps = [],
        platforms = None,
        scala_versions = None,
        extra_scalac_opts = [],
        remove_scalac_opts = [],
        visibility = None,
        tags = []):
    """Generate test targets for all platform x version combinations.

    Compiles test sources into a scala_library, then creates platform-specific
    test rules: JVM uses ScalaTest Runner, JS uses Node.js (after linking).
    Native tests are disabled (matching SBT behavior).

    Args:
        name: Base test name. Generated: {name}_{platform}_{version}.
        srcs_root: Test source root, e.g. "src/test".
        lib_deps: Multiplatform library deps (the code under test).
        platforms: Platforms to test. Defaults to ALL_PLATFORMS.
        scala_versions: Dict of platform -> [full_version]. Defaults to PLATFORM_SCALA_VERSIONS.
        extra_scalac_opts: Additional scalac options.
        remove_scalac_opts: Scalac options to remove.
        visibility: Bazel visibility.
        tags: Bazel tags.
    """
    if platforms == None:
        platforms = ALL_PLATFORMS
    if scala_versions == None:
        scala_versions = PLATFORM_SCALA_VERSIONS

    for platform in platforms:
        versions = scala_versions.get(platform, [])
        for full_version in versions:
            sv = scala_short_version(full_version)
            compiled_name = name + "_compiled_" + platform + "_" + sv
            test_name = name + "_" + platform + "_" + sv

            # Resolve library deps
            resolved_lib_deps = [_resolve_multiplatform_dep(d, platform, full_version) for d in lib_deps]

            # Add scalatest
            st_label = scalatest_label(full_version, platform)
            resolved_lib_deps.append(st_label)

            # Auto-add provided deps (same as scala_multiplatform_library)
            if scala_major(full_version) == 2:
                resolved_lib_deps.append(maven_label("org.scala-lang:scala-reflect:" + full_version, full_version))
            else:
                resolved_lib_deps.append(maven_label("org.scala-lang:scala3-compiler_3:" + full_version, full_version))

            # Scoverage runtime needed on test classpath (instrumented library macros reference it)
            scov_test_deps = []
            if platform == "jvm":
                sr = scoverage_runtime_label(full_version)
                if sr:
                    scov_test_deps = [sr]

            # Source files
            srcs = native.glob(versioned_srcs(srcs_root, full_version, platform, base_dir))

            # Compiler classpath
            compiler_cp = scala_compiler_labels(full_version)
            lib_cp = scala_library_labels(full_version)

            # Plugins
            plugins = []
            if scala_major(full_version) == 2:
                kp = kind_projector_label(full_version)
                if kp:
                    plugins.append(kp)

            # Scalac opts
            scalac_opts = version_scalac_opts(full_version)
            scalac_opts = scalac_opts + extra_scalac_opts
            if remove_scalac_opts:
                scalac_opts = [o for o in scalac_opts if o not in remove_scalac_opts]

            # Platform-specific adjustments
            if platform == "js":
                js_plugin = scalajs_compiler_plugin_label(full_version)
                if js_plugin:
                    plugins.append(js_plugin)
                else:
                    scalac_opts = scalac_opts + ["-scalajs"]
                lib_cp = lib_cp + scalajs_library_labels(full_version)
            elif platform == "native":
                native_plugin = scalanative_compiler_plugin_label(full_version)
                if native_plugin:
                    plugins.append(native_plugin)
                lib_cp = lib_cp + scalanative_library_labels(full_version)

            # Step 1: Compile test sources
            test_deps = resolved_lib_deps + select({
                "//:scoverage": scov_test_deps,
                "//conditions:default": [],
            }) if scov_test_deps else resolved_lib_deps

            scala_library(
                name = compiled_name,
                srcs = srcs,
                deps = test_deps,
                scala_version = full_version,
                platform = platform,
                scalac_opts = scalac_opts,
                compiler_classpath = compiler_cp,
                scala_library_classpath = lib_cp,
                plugins = plugins,
                tags = tags,
            )

            # Step 2: Create test rule
            test_tags = tags + [platform, "scala_" + sv]
            if platform == "jvm":
                scala_jvm_test(
                    name = test_name,
                    compiled_tests = ":" + compiled_name,
                    runtime_deps = [st_label],
                    visibility = visibility,
                    tags = test_tags,
                )
            elif platform == "js":
                runner_plugins = []
                runner_opts = []
                js_plugin = scalajs_compiler_plugin_label(full_version)
                if js_plugin:
                    runner_plugins.append(js_plugin)
                else:
                    runner_opts.append("-scalajs")

                scala_linked_test(
                    name = test_name,
                    compiled_tests = ":" + compiled_name,
                    platform = "js",
                    scala_version = full_version,
                    compiler_classpath = compiler_cp,
                    scala_library_classpath = lib_cp,
                    plugins = runner_plugins,
                    linker_classpath = [SCALAJS_LINKER_LABEL],
                    runner_scalac_opts = runner_opts,
                    visibility = visibility,
                    tags = test_tags,
                )
            elif platform == "native":
                # Runner needs the native compiler plugin
                runner_plugins = []
                np = scalanative_compiler_plugin_label(full_version)
                if np:
                    runner_plugins.append(np)

                scala_linked_test(
                    name = test_name,
                    compiled_tests = ":" + compiled_name,
                    platform = "native",
                    scala_version = full_version,
                    compiler_classpath = compiler_cp,
                    scala_library_classpath = lib_cp,
                    plugins = runner_plugins,
                    linker_classpath = [SCALANATIVE_LINKER_LABEL],
                    visibility = visibility,
                    tags = test_tags,
                )
