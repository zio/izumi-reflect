"Version constants, compiler configurations, and per-version scalac flags."

# ── Version constants ─────────────────────────────────────────────────
# Keep in sync with MODULE.bazel top-level constants.

# @@VERSIONS_START@@
PROJECT_VERSION = "3.0.10-SNAPSHOT"

SCALA_211 = "2.11.12"
SCALA_212 = "2.12.20"
SCALA_213 = "2.13.14"
SCALA_3 = "3.3.6"

SCALAJS_VERSION = "1.17.0"
SCALANATIVE_VERSION = "0.5.7"

SCALATEST_VERSION = "3.2.19"
KIND_PROJECTOR_VERSION = "0.13.3"
# @@VERSIONS_END@@

# All versions by platform
SCALA_JVM_VERSIONS = [SCALA_211, SCALA_212, SCALA_213, SCALA_3]
SCALA_JS_VERSIONS = [SCALA_212, SCALA_213, SCALA_3]
SCALA_NATIVE_VERSIONS = [SCALA_212, SCALA_213, SCALA_3]

ALL_PLATFORMS = ["jvm", "js", "native"]

PLATFORM_SCALA_VERSIONS = {
    "jvm": SCALA_JVM_VERSIONS,
    "js": SCALA_JS_VERSIONS,
    "native": SCALA_NATIVE_VERSIONS,
}

# Short version keys used in target names (e.g. "izumi-reflect_jvm_2.13")
def scala_short_version(full_version):
    """Convert '2.13.14' -> '2.13', '3.3.6' -> '3'."""
    parts = full_version.split(".")
    if parts[0] == "3":
        return "3"
    return parts[0] + "." + parts[1]

def scala_major(full_version):
    """Return major version: 2 or 3."""
    return int(full_version.split(".")[0])

def scala_binary_version(full_version):
    """Return binary version for Maven artifact suffixes: '2.11', '2.12', '2.13', '3'."""
    parts = full_version.split(".")
    if parts[0] == "3":
        return "3"
    return parts[0] + "." + parts[1]

# Compiler main classes
def scalac_main_class(full_version):
    if scala_major(full_version) == 3:
        return "dotty.tools.dotc.Main"
    return "scala.tools.nsc.Main"

# Maven coordinates for compiler classpath
def scala_compiler_artifacts(full_version):
    """Return list of Maven artifact coordinates for the Scala compiler."""
    bv = scala_binary_version(full_version)
    if scala_major(full_version) == 3:
        return [
            "org.scala-lang:scala3-compiler_3:" + full_version,
            "org.scala-lang:scala3-library_3:" + full_version,
            "org.scala-lang:scala-library:2.13.14",
        ]
    return [
        "org.scala-lang:scala-compiler:" + full_version,
        "org.scala-lang:scala-library:" + full_version,
        "org.scala-lang:scala-reflect:" + full_version,
    ]

def scala_library_artifacts(full_version):
    """Return list of Maven artifact coordinates for Scala runtime library (compile classpath)."""
    if scala_major(full_version) == 3:
        return [
            "org.scala-lang:scala3-library_3:" + full_version,
            "org.scala-lang:scala-library:2.13.14",
        ]
    return [
        "org.scala-lang:scala-library:" + full_version,
    ]

# ScalaJS artifacts
def scalajs_compiler_plugin_artifact(full_version):
    """Return the scalajs compiler plugin Maven coordinate."""
    if scala_major(full_version) == 3:
        # Scala 3 has scalajs support built into the compiler; needs scalajs-scalalib
        return None
    return "org.scala-js:scalajs-compiler_" + full_version + ":" + SCALAJS_VERSION

def scalajs_library_artifacts(full_version):
    """Return scalajs library artifacts for the compile classpath.

    scalajs-library is published for Scala 2.x binary versions only.
    Scala 3 uses the _2.13 artifact (TASTy compatible).
    """
    bv = scala_binary_version(full_version)
    if bv == "3":
        bv = "2.13"
    return [
        "org.scala-js:scalajs-library_" + bv + ":" + SCALAJS_VERSION,
    ]

# Scala Native artifacts
def scalanative_compiler_plugin_artifact(full_version):
    """Return the scala-native compiler plugin Maven coordinate.

    nscplugin uses the full Scala version in the artifact name for all versions.
    """
    return "org.scala-native:nscplugin_" + full_version + ":" + SCALANATIVE_VERSION

def scalanative_library_artifacts(full_version):
    """Return scala-native library artifacts for the compile classpath."""
    bv = scala_binary_version(full_version)
    native_bv = "native0.5"
    return [
        "org.scala-native:nativelib_" + native_bv + "_" + bv + ":" + SCALANATIVE_VERSION,
        "org.scala-native:javalib_" + native_bv + "_" + bv + ":" + SCALANATIVE_VERSION,
        "org.scala-native:auxlib_" + native_bv + "_" + bv + ":" + SCALANATIVE_VERSION,
    ]

# Base scalac options (all versions)
BASE_SCALAC_OPTS = [
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-deprecation",
    "-language:higherKinds",
]

# Per-version scalac options
# Backend parallelism placeholder — resolved to $(nproc)-1 at execution time.
_BACKEND_PARALLELISM = "@@NPROC@@"

SCALAC_OPTS_211 = []

SCALAC_OPTS_212 = [
    "-release:8",
    "-explaintypes",
    "-Ypartial-unification",
    "-Wconf:any:warning",
    "-Wconf:cat=optimizer:warning",
    "-Wconf:cat=other-match-analysis:error",
    "-Ybackend-parallelism", _BACKEND_PARALLELISM,
    "-Xlint:adapted-args",
    "-Xlint:by-name-right-associative",
    "-Xlint:constant",
    "-Xlint:delayedinit-select",
    "-Xlint:doc-detached",
    "-Xlint:inaccessible",
    "-Xlint:infer-any",
    "-Xlint:missing-interpolator",
    "-Xlint:nullary-override",
    "-Xlint:nullary-unit",
    "-Xlint:option-implicit",
    "-Xlint:package-object-classes",
    "-Xlint:poly-implicit-overload",
    "-Xlint:private-shadow",
    "-Xlint:stars-align",
    "-Xlint:type-parameter-shadow",
    "-Xlint:unsound-match",
    "-opt-warnings:_",
    "-Ywarn-extra-implicit",
    "-Ywarn-unused:_",
    "-Ywarn-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-inaccessible",
    "-Ywarn-infer-any",
    "-Ywarn-nullary-override",
    "-Ywarn-nullary-unit",
    "-Ywarn-numeric-widen",
    "-Ywarn-unused-import",
    "-Ywarn-value-discard",
    "-Ycache-plugin-class-loader:always",
    "-Ycache-macro-class-loader:last-modified",
    "-Wconf:msg=nowarn:silent",
]

SCALAC_OPTS_213 = [
    "-release:8",
    "-explaintypes",
    "-Wconf:any:warning",
    "-Wconf:cat=optimizer:warning",
    "-Wconf:cat=other-match-analysis:error",
    "-Vtype-diffs",
    "-Ybackend-parallelism", _BACKEND_PARALLELISM,
    "-Wdead-code",
    "-Wextra-implicit",
    "-Wnumeric-widen",
    "-Woctal-literal",
    "-Wvalue-discard",
    "-Wunused:_",
    "-Wmacros:default",
    "-Ycache-plugin-class-loader:always",
    "-Ycache-macro-class-loader:last-modified",
    "-Wconf:msg=nowarn:silent",
    "-Xlint:-implicit-recursion",
]

SCALAC_OPTS_3 = [
    "-Ykind-projector",
    "-no-indent",
    "-language:implicitConversions",
]

# Release-mode optimizations (non-snapshot builds)
SCALAC_RELEASE_OPTS_212 = [
    "-opt:l:inline",
    "-opt-inline-from:izumi.reflect.**",
]

SCALAC_RELEASE_OPTS_213 = [
    "-opt:l:inline",
    "-opt-inline-from:izumi.reflect.**",
]

# CI mode: the SBT build adds -Wconf:any:error then immediately removes it (line 189/428).
# Net effect: warnings are never promoted to errors. Keeping the config_setting for future use
# but the upgrade lists are empty — CI mode currently only serves as a marker.
SCALAC_CI_UPGRADE_212 = []
SCALAC_CI_UPGRADE_213 = []
SCALAC_CI_DOWNGRADE = []

# Options removed from Compile scope for boopickle-shaded module
BOOPICKLE_REMOVED_OPTS = [
    "-Ywarn-value-discard",
    "-Ywarn-unused:_",
    "-Wvalue-discard",
    "-Wunused:_",
]

def version_scalac_opts(full_version, release_mode = False):
    """Return the full list of scalac options for a given Scala version."""
    opts = list(BASE_SCALAC_OPTS)
    if full_version == SCALA_211:
        opts.extend(SCALAC_OPTS_211)
    elif full_version == SCALA_212:
        opts.extend(SCALAC_OPTS_212)
        if release_mode:
            opts.extend(SCALAC_RELEASE_OPTS_212)
    elif full_version == SCALA_213:
        opts.extend(SCALAC_OPTS_213)
        if release_mode:
            opts.extend(SCALAC_RELEASE_OPTS_213)
    elif full_version == SCALA_3:
        opts.extend(SCALAC_OPTS_3)
    return opts

# SBT also removes -Wconf:any:error globally (line 189), so we use -Wconf:any:warning above

# kind-projector plugin (Scala 2 only)
def kind_projector_artifact(full_version):
    """Return kind-projector compiler plugin Maven coordinate, or None for Scala 3."""
    if scala_major(full_version) == 3:
        return None
    return "org.typelevel:kind-projector_" + full_version + ":" + KIND_PROJECTOR_VERSION

# Per-version Maven repository names (each Scala version gets its own repo
# because core scala-lang artifacts conflict across versions).
def _scala_repo(full_version):
    """Return the Bazel repository name for the given Scala version."""
    if full_version == SCALA_211:
        return "scala_2_11"
    elif full_version == SCALA_212:
        return "scala_2_12"
    elif full_version == SCALA_213:
        return "scala_2_13"
    elif full_version == SCALA_3:
        return "scala_3"
    fail("Unknown Scala version: " + full_version)

def _artifact_label(repo, coord):
    """Convert Maven coordinate to a Bazel label in the given repo.

    Uses rules_jvm_external naming: dots and hyphens → underscores, version stripped.
    E.g. _artifact_label("scala_2_13", "org.scala-lang:scala-compiler:2.13.14")
         → "@scala_2_13//:org_scala_lang_scala_compiler"
    """
    parts = coord.split(":")
    group = parts[0].replace(".", "_").replace("-", "_")
    artifact = parts[1].replace(".", "_").replace("-", "_")
    return "@" + repo + "//:" + group + "_" + artifact

def maven_label(coord, full_version):
    """Convert Maven coordinate to Bazel label using the version-appropriate repo."""
    return _artifact_label(_scala_repo(full_version), coord)

def scala_compiler_labels(full_version):
    repo = _scala_repo(full_version)
    return [_artifact_label(repo, a) for a in scala_compiler_artifacts(full_version)]

def scala_library_labels(full_version):
    repo = _scala_repo(full_version)
    return [_artifact_label(repo, a) for a in scala_library_artifacts(full_version)]

def scalajs_library_labels(full_version):
    repo = _scala_repo(full_version)
    return [_artifact_label(repo, a) for a in scalajs_library_artifacts(full_version)]

def scalanative_library_labels(full_version):
    repo = _scala_repo(full_version)
    return [_artifact_label(repo, a) for a in scalanative_library_artifacts(full_version)]

def kind_projector_label(full_version):
    """Return the kind-projector Bazel label, or None for Scala 3."""
    coord = kind_projector_artifact(full_version)
    if coord == None:
        return None
    return _artifact_label(_scala_repo(full_version), coord)

def scalajs_compiler_plugin_label(full_version):
    """Return the scalajs compiler plugin Bazel label, or None for Scala 3."""
    coord = scalajs_compiler_plugin_artifact(full_version)
    if coord == None:
        return None
    return _artifact_label(_scala_repo(full_version), coord)

def scalanative_compiler_plugin_label(full_version):
    """Return the scala-native compiler plugin Bazel label."""
    coord = scalanative_compiler_plugin_artifact(full_version)
    return _artifact_label(_scala_repo(full_version), coord)

def scalatest_label(full_version, platform = "jvm"):
    """Return the scalatest Bazel label for the given Scala version and platform."""
    bv = scala_binary_version(full_version)
    if platform == "js":
        return _artifact_label(_scala_repo(full_version), "org.scalatest:scalatest_sjs1_" + bv + ":" + SCALATEST_VERSION)
    elif platform == "native":
        return _artifact_label(_scala_repo(full_version), "org.scalatest:scalatest_native0.5_" + bv + ":" + SCALATEST_VERSION)
    return _artifact_label(_scala_repo(full_version), "org.scalatest:scalatest_" + bv + ":" + SCALATEST_VERSION)

# Linker tool labels (from the version-independent scala_tools repo)
SCALAJS_LINKER_LABEL = "@scala_tools//:org_scala_js_scalajs_cli_2_13"
SCALANATIVE_LINKER_LABEL = "@scala_tools//:org_scala_native_scala_native_cli_2_13"
JACOCO_AGENT_LABEL = "@scala_tools//:org_jacoco_org_jacoco_agent"

# Scoverage artifacts per Scala version
_SCOVERAGE_VERSIONS = {
    SCALA_211: "1.4.9",
    SCALA_212: "2.5.2",
    SCALA_213: "2.3.0",
}

def scoverage_plugin_label(full_version):
    """Return scoverage compiler plugin label, or None for Scala 3 (uses built-in -coverage-out)."""
    if scala_major(full_version) == 3:
        return None
    sv = _SCOVERAGE_VERSIONS.get(full_version)
    if not sv:
        return None
    return _artifact_label(_scala_repo(full_version), "org.scoverage:scalac-scoverage-plugin_" + full_version + ":" + sv)

def scoverage_runtime_label(full_version):
    """Return scoverage runtime label, or None for Scala 3."""
    if scala_major(full_version) == 3:
        return None
    bv = scala_binary_version(full_version)
    sv = _SCOVERAGE_VERSIONS.get(full_version)
    if not sv:
        return None
    return _artifact_label(_scala_repo(full_version), "org.scoverage:scalac-scoverage-runtime_" + bv + ":" + sv)
