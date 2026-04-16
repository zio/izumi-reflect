"Maven artifact generation for publishing."

load("//rules:providers.bzl", "ScalaInfo")
load("//rules:versions.bzl", "SCALAJS_VERSION", "SCALANATIVE_VERSION", "scala_binary_version", "scala_major")

_GROUP_ID = "dev.zio"
_URL = "https://zio.dev"
_SCM_URL = "https://github.com/zio/izumi-reflect"
_SCM_CONNECTION = "scm:git:https://github.com/zio/izumi-reflect.git"
_LICENSE_NAME = "Apache-2.0"
_LICENSE_URL = "https://www.apache.org/licenses/LICENSE-2.0"
_DEVELOPERS = [
    ("jdegoes", "John De Goes"),
    ("7mind", "Septimal Mind"),
]

def _maven_artifact_id(name, full_version, platform):
    bv = scala_binary_version(full_version)
    if platform == "js":
        return name + "_sjs1_" + bv
    elif platform == "native":
        return name + "_native0.5_" + bv
    return name + "_" + bv

def _dep_xml(group, artifact, version, scope = ""):
    scope_line = ""
    if scope:
        scope_line = "      <scope>" + scope + "</scope>"
    return (
        "    <dependency>\n" +
        "      <groupId>" + group + "</groupId>\n" +
        "      <artifactId>" + artifact + "</artifactId>\n" +
        "      <version>" + version + "</version>\n" +
        scope_line +
        "    </dependency>"
    )

def _maven_artifact_impl(ctx):
    sv = ctx.attr.scala_version
    platform = ctx.attr.platform
    bv = scala_binary_version(sv)
    artifact_id = _maven_artifact_id(ctx.attr.module_name, sv, platform)
    version = ctx.attr.version

    compiled_jar = ctx.attr.compiled[ScalaInfo].output_jar
    java_runtime = ctx.toolchains["@bazel_tools//tools/jdk:toolchain_type"].java.java_runtime
    jar_tool = java_runtime.java_home + "/bin/jar"

    # ── Build dependency XML fragments ──
    dep_lines = []
    if scala_major(sv) == 3:
        dep_lines.append(_dep_xml("org.scala-lang", "scala3-library_3", sv))
        dep_lines.append(_dep_xml("org.scala-lang", "scala-library", "2.13.14"))
    else:
        dep_lines.append(_dep_xml("org.scala-lang", "scala-library", sv))

    if platform == "js":
        dep_lines.append(_dep_xml("org.scala-js", "scalajs-library_" + bv, SCALAJS_VERSION))
    elif platform == "native":
        dep_lines.append(_dep_xml("org.scala-native", "nativelib_native0.5_" + bv, SCALANATIVE_VERSION))

    for dep_name in ctx.attr.maven_deps:
        dep_lines.append(_dep_xml(_GROUP_ID, _maven_artifact_id(dep_name, sv, platform), version))

    if scala_major(sv) == 2:
        dep_lines.append(_dep_xml("org.scala-lang", "scala-reflect", sv, "provided"))
    else:
        dep_lines.append(_dep_xml("org.scala-lang", "scala3-compiler_3", sv, "provided"))

    deps_xml = "\n".join(dep_lines)

    dev_lines = []
    for id, name in _DEVELOPERS:
        dev_lines.append('    <developer><id>' + id + '</id><name>' + name + '</name></developer>')
    devs_xml = "\n".join(dev_lines)

    # ── Generate POM directly (no external tools) ──
    pom_file = ctx.actions.declare_file(artifact_id + "-" + version + ".pom")
    pom_lines = [
        '<?xml version="1.0" encoding="UTF-8"?>',
        '<project xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd"',
        '    xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">',
        "  <modelVersion>4.0.0</modelVersion>",
        "  <groupId>" + _GROUP_ID + "</groupId>",
        "  <artifactId>" + artifact_id + "</artifactId>",
        "  <version>" + version + "</version>",
        "  <packaging>jar</packaging>",
        "  <name>" + ctx.attr.module_name + "</name>",
        "  <url>" + _URL + "</url>",
        "  <licenses>",
        "    <license>",
        "      <name>" + _LICENSE_NAME + "</name>",
        "      <url>" + _LICENSE_URL + "</url>",
        "    </license>",
        "  </licenses>",
        "  <developers>",
        devs_xml,
        "  </developers>",
        "  <scm>",
        "    <url>" + _SCM_URL + "</url>",
        "    <connection>" + _SCM_CONNECTION + "</connection>",
        "  </scm>",
        "  <dependencies>",
        deps_xml,
        "  </dependencies>",
        "</project>",
    ]
    ctx.actions.write(pom_file, "\n".join(pom_lines) + "\n")

    # ── Sources JAR ──
    sources_jar = ctx.actions.declare_file(artifact_id + "-" + version + "-sources.jar")
    src_files = ctx.files.srcs
    if src_files:
        ctx.actions.run_shell(
            outputs = [sources_jar],
            inputs = src_files,
            tools = [java_runtime.files],
            command = "{jar} cf {out} {srcs}".format(
                jar = jar_tool, out = sources_jar.path,
                srcs = " ".join([f.path for f in src_files]),
            ),
            mnemonic = "SourcesJar",
        )
    else:
        # Empty JAR — write via ctx.actions.write (no shell tools needed)
        ctx.actions.write(sources_jar, "")

    # ── Copy compiled JAR with Maven naming ──
    output_jar = ctx.actions.declare_file(artifact_id + "-" + version + ".jar")
    ctx.actions.symlink(output = output_jar, target_file = compiled_jar)

    # ── Empty javadoc JAR ──
    javadoc_jar = ctx.actions.declare_file(artifact_id + "-" + version + "-javadoc.jar")
    readme_file = ctx.actions.declare_file(artifact_id + "-javadoc-README")
    ctx.actions.write(readme_file, "No Javadoc\n")
    ctx.actions.run_shell(
        outputs = [javadoc_jar],
        inputs = [readme_file],
        tools = [java_runtime.files],
        command = jar_tool + " cf " + javadoc_jar.path + " " + readme_file.path,
        mnemonic = "JavadocJar",
    )

    # ── Manifest for publish script ──
    manifest = ctx.actions.declare_file(artifact_id + "-" + version + ".manifest")
    ctx.actions.write(manifest, "\n".join([
        "group=" + _GROUP_ID,
        "artifact=" + artifact_id,
        "version=" + version,
        "jar=" + output_jar.short_path,
        "pom=" + pom_file.short_path,
        "sources=" + sources_jar.short_path,
        "javadoc=" + javadoc_jar.short_path,
    ]) + "\n")

    return [DefaultInfo(
        files = depset([output_jar, pom_file, sources_jar, javadoc_jar, manifest]),
        runfiles = ctx.runfiles(files = [output_jar, pom_file, sources_jar, javadoc_jar, manifest]),
    )]

maven_artifact = rule(
    implementation = _maven_artifact_impl,
    attrs = {
        "compiled": attr.label(mandatory = True),
        "srcs": attr.label_list(allow_files = [".scala", ".java"]),
        "module_name": attr.string(mandatory = True),
        "version": attr.string(mandatory = True),
        "scala_version": attr.string(mandatory = True),
        "platform": attr.string(mandatory = True),
        "maven_deps": attr.string_list(default = []),
    },
    toolchains = ["@bazel_tools//tools/jdk:toolchain_type"],
)
