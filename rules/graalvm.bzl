"GraalVM JDK toolchain registration for Bazel."

_JDK_BUILD = """\
load("@rules_java//java:defs.bzl", "java_runtime")
package(default_visibility = ["//visibility:public"])
filegroup(name = "jre", srcs = glob(["jre/bin/**", "jre/lib/**"], allow_empty = True))
filegroup(name = "jdk-bin", srcs = glob(["bin/**"], exclude = ["**/*%*/**"]))
filegroup(name = "jdk-conf", srcs = glob(["conf/**"], allow_empty = True))
filegroup(name = "jdk-include", srcs = glob(["include/**"], allow_empty = True))
filegroup(name = "jdk-lib", srcs = glob(["lib/**", "release"], allow_empty = True, exclude = ["lib/missioncontrol/**"]))
java_runtime(
    name = "jdk",
    srcs = [":jdk-bin", ":jdk-conf", ":jdk-include", ":jdk-lib", ":jre"],
    java = glob(["bin/java.exe", "bin/java"], allow_empty = True)[0],
    version = {version},
)
"""

_TOOLCHAIN_BUILD = """\
toolchain(
    name = "toolchain",
    toolchain = "@{jdk_repo}//:jdk",
    toolchain_type = "@bazel_tools//tools/jdk:runtime_toolchain_type",
    target_settings = [":version_setting"],
)
toolchain(
    name = "bootstrap_toolchain",
    toolchain = "@{jdk_repo}//:jdk",
    toolchain_type = "@rules_java//toolchains:bootstrap_runtime_toolchain_type",
    target_settings = [":version_setting"],
)
config_setting(
    name = "version_setting",
    values = {{"java_runtime_version": "{version_label}"}},
)
"""

def _graalvm_jdk_impl(repository_ctx):
    repository_ctx.download_and_extract(
        url = repository_ctx.attr.url,
        sha256 = repository_ctx.attr.sha256,
        stripPrefix = repository_ctx.attr.strip_prefix,
    )
    repository_ctx.file("BUILD.bazel", _JDK_BUILD.format(version = repository_ctx.attr.java_version))
    repository_ctx.file("WORKSPACE", "")

_graalvm_jdk = repository_rule(
    implementation = _graalvm_jdk_impl,
    attrs = {
        "url": attr.string(mandatory = True),
        "sha256": attr.string(mandatory = True),
        "strip_prefix": attr.string(mandatory = True),
        "java_version": attr.int(default = 21),
    },
)

def _graalvm_toolchain_impl(repository_ctx):
    repository_ctx.file("BUILD.bazel", _TOOLCHAIN_BUILD.format(
        jdk_repo = repository_ctx.attr.jdk_repo,
        version_label = repository_ctx.attr.version_label,
    ))
    repository_ctx.file("WORKSPACE", "")

_graalvm_toolchain = repository_rule(
    implementation = _graalvm_toolchain_impl,
    attrs = {
        "jdk_repo": attr.string(mandatory = True),
        "version_label": attr.string(mandatory = True),
    },
)

# Only register for the host platform to avoid downloading all variants.
_GRAALVM_PLATFORMS = {
    "linux-amd64": struct(
        url = "https://github.com/graalvm/graalvm-ce-builds/releases/download/jdk-21.0.2/graalvm-community-jdk-21.0.2_linux-x64_bin.tar.gz",
        sha256 = "b048069aaa3a99b84f5b957b162cc181a32a4330cbc35402766363c5be76ae48",
        strip_prefix = "graalvm-community-openjdk-21.0.2+13.1",
    ),
    "linux-aarch64": struct(
        url = "https://github.com/graalvm/graalvm-ce-builds/releases/download/jdk-21.0.2/graalvm-community-jdk-21.0.2_linux-aarch64_bin.tar.gz",
        sha256 = "",  # fill in when needed
        strip_prefix = "graalvm-community-openjdk-21.0.2+13.1",
    ),
    "mac os x-amd64": struct(
        url = "https://github.com/graalvm/graalvm-ce-builds/releases/download/jdk-21.0.2/graalvm-community-jdk-21.0.2_macos-x64_bin.tar.gz",
        sha256 = "",  # fill in when needed
        strip_prefix = "graalvm-community-openjdk-21.0.2+13.1/Contents/Home",
    ),
    "mac os x-aarch64": struct(
        url = "https://github.com/graalvm/graalvm-ce-builds/releases/download/jdk-21.0.2/graalvm-community-jdk-21.0.2_macos-aarch64_bin.tar.gz",
        sha256 = "",  # fill in when needed
        strip_prefix = "graalvm-community-openjdk-21.0.2+13.1/Contents/Home",
    ),
}

def _graalvm_ext_impl(module_ctx):
    os_name = module_ctx.os.name
    os_arch = module_ctx.os.arch
    key = os_name + "-" + os_arch
    config = _GRAALVM_PLATFORMS.get(key)
    if not config:
        # Unsupported platform — create dummy repos so use_repo doesn't fail
        return
    if not config.sha256:
        return

    _graalvm_jdk(
        name = "graalvm_21",
        url = config.url,
        sha256 = config.sha256,
        strip_prefix = config.strip_prefix,
    )
    _graalvm_toolchain(
        name = "graalvm_21_toolchain",
        jdk_repo = "graalvm_21",
        version_label = "graalvm_21",
    )

graalvm = module_extension(_graalvm_ext_impl)
