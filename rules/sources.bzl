"Version-specific and platform-specific source directory selection."

load("//rules:versions.bzl", "PLATFORM_SCALA_VERSIONS", "scala_binary_version")

def _version_tuple(short_version):
    """Convert '2.13' -> (2, 13), '3' -> (3, 0), '2.11' -> (2, 11)."""
    parts = short_version.split(".")
    if len(parts) == 1:
        return (int(parts[0]), 0)
    return (int(parts[0]), int(parts[1]))

def _short_version(full_version):
    """'2.13.14' -> '2.13', '3.3.6' -> '3.3'."""
    parts = full_version.split(".")
    return parts[0] + "." + parts[1]

def _all_cross_short_versions(platform):
    """Return sorted list of short versions for a platform, e.g. ['2.12', '2.13', '3.3']."""
    return sorted(
        [_short_version(v) for v in PLATFORM_SCALA_VERSIONS[platform]],
        key = _version_tuple,
    )

def select_source_dirs(root, full_version, platform):
    """Compute source directory list for a given Scala version and platform.

    Replicates the SBT source directory resolution from build.sbt:
    - scala/            (always)
    - scala-{major}/    (scala-2 or scala-3)
    - scala-{M.N}/      (exact version, e.g. scala-2.12)
    - scala-{M.N}+/     (for each cross version <= current)
    - scala-{M.N}-/     (for each cross version >= current)
    - scala-{L}-{R}/    (for contiguous ranges containing current)

    Additionally includes platform-specific dirs from .{platform}/ prefix.

    Args:
        root: source root, e.g. "src/main"
        full_version: e.g. "2.13.14"
        platform: "jvm", "js", or "native"

    Returns:
        List of source directory paths (relative to BUILD file).
    """
    sv = _short_version(full_version)
    current = _version_tuple(sv)
    cross_versions = _all_cross_short_versions(platform)

    dirs = _compute_version_dirs(root + "/scala", sv, current, cross_versions)

    # Platform-specific dirs (e.g. .jvm/src/main/scala, .jvm/src/main/scala-2, etc.)
    platform_root = "." + platform + "/" + root
    dirs.extend(_compute_version_dirs(platform_root + "/scala", sv, current, cross_versions))

    return dirs

def _compute_version_dirs(scala_dir, sv, current, cross_versions):
    """Compute version-qualified directories relative to a scala base dir.

    Args:
        scala_dir: e.g. "src/main/scala" or ".jvm/src/main/scala"
        sv: short version string, e.g. "2.13"
        current: version tuple, e.g. (2, 13)
        cross_versions: sorted list of short version strings

    Returns:
        List of directory paths.
    """
    dirs = []

    # Base dir (always)
    dirs.append(scala_dir)

    # Major version dir: scala-2 or scala-3
    dirs.append(scala_dir + "-" + str(current[0]))

    # Exact version dir: scala-2.13 (only if minor != 0 or explicitly exists)
    dirs.append(scala_dir + "-" + sv)

    # Plus dirs: for each cross version <= current, add scala-{M.N}+
    for v in cross_versions:
        vt = _version_tuple(v)
        if vt[0] < current[0] or (vt[0] == current[0] and vt[1] <= current[1]):
            dirs.append(scala_dir + "-" + v + "+")

    # Minus dirs: for each cross version >= current, add scala-{M.N}-
    for v in cross_versions:
        vt = _version_tuple(v)
        if vt[0] > current[0] or (vt[0] == current[0] and vt[1] >= current[1]):
            dirs.append(scala_dir + "-" + v + "-")

    # Range dirs: for each contiguous subsequence [l..r] where l <= current <= r
    sorted_vs = sorted(cross_versions, key = _version_tuple)
    for window_size in range(2, len(sorted_vs) + 1):
        for start in range(len(sorted_vs) - window_size + 1):
            window = sorted_vs[start:start + window_size]
            l = _version_tuple(window[0])
            r = _version_tuple(window[-1])
            if (l[0] < current[0] or (l[0] == current[0] and l[1] <= current[1])) and \
               (r[0] > current[0] or (r[0] == current[0] and r[1] >= current[1])):
                dirs.append(scala_dir + "-" + window[0] + "-" + window[-1])

    return dirs

def versioned_srcs(root, full_version, platform):
    """Return glob patterns for version/platform-specific Scala sources.

    This is the main entry point for BUILD files. It computes source directories
    and returns a list of glob patterns suitable for use in srcs.

    Args:
        root: source root, e.g. "src/main"
        full_version: e.g. "2.13.14"
        platform: "jvm", "js", or "native"

    Returns:
        List of glob pattern strings like ["src/main/scala/**/*.scala", ...].
    """
    dirs = select_source_dirs(root, full_version, platform)
    return [d + "/**/*.scala" for d in dirs]
