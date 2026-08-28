#!/usr/bin/env python3
"""Sync version constants from versions.json into MODULE.bazel and rules/versions.bzl.

Single source of truth: versions.json
Generated blocks: marked with '# @@VERSIONS_START@@' / '# @@VERSIONS_END@@' comments.

Usage:
  python3 tools/sync_versions.py          # update files in-place
  python3 tools/sync_versions.py --check  # verify files are in sync (for CI)
"""

import json
import sys
from pathlib import Path

ROOT = Path(__file__).parent.parent
VERSIONS_JSON = ROOT / "versions.json"
MODULE_BAZEL = ROOT / "MODULE.bazel"
VERSIONS_BZL = ROOT / "rules" / "versions.bzl"

START_MARKER = "# @@VERSIONS_START@@"
END_MARKER = "# @@VERSIONS_END@@"


def load_versions() -> dict:
    return json.loads(VERSIONS_JSON.read_text())


def generate_module_bazel_block(v: dict) -> str:
    s = v["scala"]
    return "\n".join([
        f'_SCALA_211 = "{s["2.11"]}"',
        f'_SCALA_212 = "{s["2.12"]}"',
        f'_SCALA_213 = "{s["2.13"]}"',
        f'_SCALA_3 = "{s["3"]}"',
        "",
        f'_SCALAJS = "{v["scalajs"]}"',
        f'_SCALANATIVE = "{v["scalanative"]}"',
        f'_SCALATEST = "{v["scalatest"]}"',
        f'_KIND_PROJECTOR = "{v["kind_projector"]}"',
        "",
        f'_SCOVERAGE_211 = "{v["scoverage"]["2.11"]}"',
        f'_SCOVERAGE_212 = "{v["scoverage"]["2.12"]}"',
        f'_SCOVERAGE_213 = "{v["scoverage"]["2.13"]}"',
        "",
        f'_JACOCO = "{v["jacoco"]}"',
        f'_MIMA_CLI = "{v["mima_cli"]}"',
    ])


def generate_versions_bzl_block(v: dict) -> str:
    s = v["scala"]
    return "\n".join([
        f'PROJECT_VERSION = "{v["project"]}"',
        "",
        f'SCALA_211 = "{s["2.11"]}"',
        f'SCALA_212 = "{s["2.12"]}"',
        f'SCALA_213 = "{s["2.13"]}"',
        f'SCALA_3 = "{s["3"]}"',
        "",
        f'SCALAJS_VERSION = "{v["scalajs"]}"',
        f'SCALANATIVE_VERSION = "{v["scalanative"]}"',
        "",
        f'SCALATEST_VERSION = "{v["scalatest"]}"',
        f'KIND_PROJECTOR_VERSION = "{v["kind_projector"]}"',
    ])


def replace_block(content: str, new_block: str) -> str:
    start = content.find(START_MARKER)
    end = content.find(END_MARKER)
    if start == -1 or end == -1:
        raise ValueError(f"Missing {START_MARKER} / {END_MARKER} markers")
    return content[:start] + START_MARKER + "\n" + new_block + "\n" + content[end:]


def sync(check_only: bool = False) -> bool:
    v = load_versions()
    ok = True

    for path, generator in [
        (MODULE_BAZEL, generate_module_bazel_block),
        (VERSIONS_BZL, generate_versions_bzl_block),
    ]:
        old = path.read_text()
        new_block = generator(v)
        new = replace_block(old, new_block)
        if old != new:
            if check_only:
                print(f"OUT OF SYNC: {path.relative_to(ROOT)}")
                ok = False
            else:
                path.write_text(new)
                print(f"Updated: {path.relative_to(ROOT)}")
        else:
            print(f"Up to date: {path.relative_to(ROOT)}")

    return ok


def main():
    check = "--check" in sys.argv
    if not sync(check_only=check):
        raise SystemExit(1)


if __name__ == "__main__":
    main()
