#!/usr/bin/env python3
"""Publish documentation to NPM registry.

Extracts documentation from README.md (between <!--- docs:start ---> and
<!--- docs:end ---> markers), appends it to docs/index.md, bumps the
version in docs/package.json, and publishes via npm publish.

Equivalent to the SBT zio-sbt-website :publish-ziodocs task.

Usage:
  python3 tools/publish_docs.py [OPTIONS]

Options:
  --version=VERSION   Package version to publish (default: from version.sbt)
  --dry-run           Show what would be published without actually publishing
  --registry=URL      NPM registry URL
"""

import argparse
import json
import os
import re
import subprocess
import sys
from pathlib import Path


def _npm(cmd: list[str], cwd: Path | None = None):
    """Run an npm command, raise on failure."""
    print(f"  $ {' '.join(cmd)}")
    result = subprocess.run(cmd, cwd=cwd)
    if result.returncode != 0:
        raise SystemExit(f"Command failed (exit {result.returncode}): {' '.join(cmd)}")


def _extract_prerelease_tag(version: str) -> str | None:
    """Extract prerelease tag from semver: '1.0.0-SNAPSHOT' -> 'snapshot', '1.0.0-beta.1' -> 'beta'.

    Returns None for non-semver versions like date-hash '2026.4.16-be5df6a'.
    """
    # Only match semver: major.minor.patch-prerelease (3 numeric components)
    match = re.match(r'^(\d+)\.(\d+)\.(\d+)-([a-zA-Z]+)', version)
    if not match:
        return None
    # Sanity: major < 1000 to distinguish semver from date-based YYYY.M.D
    if int(match.group(1)) >= 1000:
        return None
    return match.group(4).lower()


PROJECT_ROOT = Path(__file__).parent.parent
DOCS_DIR = PROJECT_ROOT / "docs"
README = PROJECT_ROOT / "README.md"
PACKAGE_JSON = DOCS_DIR / "package.json"
INDEX_MD = DOCS_DIR / "index.md"


def extract_docs_from_readme() -> str:
    """Extract content between <!--- docs:start ---> and <!--- docs:end ---> markers."""
    content = README.read_text()
    sections = []
    in_section = False
    for line in content.splitlines():
        if "docs:start" in line:
            in_section = True
            continue
        if "docs:end" in line:
            in_section = False
            continue
        if in_section:
            sections.append(line)
    return "\n".join(sections)


def get_docs_version() -> str:
    """Generate a docs version matching zio-sbt-website format: YYYY.M.D-<short_commit_hash>."""
    from datetime import date
    today = date.today()
    version_date = f"{today.year}.{today.month}.{today.day}"
    result = subprocess.run(
        ["git", "rev-parse", "--short", "HEAD"],
        capture_output=True, text=True, cwd=PROJECT_ROOT,
    )
    commit_hash = result.stdout.strip() if result.returncode == 0 else "0000000"
    return f"{version_date}-{commit_hash}"


def main():
    parser = argparse.ArgumentParser(description="Publish documentation to NPM")
    parser.add_argument("--version", default=None,
                        help="Package version (default: YYYY.M.D-<commit>, matching zio-sbt-website)")
    parser.add_argument("--dry-run", action="store_true", help="Don't actually publish")
    parser.add_argument("--registry", default=None, help="NPM registry URL")
    args = parser.parse_args()

    version = args.version or get_docs_version()
    print(f"Publishing docs version: {version}")

    # Extract docs from README
    docs_content = extract_docs_from_readme()
    if not docs_content.strip():
        raise SystemExit("No documentation found between docs:start/docs:end markers in README.md")
    print(f"Extracted {len(docs_content.splitlines())} lines from README.md")

    # Read existing index.md frontmatter
    index_content = INDEX_MD.read_text()
    frontmatter_end = index_content.find("---", index_content.find("---") + 1)
    if frontmatter_end == -1:
        raise SystemExit("Could not parse frontmatter in docs/index.md")
    frontmatter = index_content[:frontmatter_end + 3]

    # Write updated index.md
    new_index = frontmatter + "\n\n" + docs_content + "\n"
    INDEX_MD.write_text(new_index)
    print(f"Updated docs/index.md ({len(new_index.splitlines())} lines)")

    pkg = json.loads(PACKAGE_JSON.read_text())

    if args.dry_run:
        print(f"\nDry run — would publish:")
        print(f"  Package: {pkg['name']}@{version}")
        tag = _extract_prerelease_tag(version)
        print(f"  Tag: {tag or 'latest'}")
        print(f"  Directory: {DOCS_DIR}")
        return

    # Configure npm auth — try token if available, otherwise rely on OIDC provenance
    token = os.environ.get("NODE_AUTH_TOKEN")
    if token:
        npmrc_content = "//registry.npmjs.org/:_authToken=" + token + "\n"
        for npmrc_path in [Path.home() / ".npmrc", DOCS_DIR / ".npmrc"]:
            npmrc_path.write_text(npmrc_content)
        print("Configured npm auth via token")
    else:
        print("No NODE_AUTH_TOKEN — will use OIDC provenance (requires id-token: write permission)")

    # 1. Set version in package.json
    _npm(["npm", "version", version, "--no-git-tag-version", "--allow-same-version"], cwd=DOCS_DIR)

    # 2. Set repository URL (required for provenance verification)
    _npm(["npm", "pkg", "set", "repository.type=git"], cwd=DOCS_DIR)
    _npm(["npm", "pkg", "set", "repository.url=https://github.com/zio/izumi-reflect"], cwd=DOCS_DIR)

    # 3. Publish
    cmd = ["npm", "publish", "--access", "public", "--provenance"]
    prerelease_tag = _extract_prerelease_tag(version)
    if prerelease_tag:
        cmd += ["--tag", prerelease_tag]
    _npm(cmd, cwd=DOCS_DIR)

    print(f"Published {pkg['name']}@{version}")


if __name__ == "__main__":
    main()
