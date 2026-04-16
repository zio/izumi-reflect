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
    """Extract prerelease tag: '3.0.10-SNAPSHOT' -> 'snapshot', '1.0.0-beta.1' -> 'beta'."""
    match = re.match(r'^\d+\.\d+\.\d+-([a-zA-Z]+)', version)
    return match.group(1).lower() if match else None


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


def get_version_from_sbt() -> str:
    """Read version from version.sbt."""
    version_sbt = PROJECT_ROOT / "version.sbt"
    if not version_sbt.exists():
        raise SystemExit("version.sbt not found")
    content = version_sbt.read_text()
    match = re.search(r'"([^"]+)"', content)
    if not match:
        raise SystemExit("Could not parse version from version.sbt")
    return match.group(1)


def main():
    parser = argparse.ArgumentParser(description="Publish documentation to NPM")
    parser.add_argument("--version", default=None, help="Package version")
    parser.add_argument("--dry-run", action="store_true", help="Don't actually publish")
    parser.add_argument("--registry", default=None, help="NPM registry URL")
    args = parser.parse_args()

    version = args.version or get_version_from_sbt()
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

    # Configure npm auth (writes to ~/.npmrc, matching zio-sbt-website behavior)
    token = os.environ.get("NODE_AUTH_TOKEN")
    if not token:
        raise SystemExit("NODE_AUTH_TOKEN environment variable is required for publishing")
    registry = args.registry or "https://registry.npmjs.org/"
    registry_host = registry.removeprefix("https://").removesuffix("/")
    npmrc = Path.home() / ".npmrc"
    npmrc.write_text(f"//{registry_host}/:_authToken={token}\n")

    # Replicate zio-sbt-website publishToNpm sequence exactly:
    # 1. Set version in package.json (without git tag)
    _npm(["npm", "version", version, "--no-git-tag-version"], cwd=DOCS_DIR)

    # 2. Set repository URL for npm provenance verification
    _npm(["npm", "pkg", "set", f"repository.url=https://github.com/zio/izumi-reflect"], cwd=DOCS_DIR)

    # 3. Configure public access
    _npm(["npm", "config", "set", "access", "public"])

    # 4. Publish with prerelease tag if applicable
    prerelease_tag = _extract_prerelease_tag(version)
    cmd = ["npm", "publish"]
    if prerelease_tag:
        cmd += ["--tag", prerelease_tag]
    _npm(cmd, cwd=DOCS_DIR)

    print(f"Published {pkg['name']}@{version}")


if __name__ == "__main__":
    main()
