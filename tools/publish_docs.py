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
import re
import subprocess
import sys
from pathlib import Path


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

    # Update package.json version
    pkg = json.loads(PACKAGE_JSON.read_text())
    pkg["version"] = version
    PACKAGE_JSON.write_text(json.dumps(pkg, indent=2) + "\n")
    print(f"Updated docs/package.json to version {version}")

    if args.dry_run:
        print("\nDry run — would publish:")
        print(f"  Package: {pkg['name']}@{version}")
        print(f"  Directory: {DOCS_DIR}")
        print(f"  Files: {', '.join(f.name for f in DOCS_DIR.iterdir() if f.is_file())}")
        return

    # Configure npm auth from NODE_AUTH_TOKEN env var
    token = os.environ.get("NODE_AUTH_TOKEN")
    if not token:
        raise SystemExit("NODE_AUTH_TOKEN environment variable is required for publishing")
    registry = args.registry or "https://registry.npmjs.org/"
    npmrc = DOCS_DIR / ".npmrc"
    npmrc.write_text(f"//{registry.removeprefix('https://').removesuffix('/')}/:_authToken={token}\n")

    # Publish to NPM
    cmd = ["npm", "publish", "--access", "public", "--registry", registry]
    print(f"Running: {' '.join(cmd)}")
    result = subprocess.run(cmd, cwd=DOCS_DIR)
    npmrc.unlink(missing_ok=True)
    if result.returncode != 0:
        raise SystemExit(f"npm publish failed with exit code {result.returncode}")

    print(f"Published {pkg['name']}@{version}")


if __name__ == "__main__":
    main()
