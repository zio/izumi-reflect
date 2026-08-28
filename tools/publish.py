#!/usr/bin/env python3
"""Maven Central publishing tool with GPG signing and atomic upload.

Supports two upload modes:
  - central: Sonatype Central Portal — atomic bundle upload
  - nexus:   Classic OSSRH Nexus — staging repo workflow

Usage:
  bazel build //:maven-artifacts
  python3 tools/publish.py [OPTIONS]

Environment:
  SONATYPE_USERNAME   Sonatype username
  SONATYPE_PASSWORD   Sonatype password/token
"""

import argparse
import hashlib
import json
import os
import subprocess
import sys
import tempfile
import urllib.request
import urllib.error
import zipfile
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class Artifact:
    group: str
    artifact: str
    version: str
    jar: Path
    pom: Path
    sources: Path
    javadoc: Path

    @property
    def coordinate(self) -> str:
        return f"{self.group}:{self.artifact}:{self.version}"

    @property
    def base_path(self) -> str:
        """Maven repository relative path prefix."""
        return f"{self.group.replace('.', '/')}/{self.artifact}/{self.version}/{self.artifact}-{self.version}"


def discover_artifacts(manifests_dir: Path) -> list[Artifact]:
    """Read .manifest files and return Artifact descriptors."""
    manifests = sorted(manifests_dir.glob("*.manifest"))
    if not manifests:
        raise SystemExit(f"No .manifest files found in {manifests_dir}\nRun 'bazel build //:maven-artifacts' first")

    artifacts = []
    for mf in manifests:
        props = {}
        for line in mf.read_text().strip().splitlines():
            k, _, v = line.partition("=")
            props[k] = v

        artifacts.append(Artifact(
            group=props["group"],
            artifact=props["artifact"],
            version=props["version"],
            jar=manifests_dir / props["jar"],
            pom=manifests_dir / props["pom"],
            sources=manifests_dir / props["sources"],
            javadoc=manifests_dir / props["javadoc"],
        ))
    return artifacts


def checksum(path: Path, algo: str) -> str:
    h = hashlib.new(algo)
    h.update(path.read_bytes())
    return h.hexdigest()


def gpg_sign(path: Path, key_id: str | None = None) -> Path:
    """Sign a file with GPG, return the .asc path."""
    asc_path = path.with_suffix(path.suffix + ".asc")
    cmd = ["gpg", "--batch", "--yes", "--armor", "--detach-sign"]
    if key_id:
        cmd += ["--local-user", key_id]
    cmd.append(str(path))
    result = subprocess.run(cmd, capture_output=True, text=True)
    if result.returncode != 0:
        raise RuntimeError(f"GPG signing failed for {path.name}: {result.stderr.strip()}")
    return asc_path


def build_staging_dir(artifacts: list[Artifact], sign: bool, gpg_key: str | None) -> Path:
    """Create a staging directory with Maven repository layout, checksums, and signatures."""
    staging = Path(tempfile.mkdtemp(prefix="maven-publish-"))

    for art in artifacts:
        base = staging / art.base_path.rsplit("/", 1)[0]
        base.mkdir(parents=True, exist_ok=True)
        prefix = staging / art.base_path

        files_to_deploy: list[tuple[Path, str]] = [
            (art.jar, ".jar"),
            (art.pom, ".pom"),
            (art.sources, "-sources.jar"),
            (art.javadoc, "-javadoc.jar"),
        ]

        deployed: list[Path] = []
        for src, suffix in files_to_deploy:
            dst = Path(str(prefix) + suffix)
            dst.write_bytes(src.read_bytes())
            deployed.append(dst)

            # Checksums
            for algo, ext in [("md5", ".md5"), ("sha1", ".sha1"), ("sha256", ".sha256")]:
                Path(str(dst) + ext).write_text(checksum(dst, algo))

        if sign:
            for dst in deployed:
                gpg_sign(dst, gpg_key)

    return staging


def create_bundle(staging: Path) -> Path:
    """Create a ZIP bundle from the staging directory."""
    bundle_path = staging / "bundle.zip"
    with zipfile.ZipFile(bundle_path, "w", zipfile.ZIP_DEFLATED) as zf:
        for path in sorted(staging.rglob("*")):
            if path == bundle_path or path.is_dir():
                continue
            zf.write(path, path.relative_to(staging))
    return bundle_path


def _http_request(url: str, data: bytes | None, headers: dict, method: str = "POST") -> tuple[int, str]:
    """Perform an HTTP request and return (status_code, body)."""
    req = urllib.request.Request(url, data=data, headers=headers, method=method)
    try:
        with urllib.request.urlopen(req) as resp:
            return resp.status, resp.read().decode()
    except urllib.error.HTTPError as e:
        return e.code, e.read().decode()


def _basic_auth_header(username: str, password: str) -> str:
    import base64
    return "Basic " + base64.b64encode(f"{username}:{password}".encode()).decode()


def upload_central(bundle_path: Path, repo_url: str, username: str, password: str, snapshot: bool) -> None:
    """Upload bundle to Sonatype Central Portal."""
    publishing_type = "AUTOMATIC" if snapshot else "USER_MANAGED"
    url = f"{repo_url}/api/v1/publisher/upload?publishingType={publishing_type}"

    # Multipart form upload
    boundary = "----BazelPublishBoundary"
    body = bytearray()
    body += f"--{boundary}\r\n".encode()
    body += f'Content-Disposition: form-data; name="bundle"; filename="bundle.zip"\r\n'.encode()
    body += b"Content-Type: application/octet-stream\r\n\r\n"
    body += bundle_path.read_bytes()
    body += f"\r\n--{boundary}--\r\n".encode()

    headers = {
        "Content-Type": f"multipart/form-data; boundary={boundary}",
        "Authorization": _basic_auth_header(username, password),
    }

    print(f"Uploading bundle ({bundle_path.stat().st_size // 1024}KB) to Central Portal...")
    status, body_text = _http_request(url, bytes(body), headers)

    if 200 <= status < 300:
        print(f"Upload successful (HTTP {status})")
        if body_text:
            deployment_id = body_text.strip()
            print(f"Deployment ID: {deployment_id}")
            if not snapshot:
                print(f"\nBundle staged. To release:")
                print(f"  curl -u $SONATYPE_USERNAME:$SONATYPE_PASSWORD \\")
                print(f"    -X POST '{repo_url}/api/v1/publisher/deployment/{deployment_id}'")
                print(f"\nCheck status:")
                print(f"  curl -u $SONATYPE_USERNAME:$SONATYPE_PASSWORD \\")
                print(f"    '{repo_url}/api/v1/publisher/status?id={deployment_id}'")
    else:
        raise RuntimeError(f"Upload failed (HTTP {status}): {body_text}")


def upload_nexus(staging: Path, repo_url: str, username: str, password: str, snapshot: bool) -> None:
    """Upload to classic OSSRH Nexus with staging workflow."""
    auth = _basic_auth_header(username, password)
    api = f"{repo_url}/service/local"

    if snapshot:
        print("Uploading snapshots to Nexus...")
        for path in sorted(staging.rglob("*")):
            if path.is_dir() or path.name == "bundle.zip":
                continue
            rel = path.relative_to(staging)
            url = f"{repo_url}/content/repositories/snapshots/{rel}"
            headers = {"Authorization": auth}
            status, _ = _http_request(url, path.read_bytes(), headers, method="PUT")
            if status >= 400:
                raise RuntimeError(f"Failed to upload {rel} (HTTP {status})")
        print("Snapshot upload complete")
        return

    # Create staging repo
    print("Creating Nexus staging repository...")
    headers = {"Authorization": auth, "Content-Type": "application/json", "Accept": "application/json"}
    status, profiles_body = _http_request(f"{api}/staging/profiles", None, headers, method="GET")
    if status != 200:
        raise RuntimeError(f"Failed to list staging profiles (HTTP {status})")

    profiles = json.loads(profiles_body)
    profile_id = profiles["data"][0]["id"] if profiles.get("data") else None
    if not profile_id:
        raise RuntimeError("No staging profile found")

    payload = json.dumps({"data": {"description": "izumi-reflect release"}}).encode()
    status, start_body = _http_request(f"{api}/staging/profiles/{profile_id}/start", payload, headers)
    if status >= 400:
        raise RuntimeError(f"Failed to create staging repo (HTTP {status}): {start_body}")

    repo_id = json.loads(start_body)["data"]["stagedRepositoryId"]
    print(f"Staging repository: {repo_id}")

    # Upload artifacts
    print("Uploading artifacts...")
    file_count = 0
    for path in sorted(staging.rglob("*")):
        if path.is_dir() or path.name == "bundle.zip":
            continue
        rel = path.relative_to(staging)
        url = f"{api}/staging/deployByRepositoryId/{repo_id}/{rel}"
        upload_headers = {"Authorization": auth}
        status, _ = _http_request(url, path.read_bytes(), upload_headers, method="PUT")
        if status >= 400:
            raise RuntimeError(f"Failed to upload {rel} (HTTP {status})")
        file_count += 1
    print(f"Uploaded {file_count} files")

    # Close staging repo
    print("Closing staging repository...")
    payload = json.dumps({"data": {"stagedRepositoryId": repo_id, "description": "Close"}}).encode()
    status, _ = _http_request(f"{api}/staging/profiles/{profile_id}/finish", payload, headers)
    if status >= 400:
        raise RuntimeError(f"Failed to close staging repo (HTTP {status})")

    print(f"\nStaging repository {repo_id} closed.")
    print(f"To release:")
    print(f"  curl -u $SONATYPE_USERNAME:$SONATYPE_PASSWORD \\")
    print(f"    -H 'Content-Type: application/json' \\")
    print(f"    -d '{{\"data\":{{\"stagedRepositoryIds\":[\"{repo_id}\"],\"description\":\"Release\"}}}}' \\")
    print(f"    '{api}/staging/bulk/promote'")


def main():
    parser = argparse.ArgumentParser(description="Publish Maven artifacts to Sonatype")
    parser.add_argument("--mode", choices=["central", "nexus"], default="central",
                        help="Upload mode (default: central)")
    parser.add_argument("--repo-url", default=None,
                        help="Repository URL (default: auto per mode)")
    parser.add_argument("--snapshot", action="store_true",
                        help="Snapshot upload (no signing, no staging)")
    parser.add_argument("--gpg-key", default=None,
                        help="GPG key ID for signing")
    parser.add_argument("--username", default=os.environ.get("SONATYPE_USERNAME"),
                        help="Sonatype username (or SONATYPE_USERNAME env)")
    parser.add_argument("--password", default=os.environ.get("SONATYPE_PASSWORD"),
                        help="Sonatype password (or SONATYPE_PASSWORD env)")
    parser.add_argument("--manifests-dir", type=Path, default=Path("bazel-bin"),
                        help="Directory containing .manifest files")
    parser.add_argument("--dry-run", action="store_true",
                        help="Build bundle but don't upload")
    args = parser.parse_args()

    if args.repo_url is None:
        args.repo_url = "https://central.sonatype.com" if args.mode == "central" else "https://oss.sonatype.org"

    # Discover
    artifacts = discover_artifacts(args.manifests_dir)
    print(f"Found {len(artifacts)} artifacts:")
    for art in artifacts:
        print(f"  {art.coordinate}")

    # Build staging directory
    sign = not args.snapshot
    if sign and args.dry_run:
        # Check if GPG key is available; skip signing gracefully on dry-run
        result = subprocess.run(["gpg", "--list-secret-keys"], capture_output=True)
        if result.returncode != 0 or not result.stdout.strip():
            print("Warning: no GPG key available, dry-run will skip signing")
            sign = False
    staging = build_staging_dir(artifacts, sign=sign, gpg_key=args.gpg_key)

    file_count = sum(1 for _ in staging.rglob("*") if _.is_file())
    print(f"Staging directory: {file_count} files")

    # Create bundle
    bundle = create_bundle(staging)
    print(f"Bundle: {bundle.stat().st_size // 1024}KB")

    if args.dry_run:
        print(f"\nDry run — bundle preserved at: {bundle}")
        print("Contents:")
        with zipfile.ZipFile(bundle) as zf:
            for info in zf.infolist():
                if not info.is_dir():
                    print(f"  {info.file_size:>8}  {info.filename}")
        return

    # Upload
    if not args.username or not args.password:
        raise SystemExit(
            "Credentials required. Set SONATYPE_USERNAME and SONATYPE_PASSWORD,\n"
            "or pass --username and --password"
        )

    if args.mode == "central":
        upload_central(bundle, args.repo_url, args.username, args.password, args.snapshot)
    else:
        upload_nexus(staging, args.repo_url, args.username, args.password, args.snapshot)

    # Cleanup
    import shutil
    shutil.rmtree(staging)
    print("Done.")


if __name__ == "__main__":
    main()
