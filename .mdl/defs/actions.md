# Build Actions

## Environment

```env
LANG=C.UTF-8
```

```env-passthrough
HOME
USER
SCALA_VERSION
JAVA_VERSION
OPENSSL_IV
OPENSSL_KEY
SONATYPE_USERNAME
SONATYPE_PASSWORD
NODE_AUTH_TOKEN
CI_BRANCH_TAG
```

# action: gen

Generate build files using sbtgen

```bash
bash sbtgen.sc --js --native
ret success:Bool=$?
```

# action: test

Run tests and binary compatibility checks

```bash
sbt -batch -no-colors -v \
  --java-home "$JAVA_HOME" \
  "$VERSION_COMMAND clean" \
  "$VERSION_COMMAND Test/compile" \
  "$VERSION_COMMAND test" \
  "$VERSION_COMMAND mimaReportBinaryIssues"
ret success:Bool=$?
```

# action: publish-scala

Publish Scala artifacts to Sonatype (only on release branches/tags)

```bash
set -euo pipefail

if [[ -z "${env.SONATYPE_USERNAME:-}" ]]; then
    echo "Missing SONATYPE_USERNAME, skipping publish"
    ret success:Bool=0
    exit 0
fi

if [[ -z "${env.SONATYPE_PASSWORD:-}" ]]; then
    echo "Missing SONATYPE_PASSWORD, skipping publish"
    ret success:Bool=0
    exit 0
fi

if [[ -z "${env.CI_BRANCH_TAG:-}" ]]; then
    echo "Not on a release branch/tag, skipping publish"
    ret success:Bool=0
    exit 0
fi

if [[ "${env.CI_BRANCH_TAG}" =~ ^v.*$ ]]; then
    # Full release with sonaRelease
    sbt -batch -no-colors -v \
        --java-home "$JAVA_HOME" \
        "show credentials" \
        "+clean" \
        "+test:compile" \
        "+publishSigned" \
        "sonaRelease"
else
    # Snapshot publish without release
    sbt -batch -no-colors -v \
        --java-home "$JAVA_HOME" \
        "show credentials" \
        "+clean" \
        "+test:compile" \
        "+publishSigned"
fi

ret success:Bool=$?
```

# action: publish-ziodocs

Publish documentation to NPM

```bash
set -euo pipefail

if [[ -z "${env.NODE_AUTH_TOKEN:-}" ]]; then
    echo "Missing NODE_AUTH_TOKEN, skipping docs publish"
    ret success:Bool=0
    exit 0
fi

if [[ -z "${env.CI_BRANCH_TAG:-}" ]]; then
    echo "Not on a release branch/tag, skipping docs publish"
    ret success:Bool=0
    exit 0
fi

# Copy zio-docs.sbt
cp ${sys.project-root}/.mdl/resources/zio-docs.sbt ${sys.project-root}/zio-docs.sbt

# Extract docs section from README and append to docs/index.md
awk '/<!--- docs:start --->/,/<!--- docs:end --->/' ${sys.project-root}/README.md >> ${sys.project-root}/docs/index.md
sed -i '/<!--- docs:start --->/d' ${sys.project-root}/docs/index.md
sed -i '/<!--- docs:end --->/d' ${sys.project-root}/docs/index.md

# Setup npm auth
echo "//registry.npmjs.org/:_authToken=${env.NODE_AUTH_TOKEN}" > ~/.npmrc

# Verify npm authentication
npm whoami

# Publish to npm
sbt -batch -no-colors -v \
    --java-home "$JAVA_HOME" \
    docs/publishToNpm

ret success:Bool=$?
```

# action: build

Full build pipeline - generate and test

```bash
# This action depends on gen and test completing successfully
${action.gen.success}
${action.test.success}

ret success:Bool=$?
```
