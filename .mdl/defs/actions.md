# Build Actions

## Environment

- `LANG=C.UTF-8`

## passthrough
- `HOME`
- `USER`
- `SCALA_VERSION`
- `JAVA_VERSION`
- `OPENSSL_IV`
- `OPENSSL_KEY`
- `SONATYPE_USERNAME`
- `SONATYPE_PASSWORD`
- `NODE_AUTH_TOKEN`
- `CI_BRANCH_TAG`
- `CI_BUILD_UNIQ_SUFFIX`
- `CI_PULL_REQUEST`
- `CI_BRANCH`


# action: setup-jdk

Setup JDK path based on JAVA_VERSION

```bash
# Get JAVA_VERSION from environment (default to 17 if not set - optional)
JAVA_VERSION_VAL="${JAVA_VERSION:-17}"

# Determine JAVA_HOME based on JDK version from nix flake environment
# These are set by flake.nix shellHook
case "$JAVA_VERSION_VAL" in
  11)
    if [[ -n "${JDK11:-}" ]]; then
      JAVA_HOME="$JDK11"
    else
      echo "Error: JDK11 not set in environment" && exit 1
    fi
    ;;
  17)
    if [[ -n "${JDK17:-}" ]]; then
      JAVA_HOME="$JDK17"
    else
      echo "Error: JDK17 not set in environment" && exit 1
    fi
    ;;
  21)
    if [[ -n "${JDK21:-}" ]]; then
      JAVA_HOME="$JDK21"
    else
      echo "Error: JDK21 not set in environment" && exit 1
    fi
    ;;
  *)
    echo "Unsupported JAVA_VERSION: $JAVA_VERSION_VAL" && exit 1
    ;;
esac

ret java-home:String="$JAVA_HOME"
ret java-bin:String="$JAVA_HOME/bin"
```

# action: setup-jvm-options

Setup JVM options and optimizations

```bash
JAVA_OPTIONS="${_JAVA_OPTIONS:-}"

# Add user.home for nix environments
USER_HOME="${env.HOME}"
JAVA_OPTIONS+=" -Duser.home=$USER_HOME"

# Append any custom tail options (optional)
if [[ -n "${JAVA_OPTIONS_TAIL:-}" ]]; then
  JAVA_OPTIONS+=" $JAVA_OPTIONS_TAIL"
fi

# Add optimizations
JAVA_OPTIONS+=" -Xmx4000M"
JAVA_OPTIONS+=" -XX:ReservedCodeCacheSize=384M"
JAVA_OPTIONS+=" -XX:NonProfiledCodeHeapSize=256M"
JAVA_OPTIONS+=" -XX:MaxMetaspaceSize=1024M"

# Normalize whitespace
JAVA_OPTIONS=$(echo "$JAVA_OPTIONS" | tr '\n' ' ' | tr -s ' ')

ret java-options:String="$JAVA_OPTIONS"
```

# action: setup-scala

Setup Scala version variables

```bash
# Extract Scala versions from Deps.sc
SCALA212=$(grep 'val scala212 ' ${sys.project-root}/project/Deps.sc | sed -r 's/.*"(.*)".*/\1/')
SCALA213=$(grep 'val scala213 ' ${sys.project-root}/project/Deps.sc | sed -r 's/.*"(.*)".*/\1/')
SCALA211=$(grep 'val scala211 ' ${sys.project-root}/project/Deps.sc | sed -r 's/.*"(.*)".*/\1/')
SCALA3=$(grep 'val scala300 ' ${sys.project-root}/project/Deps.sc | sed -r 's/.*"(.*)".*/\1/')

# Get SCALA_VERSION from environment (default to 2.13 if not set - optional)
SCALA_VERSION_SHORT="${SCALA_VERSION:-2.13}"

# Resolve SCALA_VERSION to full version
case "$SCALA_VERSION_SHORT" in
  2.11) SCALA_VERSION_FULL="$SCALA211" ;;
  2.12) SCALA_VERSION_FULL="$SCALA212" ;;
  2.13) SCALA_VERSION_FULL="$SCALA213" ;;
  3) SCALA_VERSION_FULL="$SCALA3" ;;
  *) SCALA_VERSION_FULL="$SCALA_VERSION_SHORT" ;;
esac

# Extract project version (CI_BUILD_UNIQ_SUFFIX is optional)
CI_BUILD_UNIQ_SUFFIX_VAL="${CI_BUILD_UNIQ_SUFFIX:-SNAPSHOT}"
PROJECT_VERSION=$(cat ${sys.project-root}/version.sbt | sed -r 's/.*"(.*)".*/\1/' | sed -E "s/SNAPSHOT/build.${CI_BUILD_UNIQ_SUFFIX_VAL}/")

# Create sbt version command
VERSION_COMMAND="++ $SCALA_VERSION_FULL"

ret scala-version:String="$SCALA_VERSION_FULL"
ret version-command:String="$VERSION_COMMAND"
ret project-version:String="$PROJECT_VERSION"
ret scala212:String="$SCALA212"
ret scala213:String="$SCALA213"
ret scala211:String="$SCALA211"
ret scala3:String="$SCALA3"
```

# action: setup-env

Complete environment setup - combines all setup actions

```bash
# Depend on all setup actions
JAVA_HOME="${action.setup-jdk.java-home}"
JAVA_BIN="${action.setup-jdk.java-bin}"
JAVA_OPTIONS="${action.setup-jvm-options.java-options}"
VERSION_COMMAND="${action.setup-scala.version-command}"
SCALA_VERSION="${action.setup-scala.scala-version}"

export PATH="$JAVA_BIN:$PATH"
export JAVA_HOME="$JAVA_HOME"
export _JAVA_OPTIONS="$JAVA_OPTIONS"
export VERSION_COMMAND="$VERSION_COMMAND"
export SCALA_VERSION="$SCALA_VERSION"

echo "Environment setup complete:"
echo "  JAVA_HOME=$JAVA_HOME"
echo "  SCALA_VERSION=$SCALA_VERSION"
echo "  VERSION_COMMAND=$VERSION_COMMAND"
```

# action: gen

Generate build files using sbtgen

```bash
# Declare dependency on environment setup
dep action.setup-env

bash sbtgen.sc --js --native
```

# action: test

Run tests and binary compatibility checks

```bash
dep action.gen

# Declare dependencies and use their outputs
dep action.setup-env
JAVA_HOME="${action.setup-jdk.java-home}"
VERSION_COMMAND="${action.setup-scala.version-command}"

sbt -batch -no-colors -v \
  --java-home "$JAVA_HOME" \
  "$VERSION_COMMAND clean" \
  "$VERSION_COMMAND Test/compile" \
  "$VERSION_COMMAND test" \
  "$VERSION_COMMAND mimaReportBinaryIssues"
```

# action: publish-scala

Publish Scala artifacts to Sonatype (only on release branches/tags)

## vars
- `CI_PULL_REQUEST`
- `CI_BRANCH`

```bash
# Declare dependencies and use their outputs
dep action.gen

JAVA_HOME="${action.setup-jdk.java-home}"

# Get environment variables from mudyla substitution
SONATYPE_USERNAME_VAL="${env.SONATYPE_USERNAME}"
SONATYPE_PASSWORD_VAL="${env.SONATYPE_PASSWORD}"
CI_PULL_REQUEST_VAL="${env.CI_PULL_REQUEST}"
CI_BRANCH_VAL="${env.CI_BRANCH}"
CI_BRANCH_TAG_VAL="${env.CI_BRANCH_TAG}"

# Apply bash defaults
SONATYPE_USERNAME="${SONATYPE_USERNAME_VAL}"
SONATYPE_PASSWORD="${SONATYPE_PASSWORD_VAL}"
CI_PULL_REQUEST="${CI_PULL_REQUEST_VAL:-false}"
CI_BRANCH="${CI_BRANCH_VAL}"
CI_BRANCH_TAG="${CI_BRANCH_TAG_VAL}"

if [[ -z "$SONATYPE_USERNAME" ]]; then
    echo "Missing SONATYPE_USERNAME, skipping publish"
    exit 0
fi

if [[ -z "$SONATYPE_PASSWORD" ]]; then
    echo "Missing SONATYPE_PASSWORD, skipping publish"
    exit 0
fi

if [[ "$CI_PULL_REQUEST" == "true" ]]; then
    echo "Publishing not allowed on P/Rs"
    exit 0
fi

if [[ "$CI_BRANCH" != "develop" && ! "$CI_BRANCH_TAG" =~ ^v ]]; then
    echo "Publishing not allowed (CI_BRANCH=$CI_BRANCH, CI_BRANCH_TAG=$CI_BRANCH_TAG)"
    exit 0
fi

if [[ "$CI_BRANCH_TAG" =~ ^v.*$ ]]; then
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
```

# action: publish-ziodocs

Publish documentation to NPM

## vars
- `CI_PULL_REQUEST`
- `CI_BRANCH`

```bash
# Declare dependencies and use their outputs
dep action.gen

JAVA_HOME="${action.setup-jdk.java-home}"

# Get environment variables from mudyla substitution
NODE_AUTH_TOKEN_VAL="${env.NODE_AUTH_TOKEN}"
CI_PULL_REQUEST_VAL="${env.CI_PULL_REQUEST}"
CI_BRANCH_VAL="${env.CI_BRANCH}"
CI_BRANCH_TAG_VAL="${env.CI_BRANCH_TAG}"

# Apply bash defaults
NODE_AUTH_TOKEN="${NODE_AUTH_TOKEN_VAL}"
CI_PULL_REQUEST="${CI_PULL_REQUEST_VAL:-false}"
CI_BRANCH="${CI_BRANCH_VAL}"
CI_BRANCH_TAG="${CI_BRANCH_TAG_VAL}"

if [[ -z "$NODE_AUTH_TOKEN" ]]; then
    echo "Missing NODE_AUTH_TOKEN, skipping docs publish"
    exit 0
fi

if [[ "$CI_PULL_REQUEST" == "true" ]]; then
    echo "Publishing not allowed on P/Rs"
    exit 0
fi

if [[ "$CI_BRANCH" != "develop" && ! "$CI_BRANCH_TAG" =~ ^v ]]; then
    echo "Publishing not allowed (CI_BRANCH=$CI_BRANCH, CI_BRANCH_TAG=$CI_BRANCH_TAG)"
    exit 0
fi

# Copy zio-docs.sbt
cp ${sys.project-root}/.mdl/resources/zio-docs.sbt ${sys.project-root}/zio-docs.sbt

# Extract docs section from README and append to docs/index.md
awk '/<!--- docs:start --->/,/<!--- docs:end --->/' ${sys.project-root}/README.md >> ${sys.project-root}/docs/index.md
sed -i '/<!--- docs:start --->/d' ${sys.project-root}/docs/index.md
sed -i '/<!--- docs:end --->/d' ${sys.project-root}/docs/index.md

# Setup npm auth
echo "//registry.npmjs.org/:_authToken=$NODE_AUTH_TOKEN" > ~/.npmrc

# Verify npm authentication
npm whoami

# Publish to npm
sbt -batch -no-colors -v \
    --java-home "$JAVA_HOME" \
    docs/publishToNpm
```

# action: build

Full build pipeline - generate and test

```bash
# Declare dependencies on gen and test
dep action.test

# Both gen and test must succeed
if [[ "${action.gen.success}" == "True" && "${action.test.success}" == "True" ]]; then
  exit 0
else
  exit 1
fi
```
