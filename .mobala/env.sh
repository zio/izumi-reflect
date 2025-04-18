#!/usr/bin/env bash

set -euo pipefail
if [[ "${DO_VERBOSE}" == 1 ]] ; then set -x ; fi

export NIXIFIED="${NIXIFIED:-0}"
export DO_VERBOSE="${DO_VERBOSE:-0}"

export CI_BUILD_UNIQ_SUFFIX="${CI_BUILD_UNIQ_SUFFIX:-$(date +%s)}"
export IZREFLECT_VERSION=$(cat version.sbt | sed -r 's/.*\"(.*)\".**/\1/' | sed -E "s/SNAPSHOT/build."${CI_BUILD_UNIQ_SUFFIX}"/")

export SCALA211=$(cat project/Deps.sc | grep 'val scala211 ' |  sed -r 's/.*\"(.*)\".**/\1/')
export SCALA212=$(cat project/Deps.sc | grep 'val scala212 ' |  sed -r 's/.*\"(.*)\".**/\1/')
export SCALA213=$(cat project/Deps.sc | grep 'val scala213 ' |  sed -r 's/.*\"(.*)\".**/\1/')
export SCALA3=$(cat project/Deps.sc | grep 'val scala300 ' |  sed -r 's/.*\"(.*)\".**/\1/')

[[ -z "${SCALA_VERSION:-}" ]] && echo "Missing SCALA_VERSION. Define SCALA_VERSION" && exit 1
[[ -z "${JAVA_VERSION:-}" ]] && echo "Missing JAVA_VERSION. Define JAVA_VERSION" && exit 1

case $SCALA_VERSION in
  2.11) SCALA_VERSION="$SCALA211" ;;
  2.12) SCALA_VERSION="$SCALA212" ;;
  2.13) SCALA_VERSION="$SCALA213" ;;
  3) SCALA_VERSION="$SCALA3" ;;
  *) exit 1 ;;
esac

export SCALA_VERSION="$SCALA_VERSION"
export VERSION_COMMAND="++ $SCALA_VERSION"

JDK_VERSION_VAR="JDK${JAVA_VERSION}"
export JAVA_HOME="${!JDK_VERSION_VAR}"
export PATH=$JAVA_HOME/bin:$PATH

export _JAVA_OPTIONS="
  # JVM ignores HOME and relies on getpwuid to determine home directory
  # That fails when we run self-hosted github agent under non-dynamic user
  # We need that for rootless docker to work
  -Duser.home=${HOME}
  -Xmx4000M
  -XX:ReservedCodeCacheSize=384M
  -XX:NonProfiledCodeHeapSize=256M
  -XX:MaxMetaspaceSize=1024M
"

function validate_publishing() {
  # Disallow if this is a pull‑request build
  if [[ "$CI_PULL_REQUEST" == "true" ]]; then
    echo "Publishing not allowed on P/Rs"
    return 1
  fi

  # Disallow if we're neither on develop nor on a tagged release (v*)
  if [[ "$CI_BRANCH" != "develop" && ! "$CI_BRANCH_TAG" =~ ^v ]]; then
    echo "Publishing not allowed (CI_BRANCH=$CI_BRANCH, CI_BRANCH_TAG=$CI_BRANCH_TAG)"
    return 1
  fi

  return 0
}

#------------------------------------------------------------------------------------------
# Tweak JAVA_OPTIONS
export _JAVA_OPTIONS="${_JAVA_OPTIONS:-""}"

# JVM ignores HOME and relies on getpwuid to determine home directory
# That fails when we run self-hosted github agent under non-dynamic user
# We need that for rootless docker to work
if [[ "${NIXIFIED}" == 1 ]] ; then
  _JAVA_OPTIONS+=" -Duser.home=${HOME}"
fi

# Append Java Options tail
#[help]- Set `JAVA_OPTIONS_TAIL` environment variable with additional Java arguments.
_JAVA_OPTIONS+=" ${JAVA_OPTIONS_TAIL:-""}"
# Format Java Options
_JAVA_OPTIONS="$(echo "${_JAVA_OPTIONS}" | grep -v '#' | tr '\n' ' ' | tr -s ' ')"
#------------------------------------------------------------------------------------------

if [[ "${DO_VERBOSE}" == 1 && "${VERBOSE_LEVEL}" -gt 1 ]] ; then
  environment=$(env)
  environment=$(echo "$environment" | grep -v '^\s*$' | sed "s/^/[verbose:env] /;s/$/ /")
  echo "[verbose] Environment set:"
  echo "$environment"
fi

# this script receives all the CLI args from the main script and may decide which flows should be enabled
flow_enable do-build