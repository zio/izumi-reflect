#!/usr/bin/env bash
set -xeuo pipefail

export CI_BUILD_UNIQ_SUFFIX="${CI_BUILD_UNIQ_SUFFIX:-$(date +%s)}"
export IZREFLECT_VERSION=$(cat version.sbt | sed -r 's/.*\"(.*)\".**/\1/' | sed -E "s/SNAPSHOT/build."${CI_BUILD_UNIQ_SUFFIX}"/")

export SCALA211=$(cat project/Deps.sc | grep 'val scala211 ' |  sed -r 's/.*\"(.*)\".**/\1/')
export SCALA212=$(cat project/Deps.sc | grep 'val scala212 ' |  sed -r 's/.*\"(.*)\".**/\1/')
export SCALA213=$(cat project/Deps.sc | grep 'val scala213 ' |  sed -r 's/.*\"(.*)\".**/\1/')
export SCALA3=$(cat project/Deps.sc | grep 'val scala300 ' |  sed -r 's/.*\"(.*)\".**/\1/')

[[ -z "$SCALA_VERSION" ]] && echo "Missing SCALA_VERSION" && exit 1

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

_JAVA_OPTIONS="$(echo "${_JAVA_OPTIONS}" | grep -v '#' | tr '\n' ' ' | tr -s ' ')"


env
