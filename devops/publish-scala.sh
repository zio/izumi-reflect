#!/usr/bin/env bash

set -e
set -x

source ./devops/.env.sh
source ./devops/.validate-publishing.sh || exit 0

[[ -z "$SONATYPE_USERNAME" ]] && echo "Missing SONATYPE_USERNAME" && exit 1
[[ -z "$SONATYPE_PASSWORD" ]] && echo "Missing SONATYPE_PASSWORD" && exit 1


if [[ "$CI_BRANCH_TAG" =~ ^v.*$ ]] ; then
  sbt -batch -no-colors -v \
    --java-home "$JAVA_HOME" \
    "show credentials" \
    "+clean" \
    "+test:compile" \
    "+publishSigned" \
    "sonatypeBundleRelease"
else
  sbt -batch -no-colors -v \
    --java-home "$JAVA_HOME" \
    "show credentials" \
    "+clean" \
    "+test:compile" \
    "+publishSigned"
fi
