#!/usr/bin/env bash


set -euo pipefail

function run-publish-scala() {
  validate_publishing || exit 0

  [[ -z "$SONATYPE_USERNAME" ]] && echo "Missing SONATYPE_USERNAME" && exit 1
  [[ -z "$SONATYPE_PASSWORD" ]] && echo "Missing SONATYPE_PASSWORD" && exit 1
  
  
  if [[ "$CI_BRANCH_TAG" =~ ^v.*$ ]] ; then
    sbt -batch -no-colors -v \
      --java-home "$JAVA_HOME" \
      "show credentials" \
      "+clean" \
      "+test:compile" \
      "+publishSigned" \
      "sonaUpload" \
      "sonaRelease"
  else
    sbt -batch -no-colors -v \
      --java-home "$JAVA_HOME" \
      "show credentials" \
      "+clean" \
      "+test:compile" \
      "+publishSigned"
  fi
}
