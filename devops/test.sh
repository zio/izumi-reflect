#!/usr/bin/env bash

set -e
set -x

source ./devops/.env.sh

sbt -batch -no-colors -v \
  --java-home "$JAVA_HOME" \
  "$VERSION_COMMAND clean" \
  "$VERSION_COMMAND Test/compile" \
  "$VERSION_COMMAND test" \
  "$VERSION_COMMAND mimaReportBinaryIssues"
