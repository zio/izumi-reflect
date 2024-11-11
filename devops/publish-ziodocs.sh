#!/usr/bin/env bash

set -e
set -x

source ./devops/.env.sh
source ./devops/.validate-publishing.sh

[[ -z "$NODE_AUTH_TOKEN" ]] && echo "Missing NODE_AUTH_TOKEN" && exit 1

cp ./devops/zio-docs.sbt .

awk '/<!--- docs:start --->/,/<!--- docs:end --->/' README.md >> docs/index.md
sed -i '/<!--- docs:start --->/d' docs/index.md
sed -i '/<!--- docs:end --->/d' docs/index.md

echo "//registry.npmjs.org/:_authToken=${NODE_AUTH_TOKEN}" > ~/.npmrc
npm whoami

sbt -batch -no-colors -v \
    --java-home "$JAVA_HOME" \
    docs/publishToNpm

