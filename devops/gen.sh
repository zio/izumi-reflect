#!/usr/bin/env bash
set -xeuo pipefail

source ./devops/.env.sh

bash sbtgen.sc --js --native