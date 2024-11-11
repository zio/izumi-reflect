#!/usr/bin/env bash
set -xeuo pipefail

([[ "$CI_PULL_REQUEST" == "true"  ]] && echo "Publishing not allowed on P/Rs" && exit 0 || true)
([[ ! ("$CI_BRANCH" == "develop" || "$CI_BRANCH_TAG" =~ ^v.*$ ) ]] && echo "Publishing not allowed (CI_BRANCH=$CI_BRANCH, CI_BRANCH_TAG=$CI_BRANCH_TAG)" && exit 0) || true
