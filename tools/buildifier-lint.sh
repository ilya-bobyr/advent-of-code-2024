#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

buildifer="$(readlink "${buildifier_path:?buildifier_path must be set}")"

cd "${BUILD_WORKSPACE_DIRECTORY:?BUILD_WORKSPACE_DIRECTORY must be set }"

if ! "$buildifer"; then
    # shellcheck disable=SC2016
    echo 'Please run `bazel run //:buildifier` to format the *.bazel files' >&2
    exit 1
fi
