#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

mode=${ormolu_mode:?ormolu_mode must be set}
ormolu="$(readlink "${ormolu_path:?ormolu_path must be set}")"
extensionsAsList=${ormolu_extensions:-}

IFS=';' read -r -a extensions <<<"$extensionsAsList"
declare -a ghcOpts
for extension in "${extensions[@]}"; do
  ghcOpts+=(--ghc-opt "-X$extension")
done

cd "${BUILD_WORKSPACE_DIRECTORY:?BUILD_WORKSPACE_DIRECTORY must be set}"

if [ "$mode" = format ]; then
  find . -type f -name "*.hs" -exec \
    "$ormolu" "${ghcOpts[@]}" --mode inplace '{}' \+
elif [ "$mode" = lint ]; then
  if ! find . -type f -name "*.hs" -exec \
    "$ormolu" "${ghcOpts[@]}" --mode check '{}' \+; then
      # shellcheck disable=SC2016
      echo 'Please run `bazel run //:ormolu-format` to fix it' >&2
      exit 1
  fi
else
  echo "Unexpected ormolu_mode value: $mode"
  exit 1
fi
