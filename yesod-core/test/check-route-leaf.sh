#!/usr/bin/env bash
# Run after building yesod-core. Compile fixtures outside the normal test graph.
set -euo pipefail
repo_dir=$(cd "$(dirname "$0")/../.." && pwd)
cd "$repo_dir"
probe_dir=$(mktemp -d)
trap 'rm -rf "$probe_dir"' EXIT
ghc_args=(-Wall -Werror=incomplete-patterns -package yesod-core -package wai-extra -package http-types -package hspec -iyesod-core/test)
fixture_dir=yesod-core/test/RouteLeafCompile

stack exec -- ghc "${ghc_args[@]}" -outputdir "$probe_dir/isolated" \
    "$fixture_dir/Isolated.hs" -o "$probe_dir/isolated-app" >"$probe_dir/isolated.log" 2>&1 || {
    cat "$probe_dir/isolated.log"
    exit 1
}
"$probe_dir/isolated-app"
echo 'Isolated fragment application: 3 requests passed'

expect_failure() {
    local fixture=$1 expected=$2
    if stack exec -- ghc "${ghc_args[@]}" -fno-code -outputdir "$probe_dir/$fixture" \
        "$fixture_dir/$fixture.hs" >"$probe_dir/$fixture.log" 2>&1; then
        echo "Expected $fixture to fail compilation"
        exit 1
    fi
    if ! rg -q "$expected" "$probe_dir/$fixture.log"; then
        cat "$probe_dir/$fixture.log"
        echo "Unexpected failure for $fixture"
        exit 1
    fi
    echo "Expected compiler rejection: $fixture"
}

expect_failure NestedPattern "Couldn't match expected type.*RouteLeaves"
expect_failure MissingEndpoint 'non-exhaustive'
expect_failure MissingPolicy 'No instance for.*AuthorizeRoute'
