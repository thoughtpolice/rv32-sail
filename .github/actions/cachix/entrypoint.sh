#! /usr/bin/env bash

# Entrypoint that runs Cachix Push

[ "$DEBUG" = "1" ] && set -x
set -e

[ "$CACHIX_SIGNING_KEY" = "" ] && \
  echo "CACHIX_SIGNING_KEY must be configured!" && exit 1
[ "$CACHIX_CACHE_NAME" = "" ] && \
  echo "CACHIX_CACHE_NAME must be configured!" && exit 1
[ "$CACHIX_AUTH_TOKEN" = "" ] && \
  echo "CACHIX_AUTH_TOKEN must be configured!" && exit 1

file=store.roots
store="$(pwd)/store"

[ ! -f "$file"  ] && echo "File $file does not exist!" && exit 1
[ ! -d "$store" ] && echo "Store 'file://$store' does not exist!" && exit 1

mkdir -p "$HOME/.config/cachix"
echo "{ authToken=\"$CACHIX_AUTH_TOKEN\", binaryCaches = [] : List { name : Text, secretKey : Text } }" > "$HOME/.config/cachix/cachix.dhall"

mapfile -t storePaths < "$file"
nix copy --no-check-sigs --from "file://$store" "${storePaths[@]}"
cachix push "$CACHIX_CACHE_NAME" < "$file"
