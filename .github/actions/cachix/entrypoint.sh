#! /usr/bin/env sh

# Entrypoint that runs Cachix Push

[ "$DEBUG" = "1" ] && set -x
set -e

[ "$CACHIX_SIGNING_KEY" = "" ] && \
  echo "CACHIX_SIGNING_KEY must be configured!" && exit 1
[ "$CACHIX_CACHE_NAME" = "" ] && \
  echo "CACHIX_CACHE_NAME must be configured!" && exit 1

exec cachix push "$CACHIX_CACHE_NAME" "$@"
