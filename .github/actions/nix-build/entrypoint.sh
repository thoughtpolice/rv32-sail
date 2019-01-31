#! /usr/bin/env sh

# Entrypoint that runs nix-build and, optionally, copies Docker image tarballs
# to real files. The reason this is necessary is because once a Nix container
# exits, you must copy out the artifacts to the working directory before exit.

[ "$DEBUG" = "1" ] && set -x
[ "$QUIET" = "1" ] && QUIET_ARG="-Q"

set -e

# file to build (e.g. release.nix)
file="$1"
shift

# single target to build (e.g. 'emulator' or 'docker')
target="$1"
shift

# output file: only used when docker is the target
outfile="$1"

# docker outputs must have a file to copy to
if [ "$target" = "docker" ]; then
  [ "$outfile" = "" ] && \
    echo "docker images must have an extra argument, the filename to copy the .tar.gz to!" && \
    exit 1
fi

path=$(nix-build --no-link ${QUIET_ARG} "$file" -A "$target")

[ "$target" = "docker" ] && \
  cp -L "$path" "$outfile"
