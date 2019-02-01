#! /usr/bin/env sh

# Entrypoint that runs nix-build and, optionally, copies Docker image tarballs
# to real files. The reason this is necessary is because once a Nix container
# exits, you must copy out the artifacts to the working directory before exit.

[ "$DEBUG" = "1" ] && set -x
[ "$QUIET" = "1" ] && QUIET_ARG="-Q"

set -e

# file to build (e.g. release.nix)
file="$1"

[ "$file" = "" ] && echo "No .nix file to build specified!" && exit 1
[ ! -e "$file" ] && echo "File $file not exist!" && exit 1

echo "Building all attrs in $file..."
nix-build --no-link ${QUIET_ARG} "$file"

docker=$(nix-build --no-link ${QUIET_ARG} "$file" -A "docker")
pages=$(nix-build --no-link ${QUIET_ARG} "$file" -A "gh-pages")

echo "Copying Docker Tarball to docker.tar.gz..."
cp -L "$docker" docker.tar.gz

echo "Copying gh-pages tarball to gh-pages.tar.gz..."
cp -L "$pages" gh-pages.tar.gz
