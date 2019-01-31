#! /usr/bin/env bash

# Entrypoint that runs Skopeo Copy

[ "$DEBUG" = "1" ] && set -x
set -e

# sanitize env
[ "$SKOPEO_DEST_USER" = "" ] && \
  echo "SKOPEO_DEST_USER must be configured!" && exit 1
[ "$SKOPEO_DEST_PASS" = "" ] && \
  echo "SKOPEO_DEST_PASS must be configured!" && exit 1
[ "$GITHUB_SHA" = "" ] && \
  echo "GITHUB_SHA must be configured!" && exit 1

# flags, only archive and registry are required. all remaining arguments
# are passed as flags, below
archive="$1"
registry="$2"

[ "$archive" = "" ]  && echo "No archive file to copy specified!" && exit 1
[ "$registry" = "" ] && echo "No destination registry specified!" && exit 1

[ ! -e "$archive" ] && echo "File $archive does not exist!" && exit 1
[ ! -f "$archive" ] && echo "$archive must be a file!" && exit 1

# get required tags
nixtag=$(nix eval --raw -f release.nix rv32-version.version)
shatag=$(echo "$GITHUB_SHA" | cut -c 1-10)
alltags=("$nixtag" "$shatag")

if [ ! "$GITHUB_REF" = "" ]; then
  reftag="${GITHUB_REF##*/}"
  alltags+=("$reftag")
  echo using GITHUB_REF tag "$reftag"
fi

# parse arguments
latest=1
for x in "$@"; do
  case "$x" in
    --no-latest)
        echo "not tagging image with 'latest'"
        latest=0
        ;;
    *)
        ;;
  esac
done

# add 'latest' tag unless told not to
[ "$latest" = "1" ] && alltags+=("latest")

# do the business
echo using "$(skopeo --version)"
echo using tags: "$(for x in "${alltags[@]}"; do echo -n "$x "; done)"

for t in "${alltags[@]}"; do
  echo skopeo copy --dcreds "$SKOPEO_DEST_USER:$SKOPEO_DEST_PASS" "docker-archive:$archive" "${registry}:${t}"
done
