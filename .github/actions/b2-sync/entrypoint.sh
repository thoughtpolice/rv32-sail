#! /usr/bin/env bash

# Entrypoint that runs Backblze B2

[ "$DEBUG" = "1" ] && set -x
set -e

# sanitize env
export RCLONE_CONFIG_B2_TYPE=b2
[ "$RCLONE_CONFIG_B2_ACCOUNT" = "" ] && \
  echo "RCLONE_CONFIG_B2_ACCOUNT must be configured!" && exit 1
[ "$RCLONE_CONFIG_B2_KEY" = "" ] && \
  echo "RCLONE_CONFIG_B2_KEY must be configured!" && exit 1

# flags, only archive and registry are required. all remaining arguments
# are passed as flags, below
archive="$1"
b2dest="$2"

[ "$archive" = "" ]  && echo "No HTML file to upload has been specified!" && exit 1
[ "$b2dest" = "" ] && echo "No destination registry specified!" && exit 1

[ ! -e "$archive" ] && echo "File $archive does not exist!" && exit 1
[ ! -f "$archive" ] && echo "$archive must be a file!" && exit 1

echo "Extracting $archive..."
tar xf "$archive"
[ ! -d "html" ] && echo "ERROR: archive file '$archive' didn't result in an HTML directory!" && exit 1

# do the business
echo using "$(rclone version | head -1)"
exec rclone sync html/ "B2://$b2dest"
