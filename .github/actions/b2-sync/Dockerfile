FROM lnl7/nix:2.1.2

LABEL name="Backblaze B2 Sync for GitHub Actions"
LABEL version="1.0"
LABEL repository="http://github.com/thoughtpolice/rv32-sail"
LABEL homepage="http://github.com/thoughtpolice/rv32-sail"
LABEL maintainer="Austin Seipp <aseipp@pobox.com>"

LABEL "com.github.actions.name"="Backblaze B2 Sync"
LABEL "com.github.actions.description"="Sync some files to a given B2 Bucket"
LABEL "com.github.actions.icon"="disc"
LABEL "com.github.actions.color"="green"

RUN nix-env -iA nixpkgs.gnutar nixpkgs.gzip && \
  nix-env -iA rclone bash coreutils git -f https://github.com/NixOS/nixpkgs-channels/archive/f2a1a4e93be2d76720a6b96532b5b003cc769312.tar.gz

COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT [ "/entrypoint.sh" ]
CMD [ "--help" ]
