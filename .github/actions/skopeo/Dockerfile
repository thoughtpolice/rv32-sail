FROM lnl7/nix:2.1.2

LABEL name="Skopeo Copy for GitHub Actions"
LABEL version="1.0"
LABEL repository="http://github.com/thoughtpolice/rv32-sail"
LABEL homepage="http://github.com/thoughtpolice/rv32-sail"
LABEL maintainer="Austin Seipp <aseipp@pobox.com>"

LABEL "com.github.actions.name"="Skopeo Copy"
LABEL "com.github.actions.description"="Push a Docker Archive into a Container Registry"
LABEL "com.github.actions.icon"="cloud"
LABEL "com.github.actions.color"="red"

RUN nix-env -iA nixpkgs.gnutar nixpkgs.gzip && \
  nix-env -iA skopeo bash coreutils git -f https://github.com/NixOS/nixpkgs-channels/archive/f2a1a4e93be2d76720a6b96532b5b003cc769312.tar.gz

COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT [ "/entrypoint.sh" ]
CMD [ "--help" ]
