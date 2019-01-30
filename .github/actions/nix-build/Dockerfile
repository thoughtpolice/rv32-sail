FROM lnl7/nix:2.1.2

LABEL name="Nix Build for GitHub Actions"
LABEL version="1.0"
LABEL repository="http://github.com/thoughtpolice/rv32-sail"
LABEL homepage="http://github.com/thoughtpolice/rv32-sail"
LABEL maintainer="Austin Seipp <aseipp@pobox.com>"

LABEL "com.github.actions.name"="Nix Build"
LABEL "com.github.actions.description"="Runs 'nix-build'"
LABEL "com.github.actions.icon"="cpu"
LABEL "com.github.actions.color"="purple"

RUN nix-env -iA \
  nixpkgs.gnutar nixpkgs.gzip \
  nixpkgs.git

ENTRYPOINT [ "nix-build" ]
CMD [ "--help" ]
