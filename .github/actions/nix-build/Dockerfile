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
  nixpkgs.gnugrep nixpkgs.git && \
  mkdir -p /etc/nix && \
  (echo "binary-caches = https://cache.nixos.org/ https://aseipp.cachix.org/" | tee -a /etc/nix/nix.conf) && \
  (echo "trusted-public-keys = aseipp.cachix.org-1:UaUH2DczaM7ytMZlyuHtpYq8FAziIQnjmGMaB45rvRw= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" | tee -a /etc/nix/nix.conf)

COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT [ "/entrypoint.sh" ]
CMD [ "--help" ]
