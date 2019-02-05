FROM lnl7/nix:2.1.2

LABEL name="Cachix Push for GitHub Actions"
LABEL version="1.0"
LABEL repository="http://github.com/thoughtpolice/rv32-sail"
LABEL homepage="http://github.com/thoughtpolice/rv32-sail"
LABEL maintainer="Austin Seipp <aseipp@pobox.com>"

LABEL "com.github.actions.name"="Cachix Push"
LABEL "com.github.actions.description"="Pushes a store into the cache"
LABEL "com.github.actions.icon"="database"
LABEL "com.github.actions.color"="blue"

RUN nix-env -iA nixpkgs.gnutar nixpkgs.gzip && \
  nix-env -iA cachix iana-etc cacert -f https://cachix.org/api/v1/install && \
  ln -sfv $(nix-env -q --out-path --no-name iana-etc)/etc/services /etc/services && \
  ln -sfv $(nix-env -q --out-path --no-name iana-etc)/etc/protocols /etc/protocols && \
  ln -sfv $(nix-env -q --out-path --no-name nss-cacert)/etc/ssl /etc/ssl

COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT [ "/entrypoint.sh" ]
CMD [ "--help" ]
