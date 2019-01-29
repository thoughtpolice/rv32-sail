{ pkgs, ocamlPackages
}:

let
  ocamlDeps = with ocamlPackages; [ menhir linenoise omd zarith num ocaml ocamlbuild findlib ];

  # "Lem semantic definition language". Needed by Sail.
  lem = ocamlPackages.buildOcaml rec {
    name = "lem";
    version = "2018-12-14";

    src = pkgs.fetchurl {
      url = "https://github.com/rems-project/${name}/archive/${version}.tar.gz";
      sha256 = "08ng0086lrhv4gqgpwqqx46nz0i4fj548b0lqsf2syw3w670rlk9";
    };

    setupHook = pkgs.writeText "lemlib-hook.sh" ''
      export LEMLIB=@out@/share/lem/library
    '';

    buildInputs = ocamlDeps;
    installPhase = ''
      mkdir -p $out/lib/ocaml/${ocamlPackages.ocaml.version}/site-lib/${name}/
      make install INSTALL_DIR=$out
    '';
  };

  # Linker/ELF semantic libraries.
  linksem = ocamlPackages.buildOcaml rec {
    name = "linksem";
    version = "2018-12-14";

    src = pkgs.fetchFromGitHub {
      owner  = "rems-project";
      repo   = "${name}";
      rev    = "2d0c140352ccb2955b586f3473c1fe2542580281";
      sha256 = "1sh119jc4ib77cn5mry81gnr4sg91ni8jh05h2596k8iksq56pa0";
    };

    buildInputs = [ lem ] ++ ocamlDeps;
    installFlags = [ "INSTALL_DIR=$(out)" ];
  };

  # Sail architectural definition language
  sail = ocamlPackages.buildOcaml rec {
    name = "sail";
    version = with builtins; "0.8pre_${substring 0 7 src.rev}";

    src = pkgs.fetchFromGitHub {
      owner  = "rems-project";
      repo   = "${name}";
      rev    = "60164a9a221ed6566f1067100dbea2ec828b47d2";
      sha256 = "0k0j7ds09ajf6j4af3y7nm5hmhz8067a2h27cfkkl2qjjwldhzc3";
    };

    # SAIL_DIR is used by some associated CPU models to find the share directory
    # containing the standard library and extra code. The alternative is to use
    # opam, but we don't do that here (to make it easier to build models.) In
    # the future we might. For now, export SAIL_DIR for any dependent
    # expressions so they can find things.
    setupHook = pkgs.writeText "lemlib-hook.sh" ''
      export SAIL_DIR=@out@/share
    '';

    buildInputs  = [ pkgs.ott lem linksem ] ++ ocamlDeps;
    buildPhase   = "make sail isail";
    installFlags = [ "INSTALL_DIR=$(out)" "SHARE_DIR=$(out)/share" ];

    # The OCaml backend requires copying some files out of SAIL_DIR, but because
    # these live in the Nix store, they have restrictive permissions that
    # prevent naive copying, resulting in EPERM errors. Instead, replace the
    # 'cp' clauses with 'cp -rf' so they're removed instead, making the output
    # less chatty.
    patchPhase = ''
      substituteInPlace ./src/ocaml_backend.ml \
        --replace 'cp -r' 'cp -rf'
    '';

    # Most of the Sail models allow SAIL_DIR to be set to a location
    # containing a sail installation, but it expects the layout in the source
    # code repo, not the installed binary layout. But this is easy to fix:
    # in particular, it expects the sail binary to be next to the ./share
    # directory pointed to by SAIL_DIR. Fake it with a symlink to make things
    # easy and avoid the need for opam for model builds.
    postInstall = ''
      ln -sfv $out/bin/sail $out/share/sail
    '';
  };

in { inherit lem linksem sail ocamlDeps; }
