{
  # Ocaml package set to use, alternatively you
  # could ask for e.g. ocamlPackages_4_07
  ocamlPackageSet ? "ocamlPackages"

  # Similarly, set the GHC package set, e.g. "ghc844"
, ghc ? "ghc863"

  # Set the RISC-V subarchitecture for the GCC toolchain
, riscv-arch ? "rv32imc"

  # Nixpkgs import path, can be set to e.g. '<nixpkgs>'
  # The default value, null, uses a pre-determined snapshot of the nixpkgs
  # repository.
, nixpkgs ? null

  # Nixpkgs config, in case you want to override something.
, config ? {}
}:

let
  pkgs = import ./nix/bootstrap.nix { inherit nixpkgs config; };
in

# Bring the chosen package set into scope
with pkgs;

let

  # ------------------------------------
  # OCaml setup, and common dependencies

  ocamlPackages = pkgs."${ocamlPackageSet}";
  ocamlDeps = with ocamlPackages; [ menhir linenoise omd zarith num ocaml ocamlbuild findlib ];
  
  # ------------------------------------
  # Haskell inputs, for the build system

  haskellInputs = pkgs.haskell.packages."${ghc}".ghcWithPackages (p: with p;
    [ shake ]);

  # Write out a stupid wrapper for 'runghc' which sets the include path to
  # include the ./mk directory. This is to make the build system nicer, because
  # nix-shell does not support passing these arguments directly. This is used
  # only by build.hs
  runghcWrapper = pkgs.writeShellScriptBin "runghc2" ''
    exec runghc -isrc/mk $@
  '';

  # -----------------------------
  # Jobs built in this expression.
  # This is mostly to improve clarity, i.e. see where 'lem' comes from.

  jobs = {
    # RISC-V toolchain
    riscv-toolchain = import ./nix/riscv-gcc.nix { inherit pkgs riscv-arch; };

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
        mkdir -p $out/lib/ocaml/${ocaml.version}/site-lib/${name}/
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

      buildInputs = [ jobs.lem ] ++ ocamlDeps;
      installFlags = [ "INSTALL_DIR=$(out)" ];
    };

    # Sail architectural definition language
    sail = ocamlPackages.buildOcaml rec {
      name = "sail";
      version = with builtins; "0.7pre_${substring 0 7 src.rev}";

      src = pkgs.fetchFromGitHub {
        owner  = "rems-project";
        repo   = "${name}";
        rev    = "a5e2b3b7411f630b6d2337402330e13862b5666a";
        sha256 = "1l67dadxr09yiphvj4y2a060rmh64bpnq74r1g82qi0skhbgg9vf";
      };

      # SAIL_DIR is used by some associated CPU models to find the share
      # directory containing the standard library and extra code. The
      # alternative is to use opam, but we don't do that here (to make
      # it easier to build models.) In the future we might. For now,
      # export SAIL_DIR for any dependent expressions so they can find
      # things.
      setupHook = pkgs.writeText "lemlib-hook.sh" ''
        export SAIL_DIR=@out@/share
      '';

      buildInputs  = [ pkgs.ott jobs.lem jobs.linksem ] ++ ocamlDeps;
      buildPhase   = "make sail isail";
      installFlags = [ "INSTALL_DIR=$(out)" "SHARE_DIR=$(out)/share" ];

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

  }; /* jobs */

  # These are all the packages that will be available inside the nix-shell
  # environment.
  buildInputs =
    (with jobs; [ lem linksem sail riscv-toolchain ]) ++
    (with pkgs; [ gcc ott z3 zlib wget dtc python3 ]) ++
    [ runghcWrapper haskellInputs ] ++
    (ocamlDeps)
    ;

  # Serialize the list of buildInputs as a big string containing all the paths.
  depPaths = lib.concatStringsSep " " buildInputs;

# Export a usable shell environment. This also includes a little hack that
# allows you to run 'nix-build shell.nix' in order to get a store path that
# has the list of buildInputs as strict dependencies, so you can copy the
# resulting closure around to other machines or places.
in pkgs.runCommand "sail-shell" { inherit buildInputs; } ''
  mkdir -p $out/nix-support/
  touch $out/nix-support/propagated-build-inputs

  for x in ${depPaths}; do
    echo "$x" >> $out/nix-support/propagated-build-inputs
  done
''
