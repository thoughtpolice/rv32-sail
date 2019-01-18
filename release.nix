{ rv32-sail ? builtins.fetchGit ./.

  # Ocaml package set to use, alternatively you
  # could ask for e.g. ocamlPackages_4_07
, ocamlPackageSet ? "ocamlPackages"

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

, system ? builtins.currentSystem

, official-release ? false
}:

let
  pkgs = import ./nix/bootstrap.nix { inherit nixpkgs config system; };

  versionBase   = pkgs.lib.fileContents ./.version;
  versionSuffix = pkgs.lib.optionalString (!official-release)
    "pre${toString rv32-sail.revCount}_${rv32-sail.shortRev}";

  version = "${versionBase}${versionSuffix}";
in

# Bring the chosen package set into scope
with pkgs;

let

  # ------------------------------------
  # OCaml setup, and common dependencies

  ocamlPackages = pkgs."${ocamlPackageSet}";

  # ------------------------------------
  # Haskell inputs, for the build system

  haskellInputs = pkgs.haskell.packages."${ghc}".ghcWithPackages (p: with p;
    [ shake
      ghc-typelits-extra
      ghc-typelits-natnormalise
      ghc-typelits-knownnat
    ]);

  # Write out a stupid wrapper for 'runghc' which sets the include path to
  # include the ./mk directory. This is to make the build system nicer, because
  # nix-shell does not support passing these arguments directly. This is used
  # only by bake.hs
  runghcWrapper = pkgs.writeShellScriptBin "runghc2" ''
    exec runghc -isrc/mk $@
  '';

  # -----------------------------

  # -----------------------------
  tools = {
    riscv-toolchain = import ./nix/riscv-gcc.nix { inherit pkgs riscv-arch; };

    inherit (import ./nix/sail.nix { inherit pkgs ocamlPackages; })
      lem
      linksem
      sail
      ocamlDeps
    ;
  }; /* tools */

  # These are all the packages that will be available inside the nix-shell
  # environment.
  buildInputs =
    (with tools; [ lem linksem sail riscv-toolchain ]) ++
    (with pkgs; [ gcc ott z3 zlib wget dtc python3 ]) ++
    [ runghcWrapper haskellInputs ] ++
    (tools.ocamlDeps)
    ;

  # Serialize the list of buildInputs as a big string containing all the paths.
  depPaths = lib.concatStringsSep " " buildInputs;

  jobs = rec {
    shell =
      # Export a usable shell environment. This also includes a little hack that
      # allows you to run 'nix-build shell.nix' in order to get a store path that
      # has the list of buildInputs as strict dependencies, so you can copy the
      # resulting closure around to other machines or places.
      pkgs.runCommand "sail-shell" { inherit buildInputs; } ''
        mkdir -p $out/nix-support/
        touch $out/nix-support/propagated-build-inputs

        for x in ${depPaths}; do
          echo "$x" >> $out/nix-support/propagated-build-inputs
        done
      '';

    buildExe = pkgs.stdenv.mkDerivation {
      name = "bake-${version}";
      inherit version;

      src = lib.cleanSource ./.;
      buildInputs = [ haskellInputs ];

      buildPhase = ''
        ghc --make -isrc/mk bake.hs -o bake -threaded -rtsopts "-with-rtsopts=-I0 -qg"
      '';
      installPhase = ''
        mkdir -p $out/bin
        cp ./bake $out/bin
      '';
    };

    /* This only builds the emulator, it doesn't actually build any firmware,
    ** etc.
    */
    emulator = pkgs.stdenv.mkDerivation {
      name = "rv32-sail-${version}";
      inherit version;

      src = lib.cleanSource ./.;
      buildInputs = buildInputs ++ [ buildExe ];

      buildPhase =
        let targets = lib.concatStringsSep " " [ "build/cruise" "build/cruise.opt" ];
        in "bake --no-color --verbose -j$NIX_BUILD_CORES ${targets}";

      installPhase = ''
        install -m0755 -D -t $out/bin ./build/cruise{,.opt}
      '';
    };

    /*
    ** A derivation containing all the C source code generated by Sail,
    ** including the Runtime System code. This only requires a C compiler with
    ** GMP/zlib to compile into a working binary.
    */
    emulator-csrc = pkgs.stdenv.mkDerivation {
      name = "rv32-sail-csrc-${version}";
      inherit version;

      src = lib.cleanSource ./.;
      buildInputs = buildInputs ++ [ buildExe ];

      buildPhase =
        let targets = lib.concatStringsSep " " [ "build/cruise.opt.c" ];
        in "bake --no-color --verbose -j$NIX_BUILD_CORES ${targets}";

      installPhase = ''
        mkdir -p $out
        cp build/*.c $SAIL_DIR/lib/*.c $SAIL_DIR/lib/*.h $out/
      '';
    };

    /*
    ** Derivation that builds a statically linked copy of the C source
    ** code using Musl, in order to distribute binaries for any resulting
    ** users or other systems without Nix.
    **
    ** Currently zlib and gmp upstream don't ship static libraries *yet*
    ** but hopefully that will happen soon.
    */
    emulator-static =
      let ps     = pkgsMusl;
          stdenv = ps.stdenv;
      in stdenv.mkDerivation {
        name = "rv32-sail-static-${version}";
        inherit version;

        src = emulator-csrc;
        buildInputs = [ ps.gmp ps.zlib ];

        buildPhase = "cc -O2 -o cruise *.c -lgmp -lz";
        installPhase = "install -m0755 -D -t $out/bin ./cruise";
      };

  }; /* jobs */

in jobs
