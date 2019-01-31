{ rv32-sail ? builtins.fetchGit ./.

  # Ocaml package set to use, alternatively you
  # could ask for e.g. ocamlPackages_4_07
, ocamlPackageSet ? "ocamlPackages"

  # Similarly, set the GHC package set, e.g. "ghc844"
, ghc ? "ghc863"

  # Set the RISC-V subarchitecture for the GCC toolchain
, riscv-arch ? "rv32i"

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
    ]);

  # Write out a stupid wrapper for 'runghc' which sets the include path to
  # include the ./mk directory. This is to make the build system nicer, because
  # nix-shell does not support passing these arguments directly.

  # runghc-bake is only used for the shebang line in bake.hs
  runGhcWrapper = pkgs.writeShellScriptBin "runghc-bake" ''
    ( \
      SRC_DIR=$(${pkgs.git}/bin/git rev-parse --show-toplevel) && \
      exec runghc -i$SRC_DIR/src/mk "$@" \
    )
  '';

  # 'bake' can be used from anywhere, as long as you're inside 'nix-shell'.
  # starts faster, too. TODO FIXME: can we do this root-dir hack without git?
  runBakeWrapper = pkgs.writeShellScriptBin "bake" ''
    ( \
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel) && \
      exec runghc -isrc/mk bake.hs "$@" \
    )
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
    [ haskellInputs ] ++
    (tools.ocamlDeps)
    ;

  # Serialize the list of buildInputs as a big string containing all the paths.
  allDeps = [ runBakeWrapper runGhcWrapper ] ++ buildInputs;


  jobs = rec {
    cache-deps =

      # This is a little hack that allows you to build this target in order to
      # get a store path that has the list of buildInputs as strict
      # dependencies, so you can copy the resulting closure around to other
      # machines or places, e.g.
      #
      #   cat $(nix-build -Q --no-link release.nix -A cache-deps)
      #
      # Static dependencies are only included in the depPath; these won't be
      # included by default when you run nix-shell, but will be when this
      # target is run.
      let staticDeps = with pkgs.pkgsMusl;
            [ stdenv stdenv.bootstrapTools binutils gmp.dev gmp zlib.dev zlib
            ];
          depPaths = lib.concatStringsSep " " (allDeps ++ staticDeps);
      in pkgs.runCommand "cache-deps" {} ''
        touch $out
        for x in ${depPaths}; do
          echo "$x" >> $out
        done
      '';

    shell =
      # Export a usable shell environment. Hack: touch $out so
      # that nix-build works.
      pkgs.runCommand "sail-shell" { buildInputs = allDeps; } ''
        mkdir -p $out/nix-support/
        touch $out/nix-support/shell
      '';

    bake = pkgs.stdenv.mkDerivation {
      name = "bake-${version}";
      inherit version;

      src = lib.cleanSource ./src/mk;
      buildInputs = [ haskellInputs ];

      buildPhase = ''
        ghc --make -O1 -o bake Bake.hs -main-is Bake.main \
          -threaded -rtsopts "-with-rtsopts=-I0 -qg"
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
      buildInputs = buildInputs ++ [ bake ];

      buildPhase =
        let targets = lib.concatStringsSep " " [ "build/cruise" "build/cruise.ref" ];
        in "bake --lint --no-color --verbose -j$NIX_BUILD_CORES ${targets}";

      installPhase = ''
        install -m0755 -D -t $out/bin ./build/cruise{,.ref}
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
      buildInputs = buildInputs ++ [ bake ];

      buildPhase =
        let targets = lib.concatStringsSep " " [ "build/cruise.c" ];
        in "bake --lint --no-color --verbose -j$NIX_BUILD_CORES ${targets}";

      installPhase = ''
        mkdir -p $out
        cp build/*.c $SAIL_DIR/lib/*.c $SAIL_DIR/lib/*.h $out/
      '';
    };

    /*
    ** Derivation to build all the firmware demos and then keep them in a
    ** derivation by themselves. This is mostly useful for the Docker image so
    ** that it can package the firmware with the small executables.
    */
    firmware-demos = pkgs.stdenv.mkDerivation {
      name = "rv32-firmware-${version}";
      inherit version;

      src = lib.cleanSource ./.;
      buildInputs = buildInputs ++ [ bake ];

      buildPhase = "bake --lint --no-color --verbose -j$NIX_BUILD_CORES demos tests";
      installPhase = ''
        mkdir -p $out/share/rv32-sail
        cp -R build/demos/*.elf $out/share/rv32-sail
        cp -R build/t/*.elf $out/share/rv32-sail
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

    /*
    ** Derivation that builds a Docker image that can be run with ease, using
    ** musl to keep the closure size smaller.
    */
    docker = pkgs.dockerTools.buildLayeredImage {
      name = "rv32-sail";
      tag = "latest";

      contents = [ emulator-static firmware-demos ];
      config = {
        Entrypoint = [ "/bin/cruise" ];
        Cmd        = [ "--help" ];
        WorkingDir = "/share/rv32-sail";
      };
    };

  }; /* jobs */

in jobs
