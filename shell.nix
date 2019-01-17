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

    inherit (import ./nix/sail.nix { inherit pkgs ocamlPackages; })
      lem
      linksem
      sail
      ocamlDeps
    ;

  }; /* jobs */

  # These are all the packages that will be available inside the nix-shell
  # environment.
  buildInputs =
    (with jobs; [ lem linksem sail riscv-toolchain ]) ++
    (with pkgs; [ gcc ott z3 zlib wget dtc python3 ]) ++
    [ runghcWrapper haskellInputs ] ++
    (jobs.ocamlDeps)
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
