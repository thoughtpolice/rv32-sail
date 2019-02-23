{ nixpkgs ? null, ... }@args:
(import ./release.nix args).emulator
