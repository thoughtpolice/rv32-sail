{ config ? {}
, nixpkgs ? null
, system ? builtins.currentSystem
}:

let
  json = with builtins; fromJSON (readFile ./nixpkgs.json);
  pkgs = if nixpkgs != null then nixpkgs else builtins.fetchTarball {
    inherit (json) url sha256;
  };
in import pkgs  { inherit config system; }
