{ config ? {}
, nixpkgs ? null
, system ? builtins.currentSystem
}:

let
  minimumNixVersion = "2.1";

  json = with builtins; fromJSON (readFile ./nixpkgs.json);
  pkgs = if nixpkgs != null then nixpkgs else builtins.fetchTarball {
    inherit (json) url sha256;
  };
in
with builtins;
  if (compareVersions nixVersion minimumNixVersion) >= 0
    then import pkgs { inherit config system; }
    else throw ("Invalid Nix version '" + nixVersion + "'; at least " + minimumNixVersion + " is required!")
