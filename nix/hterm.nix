{ pkgs }:

let
  hterm = pkgs.fetchurl {
    url = "https://hterm.org/dist/1.84/hterm_all.js";
    name = "hterm-1.84.js";
    sha256 = "0gj6i4qfqa5x7x98a66khp1lsms9cwzq3h0452wfmzq84qbvxqy3";
  };
in hterm
