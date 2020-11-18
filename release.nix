{ }:
let
   nixos2003 = import (builtins.fetchTarball {
      name = "nixos-20.03_2020-11-18";
      url = "https://github.com/nixos/nixpkgs/archive/f05c380a51daee53ac2edb0bac2fd5f1774e977a.tar.gz";
      sha256 = "1xkgv4kvh2nii1kbxi0brjnb15scpzk8rkp7mzycgqh1lzfg23im";
    }) {};
    nixos1903 = import (builtins.fetchTarball {
      name = "nixos-19.03_2020-11-18";
      url = "https://github.com/nixos/nixpkgs/archive/34c7eb7545d155cc5b6f499b23a7cb1c96ab4d59.tar.gz";
      sha256 = "11z6ajj108fy2q5g8y4higlcaqncrbjm3dnv17pvif6avagw4mcb";
    }) {};
   oldCompilers = [ "ghc802" "ghc844" ];
   compilers = [ "ghc865" "ghc884" "ghc8101" ];
   compileWith = ghcs: pkgs: map (c: pkgs.haskell.packages."${c}".callCabal2nix "aeson-gadt-th" ./. {}) ghcs;
in compileWith compilers nixos2003 ++ compileWith oldCompilers nixos1903
