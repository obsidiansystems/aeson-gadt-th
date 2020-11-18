{ pkgs ? import (builtins.fetchTarball {
      name = "nixos-20.03_2020-11-18";
      url = "https://github.com/nixos/nixpkgs/archive/f05c380a51daee53ac2edb0bac2fd5f1774e977a.tar.gz";
      sha256 = "1xkgv4kvh2nii1kbxi0brjnb15scpzk8rkp7mzycgqh1lzfg23im";
    }) {}
}:
let compilers = [ "ghc802" "ghc844" "ghc865" "ghc884" "ghc8102" ];
in map (c: pkgs.haskell.packages."${c}".callCabal2nix "aeson-gadt-th" ./. {}) compilers
