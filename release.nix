{ }:
let
  nixpkgsSets = import ./.ci/nixpkgs.nix;
  inherit (nixpkgsSets) nixos2003 nixos1809;
  inherit (nixos2003) lib;
  ghcs = {
    ghc802 = nixos1809;
    ghc844 = nixos1809;
    ghc865 = nixos2003;
    # ghc884 = nixos2003;
    # ghc8101 = nixos2003;
  };
in 
  lib.mapAttrs (ghc: pkgs: pkgs.haskell.packages."${ghc}".callCabal2nix "aeson-gadt-th" ./. {}) ghcs
