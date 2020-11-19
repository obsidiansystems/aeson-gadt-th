{ }:
let
  nixpkgsSets = import ./.ci/nixpkgs.nix;
  inherit (nixpkgsSets) nixos1809 nixos2003;
  inherit (nixos2003) lib;
  ghcs = rec {
    ghc802 = nixos1809.haskell.packages.ghc802;
    ghc844 = nixos1809.haskell.packages.ghc844;
    ghc865 = nixos2003.haskell.packages.ghc865;
    ghc884 = nixos2003.haskell.packages.ghc884;
    ghc8101 = nixos2003.haskell.packages.ghc8101.override {
      overrides = self: super: {
        th-expand-syns = self.callHackageDirect {
          pkg = "th-expand-syns";
          ver = "0.4.6.0";
          sha256 = "1l2g98jfg86blp8mkkvzh90h557l5qklw1nn045zqb5am8977dgq";
        } {};
        ChasingBottoms = self.callHackageDirect {
          pkg = "ChasingBottoms";
          ver = "1.3.1.8";
          sha256 = "sha256:0klxmb6pgl2xv5206gn2m3n1di2aidkfyi5rlqcfdx5qvpbnhl19";
        } {};
        haskell-src-meta = self.callHackageDirect {
          pkg = "haskell-src-meta";
          ver = "0.8.5";
          sha256 = "1dhncvsyv2kc8x18gvr7if4pr7vvypl0lr450jaaj3xj7rly3lwv";
        } {};
        haskell-src-exts = self.callHackageDirect {
          pkg = "haskell-src-exts";
          ver = "1.23.1";
          sha256 = "sha256:144q88agqqfpc8z1h2jr6mgx5xs72wxkrx4kbpsfg9cza3jm9fbx";
        } {};
      };
    };
    ghc8101_aeson15 = ghc8101.override {
      overrides = self: super:
      let lib = nixos2003.haskell.lib;
      in {
        these = self.callHackageDirect {
          pkg = "these";
          ver = "1.1.1.1";
          sha256 = "1i1nfh41vflvqxi8w8n2s35ymx2z9119dg5zmd2r23ya7vwvaka1";
        } {};
        aeson = self.callHackageDirect {
          pkg = "aeson";
          ver = "1.5.4.1";
          sha256 = "1kwhxfxff2jrrlrqmr9m846g0lq2iin32hwl5i8x7wqhscx5swh5";
        } {};
        hashable-time = lib.doJailbreak super.hashable-time;
        Diff = self.callHackageDirect {
          pkg = "Diff";
          ver = "0.4.0";
          sha256 = "1phz4cz7i53jx3d1bj0xnx8vpkk482g4ph044zv5c6ssirnzq3ng";
        } {};
        doctest = self.callHackageDirect {
          pkg = "doctest";
          ver = "0.16.3";
          sha256 = "0rm91akq3d4b8xa127dklgd1vg2x9xv962pg98i7xhgnllp6i5r3";
        } {};
        quickcheck-instances = self.callHackageDirect {
          pkg = "quickcheck-instances";
          ver = "0.3.23";
          sha256 = "1baqh2harkcx7igqmk6p040vmchy30wnh1crwwvzcxqv22iwyfrw";
        } {};
      };
    };
  };
in
  lib.mapAttrs (_: ghc: ghc.callCabal2nix "aeson-gadt-th" ./. {}) ghcs
