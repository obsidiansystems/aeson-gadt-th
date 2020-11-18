{ nixos2003 = import (builtins.fetchTarball {
    name = "nixos-20.03_2020-11-18";
    url = "https://github.com/nixos/nixpkgs/archive/f05c380a51daee53ac2edb0bac2fd5f1774e977a.tar.gz";
    sha256 = "1xkgv4kvh2nii1kbxi0brjnb15scpzk8rkp7mzycgqh1lzfg23im";
   }) {};
   nixos1809 = import (builtins.fetchTarball {
     name = "nixos-18.09_2020-11-18";
     url = "https://github.com/nixos/nixpkgs/archive/a7e559a5504572008567383c3dc8e142fa7a8633.tar.gz";
     sha256 = "16j95q58kkc69lfgpjkj76gw5sx8rcxwi3civm0mlfaxxyw9gzp6";
   }) {};
}
