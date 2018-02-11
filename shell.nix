{pkgs ? import <nixpkgs> {}}:
(pkgs.callPackage ./default.nix {}).env
