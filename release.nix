{pkgs ? import <nixpkgs> {}}:
let jobs = {
   build = pkgs.callPackage ./default.nix {};
   containers = import <nixpkgs/nixos/tests/make-test.nix> ({pkgs, ...} :
   {
      name = "container-host";

      machine = {config, pkgs, lib, ...}:
      {

      };
   });
  };
in jobs
