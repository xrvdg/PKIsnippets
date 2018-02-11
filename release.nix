{pkgs ? import <nixpkgs> {}}:

let 
   jobs = {
   build = import ./default.nix {};
  };
in jobs
