{nixpkgs ? import <nixpkgs> {}}:

let 
   jobs = {
   build = import ./default.nix {inherit nixpkgs;};
  };
in jobs
