# To pin to a newer version of nixpkgs, pick the revision of nixpkgs and run:
# nix-prefetch-git --rev <revision> --url https://github.com/NixOS/nixpkgs.git > nixpkgs.json
let
  fetcher = { rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  };
in
  import (fetcher (builtins.fromJSON (builtins.readFile ./nixpkgs.json)))
  {
    config.allowUnfree = true;
  }
