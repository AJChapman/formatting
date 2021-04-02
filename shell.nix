{ pkgs ? import ./pinned-nixpkgs.nix
, compiler ? "default"
, doBenchmark ? false
, doTest ? true
}:

let
  inherit pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  modifiedHaskellPackages = import ./haskell-package-overrides.nix { haskellLib = pkgs.haskell.lib; inherit haskellPackages; };

  drv = import ./default.nix { inherit pkgs compiler doBenchmark doTest; };
  drvWithTools = pkgs.haskell.lib.addBuildDepends drv (with pkgs; [
    cabal-install ghcid modifiedHaskellPackages.pretty-simple modifiedHaskellPackages.weeder modifiedHaskellPackages.doctest modifiedHaskellPackages.haskell-ci modifiedHaskellPackages.hkgr
  ]);
in
  if pkgs.lib.inNixShell then drvWithTools.env else drv
