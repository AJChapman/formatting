{ pkgs ? import ./pinned-nixpkgs.nix
, compiler ? "default"
, doBenchmark ? false
, doTest ? false
, doProfiling ? true
}:

let

  inherit pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.compiler.${compiler};

  modifiedHaskellPackages = import ./haskell-package-overrides.nix { haskellLib = pkgs.haskell.lib; inherit haskellPackages; };

  withBench = p: if doBenchmark
    then pkgs.haskell.lib.doBenchmark p
    else p;

  withTest = p: if doTest
    then p
    else pkgs.haskell.lib.dontCheck p;

  withProfiling = p: if doProfiling
    then pkgs.haskell.lib.enableLibraryProfiling ( pkgs.haskell.lib.enableExecutableProfiling p)
    else p;

  pkgname = import ./pkgname.nix;
  pkg = modifiedHaskellPackages.callCabal2nix pkgname ./. { };

  standardata-static = pkgs.haskell.lib.justStaticExecutables pkg;

  tags = import (fetchTarball "https://github.com/tek/thax/tarball/9ac46dfef0a99a74e65c838a89d0bbab00170d8b") { inherit pkgs; };

  addons = with pkgs; {
      projectTags = tags.combined.all { targets = [pkg]; };
  };

in

  withProfiling ( withBench ( withTest (pkg) ) ) // addons
