{ haskellLib
, haskellPackages }:
haskellPackages.override {
  overrides = self: super: {
    # Put any overrides here, e.g:
    #
    # Jailbreak a certain package:
    #   pandoc-lens = haskellLib.doJailbreak super.pandoc-lens;
    #
    # Use a local copy of another package:
    #   pandoc-wrapper = super.callPackage ../pandoc-wrapper/pkg.nix { };

    # Fix some broken stuff
    haskell-ci = haskellLib.unmarkBroken super.haskell-ci;
    lattices = haskellLib.unmarkBroken super.lattices;
    universe-reverse-instances = haskellLib.unmarkBroken super.universe-reverse-instances;
    zinza = haskellLib.doJailbreak (haskellLib.unmarkBroken super.zinza);
  };
}
