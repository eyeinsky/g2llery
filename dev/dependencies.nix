{
  packageOverrides = pkgs:
  let
    lib = pkgs.haskell.lib;
  in {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: rec {
        f = path: rest: lib.dontCheck (lib.dontHaddock (haskellPackagesNew.callPackage path rest));

        rapid = f ../.dev/rapid.nix {};
        render = f ../.dev/render.nix {};
        identifiers = f ../.dev/identifiers.nix {};
        multiline = f ../.dev/multiline.nix {};
        web-browser = f ../.dev/web-browser.nix {};
        web-url = f ../.dev/web-url.nix {};
        web = f ../.dev/web.nix {};
      };
    };
  };
}
