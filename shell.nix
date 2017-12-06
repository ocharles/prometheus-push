{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  overrides = self: super: {
    tasty-discover = pkgs.haskell.lib.dontCheck super.tasty-discover_4_0_0;
  };

  drv = (haskellPackages.override { overrides = overrides; }).callPackage (import ./.) {};

in

  if pkgs.lib.inNixShell then drv.env else drv
