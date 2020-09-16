{ haddock ? false
, tests ? false
, examples ? false
, ios ? false
, overlays ? []
, system ? builtins.currentSystem
, crossSystem ? null
, crossOverlays ? []
}:
let
  options =
    { inherit
        haddock
        tests
        examples
        ios
       overlays
        system
        crossSystem
        crossOverlays;
    };
  pkgs = import ./nix options;
  deps = import ./dependencies.nix {};
  release =
    with deps.miso-ghc;
    with deps.miso-ghcjs;
    with pkgs.haskell.lib;
    with pkgs.haskell.packages.ghc865;
    sdistTarball (buildStrictly qwdeserver);
in
{
  inherit deps;
  inherit release;
  
  qwdeclient = pkgs.haskell.packages.ghcjs.qwdeclient;
  qwdeserver = pkgs.haskell.packages.ghc865.qwdeserver;
} 
