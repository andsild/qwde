options: pkgs:
let
  source = import ../source.nix pkgs;
in
with pkgs.haskell.lib;
with pkgs.lib;
self: super:
{
  inherit (pkgs.haskell.packages.ghc865) hpack;
  jsaddle = self.callCabal2nix "jsaddle" "${source.jsaddle}/jsaddle" {};
  jsaddle-dom = self.callCabal2nix "jsaddle-dom" source.jsaddle-dom {};
  jsaddle-warp = dontCheck (self.callCabal2nix "jsaddle-warp" "${source.jsaddle}/jsaddle-warp" {});
  jsaddle-webkit2gtk = self.callCabal2nix "jsaddle-webkit2gtk" "${source.jsaddle}/jsaddle-webkit2gtk" {};
  ghcjs-dom-jsaddle = self.callCabal2nix "ghcjs-dom-jsaddle" "${source.ghcjs-dom}/ghcjs-dom-jsaddle" {};
  ghcjs-dom-jsffi = self.callCabal2nix "ghcjs-dom-jsffi" "${source.ghcjs-dom}/ghcjs-dom-jsffi" {};
  ghcjs-dom = self.callCabal2nix "ghcjs-dom" "${source.ghcjs-dom}/ghcjs-dom" {};
  mkDerivation = args: super.mkDerivation (args // { doCheck = false; });
  doctest = null;
  miso-jsaddle = self.callCabal2nixWithOptions "miso" source.miso "-fjsaddle" {};
  miso = (self.callCabal2nixWithOptions "miso" source.miso "-ftests" {}).overrideDerivation (drv: {
    doHaddock = options.haddock;
    postInstall = pkgs.lib.optionalString options.tests ''
      ${pkgs.closurecompiler}/bin/closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS \
         --jscomp_off=checkVars \
         --externs=$out/bin/tests.jsexe/all.js.externs \
           $out/bin/tests.jsexe/all.js > temp.js
           mv temp.js $out/bin/tests.jsexe/all.js
         '';
  });
  qwdeshared = self.callCabal2nix "qwdeshared" source.qwdeshared {
   qwdeutil = self.qwdeutil;
  }; 
  qwdeutil = self.callCabal2nix "qwdeutil" source.qwdeutil {
  }; 
  qwdeclient = (self.callCabal2nixWithOptions "qwdeclient" source.qwdeclient "-ftests" {}).overrideDerivation (drv: {
    doHaddock = options.haddock;
    postInstall = ''
      ${pkgs.closurecompiler}/bin/closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS \
         --jscomp_off=checkVars \
         --externs=$out/bin/qwdeclient.jsexe/all.js.externs \
           $out/bin/qwdeclient.jsexe/all.js > temp.js
           mv temp.js $out/bin/qwdeclient.jsexe/all.js
         '';
  });
}
