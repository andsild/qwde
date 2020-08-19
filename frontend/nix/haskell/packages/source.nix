{ lib, fetchFromGitHub, fetchgit, fetchzip, ... }:
with lib;
let
  make-src-filter = src: with lib;
    cleanSourceWith {
      inherit src;
      filter =
        name: type: let baseName = baseNameOf (toString name); in
         ((type == "regular" && hasSuffix ".hs" baseName) ||
         (hasSuffix ".yaml" baseName) ||
         (hasSuffix ".cabal" baseName) ||
         (hasSuffix ".css" baseName) ||
         (hasSuffix ".html" baseName) ||
         (hasSuffix ".png" baseName) ||
         (hasSuffix ".js" baseName) ||
         (baseName == "README.md") ||
         (baseName == "LICENSE") ||
         (type == "directory" && baseName != "examples") ||
         (type == "directory" && baseName != "dist"));
    };
in
{
  qwdeserver = make-src-filter ../../../qwdeserver;
  qwdeclient = make-src-filter ../../../qwdeclient;
  qwdeshared = make-src-filter ../../../qwdeshared;
  qwdeutil = make-src-filter ../../../qwdeutil;
  jsaddle = fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle";
    rev = "d569be43f92b9b8c01dc3ee4c41401ab406a2076";
    sha256 = "1m1xxy4l9ii91k1k504qkxh9k1ybprm1m66mkb9dqlwcpyhcccmv";
  };
  miso = fetchFromGitHub {
    owner  = "dmjio";
    repo   = "miso";
    rev    = "b25512dfb0cc316902c33f9825355944312d1e15";
    sha256 = "007cl5125zj0c35xim8935k0pvyd0x4fc0s7jryc3qg3pmjbszc9";
  };
  jsaddle-dom = fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle-dom";
    rev = "6ce23c5";
    sha256 = "1wpwf025czibkw6770c99zk7r30j6nh7jdzhzqbi2z824qyqzbnw";
  };
  ghcjs-dom = fetchFromGitHub {
    owner = "ghcjs";
    repo = "ghcjs-dom";
    rev = "b8e483a";
    sha256 = "06qlbbhjd0mlv5cymp5q0rb69a333l0fcif5zwa83h94dh25c1g7";
  };
  webkit2gtk3-javascriptcore = fetchFromGitHub {
    owner = "gtk2hs";
    repo = "webkit-javascriptcore";
    rev = "5868624";
    sha256 = "0aj0cvcbnzrdi1ynahpb4034vadfrs99n5qa8dar1srhijv55g8b";
  };
}
