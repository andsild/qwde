#!/usr/bin/env bash

SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
cd "${SCRIPTPATH}/../frontend/"
/opt/cabal/bin/cabal run qwdeserver
