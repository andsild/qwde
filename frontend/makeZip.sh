#!/usr/bin/env bash

set -xe

rm -rv target || true 
mkdir target/
cp -vr result-4/bin/qwdeclient.jsexe/all.js target/ 
cp -vi result-5/bin/qwdeserver target/qwdeserver.bin  

(cd target/ && tar -czvf qwdefrontend.tar.gz all.js qwdeserver.bin) 

tar -tvf target/qwdefrontend.tar.gz
