#!/bin/bash

set -e


cd $LUMEN
cd ..

patch -p0 < $LACE/install/patches/lumen.gpr-patch
patch -p0 < $LACE/install/patches/lumen-window.adb-patch
patch -p0 < $LACE/install/patches/lumen-window.ads-patch
patch -p0 < $LACE/install/patches/x11.ads-patch

rm $LUMEN/opengl.gpr

