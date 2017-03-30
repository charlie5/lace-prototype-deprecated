#!/bin/bash

set -e

gprbuild -XOS=Linux              \
         -Xrestrictions=xgc      \
         -Xopengl_platform=glx   \
         -Xopengl_profile=desk   \
         -XBuild_Mode=debug      \
         -XFLORIST_BUILD=default \
         -XBUILD_KIND=release    \
         -XOSTYPE=linux-gnu      \
         -XLIBRARY_TYPE=static   \
         -XSDLADA_BUILD=release.static \
         -P ../../../demo/hello_mmi/hello_lumen_mmi.gpr


valgrind --suppressions=../suppress-mesa.supp \
         --log-file=leak-check.txt            \
         --leak-check=yes                     \
         ../../../demo/hello_mmi/launch_hello_mmi

cat leak-check.txt

