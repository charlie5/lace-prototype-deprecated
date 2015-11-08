#!/bin/bash

set -e

export OS=Linux

mkdir -p build

gprclean -P../../../../library/lace.gpr

po_gnatdist -P simple_chat.gpr simple_chat.dsa 

rmdir  build
rm -fr dsa
