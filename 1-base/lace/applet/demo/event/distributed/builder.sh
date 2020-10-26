#!/bin/bash

set -e

export OS=Linux

mkdir -p build

rm -fr dsa
export Build_Mode=debug
po_gnatdist -P simple_chat.gpr simple_chat.dsa -cargs -g -largs -g

#rm -fr build
#rm -fr dsa
