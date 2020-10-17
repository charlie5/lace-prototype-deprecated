#!/bin/bash

set -e

export OS=Linux

mkdir -p build

po_gnatdist -P simple_chat.gpr simple_chat.dsa 

rm -fr build
rm -fr dsa
