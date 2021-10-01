#!/bin/bash

set -e

if [ "$LACE" = "" ]; then
  echo Please ensure the LACE environment variable points to the Lace installation root folder.
  exit
fi

mkdir -p assets
cd       assets

ln -s $LACE/4-high/gel/applet/assets  gel


echo Done.