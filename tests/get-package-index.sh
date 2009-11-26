#!/bin/bash

mkdir packages
cd packages
curl http://hackage.haskell.org/packages/archive/00-index.tar.gz | tar -xvzf -

