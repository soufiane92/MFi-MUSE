#!/bin/bash -ex

rm -rf build
mkdir build
cd build

cmake .. -DENABLE_CROW=ON -DBOOST_ROOT="$CONDA_PREFIX" -DVERSION="${PKG_VERSION}-${PKG_BUILDNUM}"
make
cp mfixsolver "$PREFIX"/bin/mfixsolver-crow
