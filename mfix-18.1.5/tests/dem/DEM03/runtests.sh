#!/bin/bash -lex

RUN_NAME="DEM03"

MFIXSOLVER=./mfixsolver
if (($# > 0)); then
  MFIXSOLVER=$1
fi

DES_IM=ADAMS_BASHFORTH
for DES_ETA in 1.0 0.9 0.8 0.7 0.6 0.5; do
  rm -f ${RUN_NAME}* &>/dev/null
  time -p ${MFIXSOLVER} -f mfix.dat DES_INTG_METHOD=\"${DES_IM}\" \
    DES_EN_INPUT\(1\)=${DES_ETA} \
    DES_EN_INPUT\(2\)=${DES_ETA} \
    DES_EN_INPUT\(3\)=${DES_ETA} \
    DES_EN_WALL_INPUT\(1\)=${DES_ETA} \
    DES_EN_WALL_INPUT\(2\)=${DES_ETA}
done

#diff -q POST_posvel.dat AUTOTEST/POST_posvel.dat

numdiff \
  -a 0.000001 -r 0.05 \
  --exclude=1:4 --exclude=2:4 \
  AUTOTEST/POST_POS1.dat POST_POS1.dat || echo "Post POS1 results differ"

numdiff \
  -a 0.000001 -r 0.05 \
  --exclude=1:4 --exclude=2:4 \
  AUTOTEST/POST_POS2.dat POST_POS2.dat || echo "Post POS2 results differ"
