#!/bin/bash -lex

RUN_NAME="FLD05"
rm -f POST_* &>/dev/null

MFIXSOLVER=./mfixsolver
if (($# > 0)); then
  MFIXSOLVER=$1
fi

for DELP_X in -3.0 -2.0 -1.0 0.0 1.0 2.0 3.0; do
  for JMAX in 8 16 32 64; do
    rm -f ${RUN_NAME}* &>/dev/null
    time -p ${MFIXSOLVER} -f mfix.dat JMAX=${JMAX} DELP_X=${DELP_X}
  done
done

post_dats=(AUTOTEST/POST*.dat)

for test_post_file in "${post_dats[@]}"; do
  numdiff -a 0.000001 -r 0.05 "${test_post_file}" \
    "$(basename "${test_post_file}")"
done
