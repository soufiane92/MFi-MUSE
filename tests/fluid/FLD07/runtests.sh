#!/bin/bash -lex

RUN_NAME="FLD07"

MFIXSOLVER=./mfixsolver
if (($# > 0)); then
  MFIXSOLVER=$1
fi

rm -f POST_*.dat &>/dev/null

#IMAX=64
for IMAX in 6 12 18; do
  rm -f ${RUN_NAME}.* &>/dev/null
  time -p ${MFIXSOLVER} -f mfix.dat IMAX=${IMAX}
done

post_dats=(AUTOTEST/POST*.dat)

for test_post_file in "${post_dats[@]}"; do
  numdiff -a 0.000001 -r 0.05 "${test_post_file}" \
    "$(basename "${test_post_file}")"
done
