#!/bin/bash -lex

RUN_NAME="FLD08"

MFIXSOLVER=./mfixsolver
if (($# > 0)); then
  MFIXSOLVER=$1
fi

rm -f POST_*.dat VTU* &>/dev/null

rm -f ${RUN_NAME}* &>/dev/null
time -p ${MFIXSOLVER} -f mfix.dat

post_dats=(AUTOTEST/POST*.dat)

for test_post_file in "${post_dats[@]}"; do
  numdiff -a 0.000001 -r 0.05 "${test_post_file}" \
    "$(basename "${test_post_file}")"
done
