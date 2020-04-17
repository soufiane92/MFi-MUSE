#!/bin/bash -exl

# set case directory
RUN_NAME="FLD03"

MFIXSOLVER=./mfixsolver
if (($# > 0)); then
  MFIXSOLVER=$1
fi

rm -f POST_* &>/dev/null

for MU in 0.0100 0.0025; do
  rm -f ${RUN_NAME}* &>/dev/null
  time -p mpirun -np 4 "${MFIXSOLVER}" -f mfix.dat MU_G0=${MU} nodesi=2 nodesj=2
done

post_dats=(AUTOTEST/POST*.dat)

for test_post_file in "${post_dats[@]}"; do
  numdiff -a 0.000001 -r 0.05 "${test_post_file}" \
    "$(basename "${test_post_file}")"
done
