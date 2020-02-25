#!/bin/bash -lex

RUN_NAME="FLD06"
rm -f POST_* &>/dev/null

MFIXSOLVER=./mfixsolver
if (($# > 0)); then
  MFIXSOLVER=$1
fi

rm -f ${RUN_NAME}* &>/dev/null
time -p ${MFIXSOLVER} -f mfix.dat \
  SPECIES_g\(1\)=\'A\' MW_G\(1\)=01.0 IC_X_G\(1,1\)=0.03 \
  SPECIES_g\(2\)=\'B\' MW_G\(2\)=10.0 IC_X_G\(1,2\)=0.27 \
  SPECIES_g\(3\)=\'C\' MW_G\(3\)=25.0 IC_X_G\(1,3\)=0.70

rm -f ${RUN_NAME}* &>/dev/null
time -p ${MFIXSOLVER} -f mfix.dat \
  SPECIES_g\(3\)=\'A\' MW_G\(3\)=01.0 IC_X_G\(1,3\)=0.03 \
  SPECIES_g\(1\)=\'B\' MW_G\(1\)=10.0 IC_X_G\(1,1\)=0.27 \
  SPECIES_g\(2\)=\'C\' MW_G\(2\)=25.0 IC_X_G\(1,2\)=0.70

rm -f ${RUN_NAME}* &>/dev/null
time -p ${MFIXSOLVER} -f mfix.dat \
  SPECIES_g\(2\)=\'A\' MW_G\(2\)=01.0 IC_X_G\(1,2\)=0.03 \
  SPECIES_g\(3\)=\'B\' MW_G\(3\)=10.0 IC_X_G\(1,3\)=0.27 \
  SPECIES_g\(1\)=\'C\' MW_G\(1\)=25.0 IC_X_G\(1,1\)=0.70

post_dats=(AUTOTEST/POST*.dat)

for test_post_file in "${post_dats[@]}"; do
  numdiff -X 1:4 -X 2:4 -a 0.000001 -r 0.05 "${test_post_file}" \
    "$(basename "${test_post_file}")"
done
