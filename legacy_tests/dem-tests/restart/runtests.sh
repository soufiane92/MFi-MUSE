#!/bin/bash -lex

# EXEEXT should be ".exe" on Windows; empty string otherwise
EXEEXT=${EXEEXT:-}

MFIXSOLVER=./mfixsolver${EXEEXT}
if (($# > 0)); then
  MFIXSOLVER=$1
fi

rm -f DEM_RESTART* POST_posvel.dat
time -p ${MFIXSOLVER} TSTOP=1.0 RUN_TYPE=\'NEW\'
diff -q POST_posvel.dat AUTOTEST/POST_posvel.dat

rm -f DEM_RESTART* POST_posvel.dat
time -p ${MFIXSOLVER} TSTOP=0.33549 RUN_TYPE=\'NEW\'
time -p ${MFIXSOLVER} TSTOP=1.0 RUN_TYPE=\'RESTART_1\'
diff -q POST_posvel.dat AUTOTEST/POST_posvel.dat
