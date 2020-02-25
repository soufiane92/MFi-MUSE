#!/bin/bash -lex

# EXEEXT should be ".exe" on Windows; empty string otherwise
EXEEXT=${EXEEXT:-}

MFIXSOLVER=./mfixsolver${EXEEXT}
if (($# > 0)); then
  MFIXSOLVER=$1
fi

rm -f DEM_RESTART* POST_posvel.dat
time -p mpirun -np 4 "${MFIXSOLVER}" TSTOP=1.0 NODESI=2 NODESJ=2 RUN_TYPE=\'NEW\'
diff -q POST_posvel.dat AUTOTEST/POST_posvel.dat

rm -f DEM_RESTART* POST_posvel.dat
time -p mpirun -np 4 "${MFIXSOLVER}" TSTOP=0.33549 NODESI=2 NODESJ=2 RUN_TYPE=\'NEW\'
time -p mpirun -np 4 "${MFIXSOLVER}" TSTOP=1.0 NODESI=2 NODESJ=2 RUN_TYPE=\'RESTART_1\'
diff -q POST_posvel.dat AUTOTEST/POST_posvel.dat
