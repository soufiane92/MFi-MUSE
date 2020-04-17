#!/bin/bash -ex

set -euo pipefail

# MPICMD may be "mpirun -np <N>" or empty string
MPICMD=${MPICMD:-}

# EXEEXT should be ".exe" on Windows; empty string otherwise
EXEEXT=${EXEEXT:-}

MFIXSOLVER=./mfixsolver${EXEEXT}
if (($# > 0)); then
  MFIXSOLVER=$1
fi

POSTMFIX=./postmfix${EXEEXT}
if (($# > 1)); then
  POSTMFIX=$2
fi

post_script=AUTOTEST/post.script.NEW

if [ -n "${MPICMD}" ]; then
  ${MPICMD} "${MFIXSOLVER}" -f mfix.dat
else
  "${MFIXSOLVER}" -f mfix.dat
fi
if [ -e ${post_script} ]; then
  env OMP_NUM_THREADS=1 "${POSTMFIX}" <${post_script}
fi

shopt -s nullglob
post_dats=(AUTOTEST/POST*.dat)

for test_post_file in "${post_dats[@]:0}"; do
  actual_output_file=$(basename "${test_post_file}")
  numdiff -a 0.000001 -r 0.05 -X 4 "${test_post_file}" "${actual_output_file}"
done
