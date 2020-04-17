#!/bin/bash -ex

set -euo pipefail

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

mpirun -np 8 "${MFIXSOLVER}" -f mfix.dat

if [ -e ${post_script} ]; then
  env OMP_NUM_THREADS=1 "${POSTMFIX}" <${post_script}
fi

shopt -s nullglob
post_dats=(AUTOTEST/POST*.dat)

for test_post_file in "${post_dats[@]:0}"; do
  numdiff -a 0.000001 -r 0.05 "${test_post_file}" "$(basename "${test_post_file}")"
done
