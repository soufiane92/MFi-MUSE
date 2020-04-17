#!/bin/bash -ex

set -euo pipefail

# MPICMD may be "mpirun -np <N>" or empty string
MPIRANK=${MPIRANK:-}
MPICMD=${MPICMD:-}

# EXEEXT should be ".exe" on Windows; empty string otherwise
EXEEXT=${EXEEXT:-}

if [ -f runtests.sh ]; then
  exec ./runtests.sh "$@"
fi

MFIXSOLVER=./mfixsolver${EXEEXT}
if (($# > 0)); then
  MFIXSOLVER="$1"
fi

POSTMFIX=./postmfix${EXEEXT}
if (($# > 1)); then
  POSTMFIX="$2"
fi

post_script=AUTOTEST/post.script.NEW

if [ -f mfix.dat ]; then
  MFXS[0]=mfix.dat
else
  MFXS=(*.mfx)
fi

if [ -n "${MPICMD}" ]; then
  ${MPICMD} "${MFIXSOLVER}" -f "${MFXS[0]}"
elif [ -n "${MPIRANK}" ]; then
  mpirun -np "${MPIRANK}" "${MFIXSOLVER}" -f "${MFXS[0]}"
else
  "${MFIXSOLVER}" -f "${MFXS[0]}"
fi

if [ -e ${post_script} ]; then
  env OMP_NUM_THREADS=1 "${POSTMFIX}" <${post_script}
fi

shopt -s nullglob
post_dats=(AUTOTEST/POST*.dat)

for test_post_file in "${post_dats[@]:0}"; do
  numdiff -a 0.000001 -r 0.05 "${test_post_file}" "$(basename "${test_post_file}")"
done
