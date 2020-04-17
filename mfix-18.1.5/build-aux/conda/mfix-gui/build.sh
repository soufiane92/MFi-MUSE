#!/bin/bash -ex

export MFIX_TEMPLATES="$PREFIX"/share/mfix/templates
mkdir -p "$MFIX_TEMPLATES"

git archive "$GIT_FULL_HASH" benchmarks | tar -x -C "$MFIX_TEMPLATES"
git archive "$GIT_FULL_HASH" queue_templates | tar -x -C "$MFIX_TEMPLATES"
git archive "$GIT_FULL_HASH" tests | tar -x -C "$MFIX_TEMPLATES"
git archive "$GIT_FULL_HASH" tutorials | tar -x -C "$MFIX_TEMPLATES"

"${PYTHON}" setup.py install --package=gui
