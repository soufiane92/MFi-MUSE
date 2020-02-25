#!/bin/bash -ex

export MFIX_SRC="$PREFIX"/share/mfix/src
mkdir -p "$MFIX_SRC"

git archive "$GIT_FULL_HASH" CMakeLists.txt | tar -x -C "$MFIX_SRC"
git archive "$GIT_FULL_HASH" build-aux | tar -x -C "$MFIX_SRC"
git archive "$GIT_FULL_HASH" model | tar -x -C "$MFIX_SRC"
git archive "$GIT_FULL_HASH" post_mfix | tar -x -C "$MFIX_SRC"

python crow_repo/amalgamate/merge_all.py crow_repo/include
cp crow_all.h "$MFIX_SRC"/model/crow/crow.h
