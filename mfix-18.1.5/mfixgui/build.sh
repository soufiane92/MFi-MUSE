#!/bin/sh
# Convenience script for developers: build the mfix solver from current directory, without installing

SOURCE_DIR=$(dirname "$0")/..
PYTHONPATH="$SOURCE_DIR:$PYTHONPATH" python -m mfixgui.build_mfixsolver "$@"
