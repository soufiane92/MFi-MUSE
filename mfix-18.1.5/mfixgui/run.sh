#!/bin/sh
# Convenience script for developers: run the mfix gui from current directory, without installing

SOURCE_DIR=$(dirname "$0")/..
PYTHONPATH="$SOURCE_DIR:$PYTHONPATH" python -m mfixgui.gui -d "$@"
