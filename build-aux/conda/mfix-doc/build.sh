#!/bin/bash -ex

"${PYTHON}" -c "import mfixgui.namelistparser; mfixgui.namelistparser.build_keywords_rst('doc/user_manual/reference')"

sphinx-build -M html doc "$PREFIX"/share/mfix/doc -t release
