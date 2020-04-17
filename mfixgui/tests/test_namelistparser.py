import pytest

import mfixgui.namelistparser

@pytest.mark.xfail(reason="need to update test case in namelistparser.main")
def test_namelistparser_test1():
    mfixgui.namelistparser.main()
