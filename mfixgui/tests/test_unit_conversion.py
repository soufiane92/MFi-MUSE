# -*- coding: utf-8 -*-
"""
run with pytest
"""


import pytest

import mfixgui.unit_conversion

def test_unit_conversion(qtbot):
    mfixgui.unit_conversion.main()
