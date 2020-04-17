# -*- coding: utf-8 -*-
"""
run with pytest
"""

from mfixgui.tools import num_to_time


def test_num_to_time():
    assert num_to_time("3600") == "00:01:00:00"
    assert num_to_time("3600", "s", "hrs") == 1
    assert num_to_time("1.5", "d", "s") == 129600
    assert num_to_time("1008", "hr", "days") == 42
    assert num_to_time("0.1", "m", "time") == "00:00:00:06"
    assert num_to_time("12345", "m", "mins") == 12345
