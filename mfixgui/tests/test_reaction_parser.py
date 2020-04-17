# -*- coding: utf-8 -*-
"""
run with pytest
"""


import mfixgui.reaction_parser

def test_reaction_parser(qtbot):
    mfixgui.reaction_parser.main()
