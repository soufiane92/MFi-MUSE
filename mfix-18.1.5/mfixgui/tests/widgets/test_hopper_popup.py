# -*- coding: utf-8 -*-
"""
run with pytest
"""

from PyQt5.QtWidgets import QMainWindow

from mfixgui.widgets.hopper_popup import HopperPopUp


def test_default(qtbot):
    qmain = QMainWindow()
    p = HopperPopUp(qmain)
    p.show()
    p.close()

def test_popup(qtbot):
    qmain = QMainWindow()
    p = HopperPopUp(qmain)
    p.show()
    p.popup()
    p.close()

def test_apply(mocker, qtbot):
    qmain = QMainWindow()
    qmain.add_primitive = None
    qmain.boolean_operation = None
    qmain.geometrydict = None
    mocker.patch.object(qmain, 'add_primitive')
    mocker.patch.object(qmain, 'boolean_operation')
    mocker.patch.object(qmain, 'geometrydict')
    p = HopperPopUp(qmain)
    p.show()
    p.apply_()
    p.close()
