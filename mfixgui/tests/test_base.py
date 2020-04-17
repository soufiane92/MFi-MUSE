# -*- coding: utf-8 -*-
"""
run with pytest
"""


import math
import pytest

from PyQt5 import QtCore

from mfixgui.constants import PARAMETER_DICT
from mfixgui.project import Equation
from mfixgui.widgets import base

@pytest.fixture
def widget(mocker, qtbot):
    w = base.LineEdit()
    parameter_dict = mocker.patch('mfixgui.constants.PARAMETER_DICT')
    parameter_dict['test_param'] = 10
    qtbot.addWidget(w)
    return w

def test_lineedit_str(widget, qtbot):
    widget.setdtype(str)
    widget.show()
    qtbot.waitExposed(widget)
    qtbot.keyClicks(widget, 'test string')
    qtbot.wait(10)
    assert widget.value == 'test string'

def test_lineedit_float(widget, qtbot):
    widget.setdtype('dp')
    widget.show()
    qtbot.waitExposed(widget)
    qtbot.keyClicks(widget, '10.0')
    qtbot.wait(10)
    assert widget.value == 10.0

def test_lineedit_float_eq(widget, qtbot):
    widget.setdtype('dp')
    widget.show()
    qtbot.waitExposed(widget)
    qtbot.keyClicks(widget, '10.0*4')
    qtbot.wait(10)
    assert isinstance(widget.value, Equation)
    assert widget.value.eq == '10.0*4'

def test_lineedit_float_eq_pi(widget, qtbot):
    widget.setdtype('dp')
    widget.show()
    qtbot.waitExposed(widget)
    qtbot.keyClicks(widget, 'pi')
    qtbot.wait(10)
    assert isinstance(widget.value, Equation)
    assert widget.value.eq == 'pi'
    assert float(widget.value) == math.pi

@pytest.mark.xfail(reason="widget.value is None")
def test_lineedit_float_eq_param(widget, qtbot):
    widget.setdtype('dp')
    widget.show()
    qtbot.waitExposed(widget)
    qtbot.keyClicks(widget, 'test_param')
    qtbot.wait(10)
    assert isinstance(widget.value, Equation)
    assert widget.value.eq == 'test_param'

#    def test_lineedit_float_range(widget, qtbot):
#        widget.setdtype('dp')
#        widget.show()
#        widget.setValInfo(1, 0)
#        qtbot.waitExposed(widget)
#        qtbot.keyClicks(widget, '100')
#        qtbot.wait(10)
#        with self.assertRaises(ValueError):
#            val = widget.value
#
#        qtbot.keyClicks(widget, '-1')
#        qtbot.wait(10)
#        with self.assertRaises(ValueError):
#            val = widget.value

def test_lineedit_int(widget, qtbot):
    widget.setdtype('i')
    widget.show()
    qtbot.waitExposed(widget)
    qtbot.keyClicks(widget, '10')
    qtbot.wait(10)
    assert widget.value == 10

def test_lineedit_int_with_float(widget, qtbot):
    widget.setdtype('i')
    widget.show()
    qtbot.waitExposed(widget)
    qtbot.keyClicks(widget, '10.0')
    qtbot.wait(10)
    assert widget.value == 10

def test_lineedit_int_eq(widget, qtbot):
    widget.setdtype('i')
    widget.show()
    qtbot.waitExposed(widget)
    qtbot.keyClicks(widget, '10.0*4')
    qtbot.wait(10)
    assert isinstance(widget.value, Equation)
    assert widget.value.eq == '10.0*4'

@pytest.mark.xfail(reason="widget.value is None")
def test_lineedit_int_eq_param(widget, qtbot):
    widget.setdtype('i')
    widget.show()
    qtbot.waitExposed(widget)
    qtbot.keyClicks(widget, 'test_param')
    qtbot.wait(10)
    assert isinstance(widget.value, Equation)
    assert widget.value.eq == 'test_param'

def test_lineedit_no_qcompleter(widget, qtbot):
    widget.show()
    qtbot.waitExposed(widget)
    qtbot.keyClicks(widget, 'si')
    qtbot.wait(10)
    assert not widget._completer.popup().isVisible()

def test_lineedit_qcompleter(widget, qtbot):
    widget.allow_parameters = True
    widget.show()
    qtbot.waitExposed(widget)
    qtbot.keyClicks(widget, 'si')
    qtbot.wait(10)
    assert widget._completer.popup().isVisible()

def test_lineedit_qcompleter_completion_sin(widget, qtbot):
    widget.allow_parameters = True
    widget.show()
    widget.setFocus()
    qtbot.waitExposed(widget)
    qtbot.keyClicks(widget, 'si')
    qtbot.wait(10)
    qtbot.keyClick(widget._completer.popup(), QtCore.Qt.Key_Enter)
    assert widget.text() == 'sin'

def test_lineedit_qcompleter_completion_eq(widget, qtbot):
    widget.allow_parameters = True
    widget.show()
    widget.setFocus()
    qtbot.waitExposed(widget)
    qtbot.keyClicks(widget, '10*3+4/co')
    qtbot.wait(10)
    qtbot.keyClick(widget._completer.popup(), QtCore.Qt.Key_Enter)
    assert widget.text() == '10*3+4/cos'

def test_lineedit_qcompleter_completion_eq_middle(widget, qtbot):
    widget.allow_parameters = True
    widget.show()
    widget.setFocus()
    qtbot.waitExposed(widget)
    qtbot.keyClicks(widget, '10*3+4/cos')
    qtbot.wait(10)
    widget.setCursorPosition(5)
    qtbot.keyClicks(widget, 'si')
    qtbot.keyClick(widget._completer.popup(), QtCore.Qt.Key_Enter)
    assert widget.text() == '10*3+sin4/cos'

def test_lineedit_create_parameter(widget, qtbot):
    widget.allow_parameters = True
    widget.show()
    widget.setFocus()
    qtbot.waitExposed(widget)
    qtbot.keyClicks(widget, '10.0')
    qtbot.wait(10)
    widget.create_parameter()
    assert widget.text() == 'param'
