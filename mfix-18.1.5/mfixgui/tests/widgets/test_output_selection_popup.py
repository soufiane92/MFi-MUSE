# -*- coding: utf-8 -*-
"""
run with pytest
"""

from PyQt5.QtWidgets import QMainWindow

from mfixgui.widgets.output_selection_popup import OutputSelectionPopup


def test_default(mocker, qtbot):
    qmain = QMainWindow()
    p = OutputSelectionPopup(qmain)
    p.show()
    p.close()


def test_sort_files(mocker, qtbot):
    qmain = QMainWindow()
    p = OutputSelectionPopup(qmain)
    p.show()
    p.sort_files([])
    p.close()


def test_exec_(mocker, qtbot):
    exec_ = mocker.patch('mfixgui.widgets.output_selection_popup.QDialog.exec_')
    qmain = QMainWindow()
    p = OutputSelectionPopup(qmain)
    p.show()
    p.exec_([])
    p.close()
    exec_.assert_called_once()


def test_get_output_files(mocker, qtbot):
    qmain = QMainWindow()
    p = OutputSelectionPopup(qmain)
    p.show()
    p.sort_files([])
    assert p.get_output_files() == set()
    p.close()
