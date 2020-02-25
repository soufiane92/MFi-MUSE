# -*- coding: utf-8 -*-
"""
run with pytest
"""


import os
import subprocess
import sys

import pytest

from PyQt5.QtCore import Qt
from PyQt5.QtWidgets import QMainWindow

import mfixgui.gui

RUN_NAME = 'DES_FB1'
RUNDIR = '/run/foo'

@pytest.fixture
def mfix_gui(qtbot):
    qmain = QMainWindow()
    mfixgui_fixture = mfixgui.gui.MfixGui(qmain)
    qtbot.addWidget(mfixgui_fixture)
    return mfixgui_fixture


def get_tree_item(gui, name):
    flags = Qt.MatchFixedString | Qt.MatchRecursive
    clist = gui.ui.treewidget_navigation.findItems(name, flags, 0)
    assert len(clist) == 1
    return clist[0]


@pytest.mark.xfail(run=False, reason="Segfault")
def test_setup_current_pane(mocker, mfix_gui):
    def make_mock(txt):
        mock = mocker.Mock()
        mock.text.return_value = txt
        return [mock]
    selectedItems = mocker.patch.object(mfix_gui.ui.treewidget_navigation, 'selectedItems')

    selectedItems.return_value = make_mock("regions")
    mfix_gui.setup_current_pane()
    selectedItems.return_value = make_mock("model_setup")
    mfix_gui.setup_current_pane()
    selectedItems.return_value = make_mock("solids")
    mfix_gui.setup_current_pane()
    selectedItems.return_value = make_mock("initial_conditions")
    mfix_gui.setup_current_pane()
    selectedItems.return_value = make_mock("boundary_conditions")
    mfix_gui.setup_current_pane()
    selectedItems.return_value = make_mock("point_sources")
    mfix_gui.setup_current_pane()
    selectedItems.return_value = make_mock("internal_surfaces")
    mfix_gui.setup_current_pane()
    selectedItems.return_value = make_mock("chemistry")
    mfix_gui.setup_current_pane()
    selectedItems.return_value = make_mock("numerics")
    mfix_gui.setup_current_pane()
    selectedItems.return_value = make_mock("output")
    mfix_gui.setup_current_pane()


def open_tree_item(gui, name):
    gui.ui.treewidget_navigation.setCurrentItem(get_tree_item(gui, name))


@pytest.mark.xfail(reason="FIXME: hangs on Windows", run=False)
def test_save_as(mocker, mfix_gui):
    newname = '%s_new_name' % RUN_NAME
    newpath = os.path.join(RUNDIR, '%s.mfx' % newname)

    check_writable, get_save_filename, writeDatFile = (
        mocker.patch('mfixgui.gui.MfixGui.check_writable'),
        mocker.patch('mfixgui.gui.MfixGui.get_save_filename'),
        mocker.patch.object(mfix_gui.project, 'writeDatFile'),
    )
    mocker.patch('mfixgui.gui.os.path.getmtime')
    get_save_filename.return_value = newpath
    check_writable.return_value = True
    writeDatFile.return_value = True

    mfix_gui.save_as()

    assert newname == mfix_gui.project.get_value('run_name')


@pytest.mark.xfail(reason="None != 'some new text'", run=False)
def test_description_ascii(mocker, mfix_gui, qtbot):
    mocker.patch('mfixgui.gui.os.path.getmtime')
    writeDatFile = mocker.patch.object(mfix_gui.project, 'writeDatFile')
    writeDatFile.return_value = True

    open_tree_item(mfix_gui, 'run')
    cb = mfix_gui.ui.model_setup.combobox_keyword_description
    description = cb.value
    cb.setFocus()
    qtbot.keyClick(cb, Qt.Key_Right)
    new_text = u'some new text'
    qtbot.keyClicks(cb, new_text)
    qtbot.keyClick(cb, Qt.Key_Enter)
    qtbot.mouseClick(mfix_gui.ui.toolbutton_save, Qt.LeftButton)

    new_description = description + new_text
    assert mfix_gui.project.get_value('description') == new_description


@pytest.mark.xfail(reason="None != 'mañana'", run=False)
def test_description_unicode(mocker, mfix_gui, qtbot):
    mocker.patch('mfixgui.gui.os.path.getmtime')
    writeDatFile = mocker.patch.object(mfix_gui.project, 'writeDatFile')
    writeDatFile.return_value = True

    open_tree_item(mfix_gui, 'run')
    cb = mfix_gui.ui.model_setup.combobox_keyword_description
    description = cb.value
    cb.setFocus()
    qtbot.keyClick(cb, Qt.Key_Right)

    new_text = u'mañana'
    ### KeyClicks won't take a unicode string - need to input ñ separately
    qtbot.keyClicks(cb, "ma")
    qtbot.keyClick(cb, Qt.Key_Ntilde)
    # how to input lower-case ñ ?
    qtbot.keyClicks(cb, "ana")
    qtbot.keyClick(cb, Qt.Key_Enter)
    qtbot.mouseClick(mfix_gui.ui.toolbutton_save, Qt.LeftButton)

    new_description = description + new_text
    assert mfix_gui.project.get_value('description') == new_description


@pytest.mark.xfail(reason="FIXME: not a unit test", run=False)
def test_load_all():
    """ load every mfix.dat file for testing """
    cases = [
        os.path.join(root, 'mfix.dat')
        for root, _, files in os.walk('.')
        if 'mfix.dat' in files
    ]

    for case in cases:
        cmd = '%s -m mfixgui.gui -d -linfo -t %s' % (sys.executable, case)
        os.environ["MFIX_NO_VTK"] = "1"
        subprocess.check_call(cmd, shell=True)
