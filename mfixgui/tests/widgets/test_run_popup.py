# -*- coding: utf-8 -*-
"""
run with pytest
"""

# import platform

import pytest
import pytest_mock

from PyQt5.QtWidgets import QMainWindow

from mfixgui.gui import MfixGui
import mfixgui.gui
from mfixgui.widgets.run_popup import RunPopup
import mfixgui.widgets.run_popup


@pytest.fixture
def mfix(mocker, qtbot):
    MfixGui.project_file = 'foobar'
    app = QMainWindow()

    app.project = mocker.Mock()
    app.project.mfix_gui_comments = {}
    app.warn = mocker.Mock()
    app.objectName = mocker.Mock()
    app.objectName.return_value = "mfixgui"

    mfix = MfixGui(app)
    mfix.settings = mocker.Mock()
    mfix.settings.value.return_value = []
    return mfix


def test_popup(mocker, mfix):
    runpopup = RunPopup(None, mfix)
    runpopup.solver_list = {'some/solver': '/foo/bar/mfixsolver'}
    runpopup.ui.listwidget_solver_list.currentItem = mocker.Mock()
    runpopup.ui.listwidget_solver_list.currentItem.return_value.text.return_value = "some/solver"
    runpopup.show()
    runpopup.popup()
    assert runpopup.get_run_command() == ["/foo/bar/mfixsolver", "-s", "-f", "foobar"]
    runpopup.close()


def test_get_exe_flags(mocker, mfix):
    runpopup = RunPopup(None, mfix)
    runpopup.solver_list = {'some/solver': '/foo/bar/mfixsolver'}
    runpopup.ui.listwidget_solver_list.currentItem = mocker.Mock()
    runpopup.ui.listwidget_solver_list.currentItem.return_value.text.return_value = "some/solver"
    runpopup.show()
    runpopup.popup()
    runpopup.get_exe_flags('some/solver')


def test_handle_remove_exe(mocker, mfix):
    runpopup = RunPopup(None, mfix)
    runpopup.solver_list = {'[default]/mfixsolver': 'foobar'}
    runpopup.ui.listwidget_solver_list.currentItem = mocker.Mock()
    runpopup.ui.listwidget_solver_list.currentItem.return_value.text.return_value = "[default]/mfixsolver"
    runpopup.show()
    runpopup.popup()
    runpopup.handle_remove_exe()


@pytest.mark.xfail(reason="FIXME:")
def test_browse_template_exe(mocker, mfix):
    runpopup = RunPopup(None, mfix)
    runpopup.show()
    runpopup.popup()
    mfixgui.widgets.run_popup.QFileDialog = mocker.Mock()
    mfixgui.widgets.run_popup.QFileDialog.DontResolveSymlinks = None
    mfixgui.widgets.run_popup.QFileDialog.getExistingDirectory.return_value = None
    mfixgui.widgets.run_popup.QFileDialog.getOpenFileName.return_value = None
    runpopup.handle_browse_template()


def test_finish_with_dialog(mfix):
    mfix.settings.value.return_value = ''
    popup = RunPopup(None, mfix)
    popup.show()
    popup.popup()
    popup.finish_with_dialog()


def test_submit(mocker, mfix):
    runpopup = RunPopup(None, mfix)
    runpopup.solver_list = {'some/solver': '/foo/bar/mfixsolver'}
    runpopup.ui.listwidget_solver_list.currentItem = mocker.Mock()
    runpopup.ui.listwidget_solver_list.currentItem.return_value.text.return_value = "some/solver"
    runpopup.show()
    runpopup.popup()
    runpopup.ui.combobox_template.currentText = mocker.Mock()
    runpopup.ui.combobox_template.currentText.return_value = "test_template"
    runpopup.templates = {'test_template':
                          {'script':'foobar',
                           'options':{'submit':None}}}
    runpopup.submit()


def test_run(mocker, mfix):
    runpopup = RunPopup(None, mfix)
    runpopup.solver_list = {'some/solver': '/foo/bar/mfixsolver'}
    runpopup.ui.listwidget_solver_list.currentItem = mocker.Mock()
    runpopup.ui.listwidget_solver_list.currentItem.return_value.text.return_value = "some/solver"
    runpopup.show()
    runpopup.popup()
    runpopup.run()


def test_start_command(mfix):
    popup = RunPopup(None, mfix)
    popup.show()
    popup.popup()
    popup.start_command(["the_cmd"], "bar", {})
