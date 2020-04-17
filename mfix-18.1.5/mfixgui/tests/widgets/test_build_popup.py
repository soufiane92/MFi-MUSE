# -*- coding: utf-8 -*-
"""
run with pytest
"""

import pytest

from PyQt5.QtWidgets import QMainWindow

import mfixgui.widgets.build_popup


@pytest.fixture
def buildpopup(mocker, qtbot):
    mocker.patch('mfixgui.gui.MfixGui.add_extra_keyword_doc')
    qprocess = mocker.patch('mfixgui.widgets.build_popup.QProcess')
    qprocess.return_value.readAllStandardOutput.return_value = b"[ 42%]  build foo"
    qmain = QMainWindow()
    mfix = mfixgui.gui.MfixGui(qmain)
    popup = mfixgui.widgets.build_popup.make_build_popup(mfix, './')
    popup.project_dir = "foo_dir"
    popup.comboBox_compiler.clear()
    mocker.patch.object(popup, 'build_proc')
    qtbot.addWidget(popup)
    return popup


def test_default(buildpopup):
    """ create build popup and run it """
    buildpopup.show()
    assert mfixgui.widgets.build_popup._get_build_cmd(buildpopup)[1:] == ["-j"]
    buildpopup.close()


def test_dmp(buildpopup, qtbot):
    """ create build popup and run it """

    popup = buildpopup
    popup.show()
    popup.checkBox_dmp.setChecked(True)
    qtbot.keyClicks(popup.comboBox_compiler, "mpifort")
    assert mfixgui.widgets.build_popup._get_build_cmd(popup)[1:] == ["-j",
                                                                     "--dmp",
                                                                     "-DMPI_Fortran_COMPILER=mpifort"]
    popup.close()


def test_smp(buildpopup):
    popup = buildpopup
    popup.show()
    popup.checkBox_smp.setChecked(True)
    assert mfixgui.widgets.build_popup._get_build_cmd(popup)[1:] == ["-j", "--smp"]
    popup.close()


def test_parallel(buildpopup):
    popup = buildpopup
    popup.show()
    popup.checkBox_parallel.setChecked(True)
    assert mfixgui.widgets.build_popup._get_build_cmd(popup)[1:] == ["-j"]
    popup.close()


def test_flags(buildpopup, qtbot):
    popup = buildpopup
    popup.show()
    qtbot.keyClicks(popup.lineEdit_compiler_flags, "foo")
    assert mfixgui.widgets.build_popup._get_build_cmd(popup)[1:] == ['-DCMAKE_Fortran_FLAGS="foo"', '-j']
    popup.close()


def test_all(buildpopup, qtbot):
    popup = buildpopup
    popup.show()
    popup.checkBox_dmp.setChecked(True)
    popup.checkBox_parallel.setChecked(True)
    popup.checkBox_smp.setChecked(True)
    qtbot.keyClicks(popup.lineEdit_compiler_flags, "foo")
    qtbot.keyClicks(popup.comboBox_compiler, "mpifort")
    assert mfixgui.widgets.build_popup._get_build_cmd(popup)[1:] == ['-DCMAKE_Fortran_FLAGS="foo"',
                                                                     '-j', '--smp', '--dmp',
                                                                     '-DMPI_Fortran_COMPILER=mpifort']
    popup.close()


def test_output(buildpopup):
    """ create build popup and run it """

    popup = buildpopup
    popup.textBrowser_output.isVisible = lambda: popup.textBrowser_output.isVisibleTo(popup)

    mfixgui.widgets.build_popup._toggle_output(popup, show=True)
    assert popup.textBrowser_output.isVisibleTo(popup)

    mfixgui.widgets.build_popup._toggle_output(popup)
    assert not popup.textBrowser_output.isVisibleTo(popup)

    mfixgui.widgets.build_popup._toggle_output(popup)
    assert popup.textBrowser_output.isVisibleTo(popup)

    popup.close()


def test_print_to_output(buildpopup):
    """ create build popup and run it """

    popup = buildpopup
    popup.show()
    mfixgui.widgets.build_popup._print_to_output(popup, "foo ", error=True)
    mfixgui.widgets.build_popup._print_to_output(popup, " bar \n baz", error=True)
    assert popup.textBrowser_output.toPlainText() == "foo \n bar \n baz\n"
    popup.close()


def test_error(buildpopup):
    """ create build popup and run it """

    popup = buildpopup
    qp = mfixgui.widgets.build_popup.QProcess
    popup.show()
    mfixgui.widgets.build_popup._error(popup, qp.FailedToStart)
    mfixgui.widgets.build_popup._error(popup, qp.Crashed)
    mfixgui.widgets.build_popup._error(popup, qp.Timedout)
    mfixgui.widgets.build_popup._error(popup, qp.WriteError)
    mfixgui.widgets.build_popup._error(popup, qp.ReadError)
    mfixgui.widgets.build_popup._error(popup, None)
    popup.close()


def test_read_error(buildpopup):
    popup = buildpopup
    popup.show()
    mfixgui.widgets.build_popup._build(popup)
    mfixgui.widgets.build_popup._read_err(popup)
    popup.close()


def test_clean(mocker, buildpopup):
    do_clean = mocker.patch('mfixgui.build_mfixsolver.do_clean')
    exists = mocker.patch('mfixgui.widgets.build_popup.os.path.exists')
    remove = mocker.patch('mfixgui.widgets.build_popup.os.remove')
    remove.return_value = True
    exists.return_value = True

    do_clean.return_value = ''
    popup = buildpopup
    popup.show()
    mfixgui.widgets.build_popup._clean(popup)
    mfixgui.widgets.build_popup._clean(popup)
    popup.close()


def test_check_progress(buildpopup):
    popup = buildpopup
    popup.show()
    mfixgui.widgets.build_popup._build(popup)
    mfixgui.widgets.build_popup._check_progress(popup)
    assert popup.progressBar.value() == 42
    assert popup.label_progressBar.text() == 'Compiling...'
    mfixgui.widgets.build_popup.QProcess.return_value.readAllStandardOutput.return_value = b"[100%]  build foo"
    mfixgui.widgets.build_popup._check_progress(popup)
    mfixgui.widgets.build_popup.QProcess.return_value.readAllStandardOutput.return_value = b"BUILD SUCCESSFUL"
    mfixgui.widgets.build_popup._check_progress(popup)
    assert popup.label_progressBar.text() == 'Linking...'
    popup.close()


def test_finished_building(buildpopup):
    # test success & failure
    popup = buildpopup
    popup.show()
    mfixgui.widgets.build_popup._check_progress(popup)
    mfixgui.widgets.build_popup._finished_building(popup, 0, 1)
    assert popup.label_progressBar.text() == 'Build failed.'
    mfixgui.widgets.build_popup._finished_building(popup, 0, 0)
    assert popup.label_progressBar.text() == 'Build succeeded.'
    mfixgui.widgets.build_popup._finished_building(popup, 1, 0)
    assert popup.label_progressBar.text() == 'Build failed.'
    popup.close()


def test_cancel(mocker, buildpopup):
    popup = buildpopup
    popup.build_proc.readAllStandardOutput.return_value = b""
    popup.build_proc.readAllStandardError.return_value = b""
    popup.show()


def test_build_nt(mocker, buildpopup):
    popup = buildpopup
    popup.show()
    mfixgui.widgets.build_popup.os.name = 'nt'
    mfixgui.widgets.build_popup._build(popup)
    mfixgui.widgets.build_popup._read_err(popup)
    popup.close()
