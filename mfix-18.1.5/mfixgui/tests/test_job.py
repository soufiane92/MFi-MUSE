# -*- coding: utf-8 -*-


import pytest

from PyQt5.QtWidgets import QMainWindow

from mfixgui.gui import MfixGui
from mfixgui.job import JobManager

import mfixgui.gui

@pytest.fixture
def mfix(mocker, qtbot):
    app = QMainWindow()
    app.project = mocker.Mock()
    app.project.mfix_gui_comments = {}
    app.warn = mocker.Mock()
    app.objectName = mocker.Mock()
    app.objectName.return_value = "mfixgui"
    gui = MfixGui(app)
    gui.confirm_close = mocker.Mock()
    gui.confirm_close.return_value = True
    gui.confirm_clobber = mocker.Mock()
    gui.confirm_clobber.return_value = True
    gui.get_project_dir = mocker.Mock()
    gui.get_project_dir.return_value = 'foo_project'
    gui.warning = mocker.Mock()
    qtbot.addWidget(gui)
    return gui

def nest_job_manager(mfix, mocker):
    api = mocker.patch('mfixgui.job.PymfixAPI')
    mocker.patch('mfixgui.job.open')
    mocker.patch('mfixgui.job.os.path.isfile')
    mocker.patch('mfixgui.job.os.stat')
    mocker.patch('mfixgui.job.os.unlink')
    api.get_status.return_value = {'paused': 'True'}
    api.mfix_pid = 123
    job_manager = JobManager(mfix)
    pidfile = mocker.Mock()
    job_manager.try_to_connect(pidfile)
    assert bool(job_manager.job)
    assert not bool(job_manager.is_job_paused())

    job_manager.pause_job()
    job_manager.job.status = {'paused':'True'}

    assert bool(job_manager.is_job_paused())

    job_manager.job.reinit('TSTOP=2.0')
    job_manager.stop_mfix()

def nest_job_manager_force_stop(mfix, mocker):
    api = mocker.patch('mfixgui.job.PymfixAPI')
    mocker.patch('mfixgui.job.open')
    mocker.patch('mfixgui.job.os.path.isfile')
    mocker.patch('mfixgui.job.os.stat')
    mocker.patch('mfixgui.job.os.unlink')
    api.get_status.return_value = {'paused': 'True'}
    api.mfix_pid = 123
    job_manager = JobManager(mfix)
    pidfile = mocker.Mock()
    job_manager.try_to_connect(pidfile)
    job_manager.force_stop_mfix()

def nest_job_manager_submit_command(mfix, mocker):
    mocker.patch('mfixgui.job.open')
    mocker.patch('mfixgui.job.os.path.isfile')
    mocker.patch('mfixgui.job.os.stat')
    mocker.patch('mfixgui.job.os.unlink')
    api, popen = (
        mocker.patch('mfixgui.job.PymfixAPI'),
        mocker.patch('mfixgui.job.Popen'),
    )
    api.get_status.return_value = {'paused': 'True'}
    api.mfix_pid = 123
    popen.return_value.communicate.return_value = 'stdout', 'stderr'
    job_manager = JobManager(mfix)
    pidfile = mocker.Mock()
    job_manager.try_to_connect(pidfile)
    job_manager.submit_command(
        'foo_qscript',
        'bar_subcmd',
        'baz_del',
        'baz_status',
        '123',
        {'a':1, 'b':2},
    )
