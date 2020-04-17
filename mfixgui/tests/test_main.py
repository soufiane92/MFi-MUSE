# -*- coding: utf-8 -*-
"""
run with pytest
"""

import pytest
import pytest_mock

import mfixgui.gui


def test_clear(mocker):
    settings = mocker.patch('mfixgui.gui.SETTINGS')
    mfixgui.gui.main_args(['-c'])
    settings.clear.assert_called_once()
    settings.sync.assert_called_once()


def test_missing_project(mocker):
    print_help, isdir, isfile = (
        mocker.patch('argparse.ArgumentParser.print_help'),
        mocker.patch('mfixgui.gui.os.path.isdir'),
        mocker.patch('mfixgui.gui.os.path.isfile'),
    )
    isdir.return_value = False
    isfile.return_value = False
    mfixgui.gui.main_args(['foobar.mfx'])
    print_help.assert_called_once()


def test_empty_directory(mocker):
    print_help, glob, isdir, isfile = (
        mocker.patch('argparse.ArgumentParser.print_help'),
        mocker.patch('mfixgui.gui.glob.glob'),
        mocker.patch('mfixgui.gui.os.path.isdir'),
        mocker.patch('mfixgui.gui.os.path.isfile'),
    )
    isdir.return_value = True
    isfile.return_value = False
    glob.return_value = []
    mfixgui.gui.main_args(['foobar.mfx'])
    print_help.assert_called_once()


def test_mfix_dat(mocker, qtbot):
    mocker.patch('mfixgui.gui.MfixGui')
    mocker.patch('mfixgui.gui.QtWidgets.QApplication')
    mocker.patch('mfixgui.gui.os.path.exists')
    glob, isdir, isfile = (
        mocker.patch('mfixgui.gui.glob.glob'),
        mocker.patch('mfixgui.gui.os.path.isdir'),
        mocker.patch('mfixgui.gui.os.path.isfile'),
    )
    isdir.return_value = True
    isfile.return_value = False
    glob.return_value = ['mfix.dat']
    mfixgui.gui.main_args(['foobar.mfx'])
    mfixgui.gui.gui.open_project.assert_called_once()


def test_test(mocker, qtbot):
    mocker.patch('mfixgui.gui.MfixGui')
    mocker.patch('mfixgui.gui.QtWidgets.QApplication')
    mfixgui.gui.main_args(['--test', '--thumbnails'])
    mfixgui.gui.gui.navigate_all.assert_called_once()
    mfixgui.gui.gui.create_project_thumbnail.assert_called_once()


def test_restore_geometry(mocker, qtbot):
    mocker.patch('mfixgui.gui.MfixGui')
    mocker.patch('mfixgui.gui.QtWidgets.QApplication')
    settings = mocker.patch('mfixgui.gui.SETTINGS')
    settings.value.side_effect = lambda a, *arg: {
        'developer_mode': 123,
        'geometry': 'foo_geo',
        'splitter_graphics_cmd_output': [1, 2, 3],
        'splitter_left_right': [1, 2, 3],
    }.get(a)
    mfixgui.gui.main_args([])
    mfixgui.gui.gui.restoreGeometry.assert_called_once()
    mfixgui.gui.gui.ui.splitter_graphics_cmd_output.setSizes.assert_called_once()
    mfixgui.gui.gui.ui.splitter_left_right.setSizes.assert_called_once()


def test_settings_style(mocker, qtbot):
    mocker.patch('mfixgui.gui.MfixGui')
    qapp, settings = (
        mocker.patch('mfixgui.gui.QtWidgets.QApplication'),
        mocker.patch('mfixgui.gui.SETTINGS'),
    )
    settings.value.side_effect = lambda a, *arg: {
        'app_style': 'fusion',
        'developer_mode': 123,
    }.get(a)
    mfixgui.gui.main_args([])
    qapp.return_value.setStyle.assert_called_with('fusion')


def test_cmdline_style(mocker, qtbot):
    _, qapp = (
        mocker.patch('mfixgui.gui.MfixGui'),
        mocker.patch('mfixgui.gui.QtWidgets.QApplication'),
    )
    mfixgui.gui.main_args(['--style=fusion'])
    qapp.return_value.setStyle.assert_called_with('fusion')


def test_exe(mocker, qtbot):
    mocker.patch('mfixgui.gui.MfixGui')
    mocker.patch('mfixgui.gui.QtWidgets.QApplication')
    mfixgui.gui.main_args(['--exe=foosolver'])
    assert mfixgui.gui.gui.commandline_option_exe == 'foosolver'


def test_settings_project_load(mocker, qtbot):
    mocker.patch('mfixgui.gui.MfixGui')
    mocker.patch('mfixgui.gui.QtWidgets.QApplication')
    settings, exists = (
        mocker.patch('mfixgui.gui.SETTINGS'),
        mocker.patch('mfixgui.gui.os.path.exists'),
    )
    exists.return_value = True
    settings.value.side_effect = lambda a, *arg: {
        'app_style': 'fusion',
        'developer_mode': 123,
        'project_file': 'loadme.mfx',
    }.get(a)
    mfixgui.gui.main_args([])
    mfixgui.gui.gui.open_project.assert_called_with('loadme.mfx', interactive=True)


def test_settings_project_noload(mocker, qtbot):
    mocker.patch('mfixgui.gui.MfixGui')
    mocker.patch('mfixgui.gui.QtWidgets.QApplication')
    settings, exists = (
        mocker.patch('mfixgui.gui.SETTINGS'),
        mocker.patch('mfixgui.gui.os.path.exists'),
    )
    exists.return_value = True
    settings.value.side_effect = lambda a, *arg: {
        'app_style': 'fusion',
        'developer_mode': 123,
        'project_file': 'dontloadme.mfx',
    }.get(a)
    mfixgui.gui.main_args(['--noload'])
    mfixgui.gui.gui.open_project.assert_not_called()


def test_nonodeworks(mocker, qtbot):
    MfixGui = mocker.patch('mfixgui.gui.MfixGui')
    mocker.patch('mfixgui.gui.QtWidgets.QApplication')
    mfixgui.gui.main_args(['--nonodeworks'])
    _, kwargs = MfixGui.call_args
    assert kwargs['loadnodeworks'] is False
    assert kwargs['loadvtk'] is True
    assert kwargs['project_file'] is None


def test_novtk(mocker, qtbot):
    MfixGui = mocker.patch('mfixgui.gui.MfixGui')
    mocker.patch('mfixgui.gui.QtWidgets.QApplication')
    mfixgui.gui.main_args(['--novtk'])
    _, kwargs = MfixGui.call_args
    assert kwargs['loadnodeworks'] is True
    assert kwargs['loadvtk'] is False
    assert kwargs['project_file'] is None


def test_developer(mocker, qtbot):
    mocker.patch('mfixgui.gui.MfixGui')
    mocker.patch('mfixgui.gui.QtWidgets.QApplication')
    mfixgui.gui.main_args(['--developer'])
