from unittest.mock import patch

import pytest

import mfixgui.gui

@pytest.fixture
def mfix(mocker, qtbot):
    mocker.patch('mfixgui.gui.MfixGui.add_extra_keyword_doc')
    app = mocker.Mock()
    gui = mfixgui.gui.MfixGui(app)
    mocker.patch.object(gui, 'vtkwidget')
    return gui

def test_handle_clear_recent(mfix):
    mfix.handle_clear_recent()

def test_handle_file_menu_selection_changed(mfix):
    mfix.handle_file_menu_selection_changed(None, None)

@pytest.mark.xfail(run=False, reason="Segfault")
def test_change_file_menu_selection(mocker, mfix):
    close, handle_export, handle_save, handle_save_as = (
        mocker.patch('mfixgui.gui.MfixGui.close'),
        mocker.patch('mfixgui.gui.MfixGui.handle_export'),
        mocker.patch('mfixgui.gui.MfixGui.handle_save'),
        mocker.patch('mfixgui.gui.MfixGui.handle_save_as'),
    )

    mfix._change_file_menu_selection('save')
    handle_save.assert_called_once()

    mfix._change_file_menu_selection('save as')
    handle_save_as.assert_called_once()

    mfix._change_file_menu_selection('export project')
    handle_export.assert_called_once()

    mfix._change_file_menu_selection('export workflow')
    mfix._change_file_menu_selection('import workflow')

    mfix._change_file_menu_selection('quit')
    close.assert_called_once()

    mfix._change_file_menu_selection('new')
    mfix._change_file_menu_selection('open')
    mfix._change_file_menu_selection('')

def test_handle_open_shortcut(mfix):
    mfix.handle_open_shortcut()

def test_handle_new_shortcut(mfix):
    mfix.handle_new_shortcut()

def test_file_menu_animation_finished(mfix):
    mfix.file_menu_animation_finished()

def test_file_menu_animation_finished_hide(mfix):
    mfix.file_menu_animation_finished_hide()

def test_enable_developer_mode(mfix):
    mfix.handle_enable_developer_mode(True)
    mfix.handle_enable_developer_mode(False)

def test_collapse_splitters(mfix):
    mfix.collapse_splitters(True)
    mfix.collapse_splitters(False)

def test_handle_file_menu_open_project(mocker, mfix):
    item = mocker.Mock()
    item.full_path = 'foobar'
    mfix.message = mocker.Mock()
    open_project = mocker.patch('mfixgui.gui.MfixGui.open_project')
    check_unsaved = mocker.patch('mfixgui.file_menu.FileMenu.check_unsaved_abort')
    check_unsaved.return_value = False
    exists = mocker.patch('mfixgui.gui.os.path.exists')
    exists.return_value = True
    mfix.handle_file_menu_open_project(item)
    open_project.assert_called_once()

@pytest.mark.xfail(run=False, reason="None")
def test_handle_file_menu_new_project(mocker, mfix):
    mocker.patch('mfixgui.gui.MfixGui.add_extra_keyword_doc')
    mocker.patch('mfixgui.gui.shutil')
    glob = mocker.patch('mfixgui.gui.glob')
    glob.glob.return_value = ['myudf.f']
    os = mocker.patch('mfixgui.gui.os')
    os.path.exists.return_value = False
    os.linesep = 'the linesep'
    mocker.patch.object(mfix, 'check_writable')
    mocker.patch.object(mfix, 'open_project')
    get_new_project = mocker.patch('mfixgui.gui.NewProjectDialog.get')
    get_new_project.return_value = True, 'foo_runname', 'bar_rundir'
    item = mocker.Mock()
    item.full_path = 'foobar'
    m = mocker.mock_open()

    with patch('mfixgui.gui.open', m, create=True):
        mfix.handle_file_menu_new_project(item)

    mfix.unsaved_flag = True
    mfix.message = mocker.Mock()
    mfix.handle_file_menu_new_project(item)

def test_filter_new(mfix):
    mfix._change_file_menu_selection('new')
    mfix.handle_filter_new(True)
    mfix.handle_filter_new(True)
