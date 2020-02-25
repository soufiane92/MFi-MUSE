from PyQt5.QtWidgets import QSizePolicy

from mfixgui.file_menu.about_widget import _copy_version_info, make_about_widget

import mfixgui.file_menu.about_widget

def test_copy_version_info(mocker, qtbot):
    mocker.patch('mfixgui.gui.MfixGui.add_extra_keyword_doc')
    app = mocker.Mock()
    gui = mfixgui.gui.MfixGui(app)
    _copy_version_info(gui)

def test_make_about_widget(mocker, qtbot):
    mocker.patch('mfixgui.gui.MfixGui.add_extra_keyword_doc')
    app = mocker.Mock()
    gui = mfixgui.gui.MfixGui(app)
    label_policy = QSizePolicy(QSizePolicy.Minimum, QSizePolicy.Maximum)
    make_about_widget(gui, label_policy)
