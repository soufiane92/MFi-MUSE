from PyQt5.QtWidgets import QMainWindow, QListWidget

from mfixgui.file_menu.open_widget import switch_tab_open
import mfixgui.gui


def test_switch_to(mocker, qtbot):
    app = mocker.Mock()
    mocker.patch('mfixgui.gui.MfixGui.add_extra_keyword_doc')
    gui = mfixgui.gui.MfixGui(app)
    switch_tab_open(gui.ui.file_menu_stackedwidget, gui)
