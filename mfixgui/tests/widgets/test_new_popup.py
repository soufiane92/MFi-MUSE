from PyQt5.QtWidgets import QMainWindow

from mfixgui.tools import get_mfix_home
from mfixgui.widgets.new_popup import NewProjectDialog

def test_default(qtbot):
    qmain = QMainWindow()
    p = NewProjectDialog(qmain)
    p.show()
    p.close()

def test_check_location(qtbot):
    qmain = QMainWindow()
    p = NewProjectDialog(qmain)
    p.show()
    p.check_location("foo bar")
    p.check_location(get_mfix_home())
    p.close()

def test_get(mocker, qtbot):
    qmain = QMainWindow()
    p = NewProjectDialog(qmain)
    exec_ = mocker.patch.object(p, 'exec_')
    exec_.return_value = "ok"
    p.show()
    p.get()
    p.close()

def test_get_items(qtbot):
    qmain = QMainWindow()
    p = NewProjectDialog(qmain)
    p.show()
    p.get_items()
    p.close()
