"""Help popup dialog"""

from collections import OrderedDict
from qtpy import QtWidgets, QtCore
from mfixgui.tools.qt import get_icon, sub_icon_size
from mfixgui.widgets.base import LineEdit

class HelpDialog(QtWidgets.QDialog):

    def __init__(self, parent):
        QtWidgets.QDialog.__init__(self, parent)
        self.gui = parent
        self.setWindowIcon(get_icon('mfix.png'))
        self.setWindowTitle('Help')

        self.grid_layout = QtWidgets.QGridLayout(self)
        self.grid_layout.setContentsMargins(5, 5, 5, 5)

        self.lineedit = LineEdit()
        self.lineedit.key = 'keyword'
        self.grid_layout.addWidget(QtWidgets.QLabel("Enter keyword:"), 0, 0)
        self.grid_layout.addWidget(self.lineedit, 1, 0)
        self.lineedit.value_updated.connect(self.do_help)

    def do_help(self, widget, value_dict, args):
        keyword = value_dict.get('keyword')
        if keyword:
            r = self.gui.find_keyword(keyword)
            if not r:
                self.gui.error("Unknown keyword %s" % keyword, popup=True)
        self.close()
