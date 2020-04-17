# -*- coding: utf-8 -*-
import os
import sys

from qtpy import QtWidgets, QtCore, QtGui
from mfixgui.tools.qt import get_icon, get_ui
from mfixgui.regexes import re_valid_run_name_qt

SETTINGS = QtCore.QSettings('MFIX', 'MFIX')

def strip_trailing_slash(name):
    return name.rstrip(os.path.sep)

class NewProjectDialog(QtWidgets.QDialog):
    def __init__(self, parent):
        QtWidgets.QDialog.__init__(self, parent)

        self.vtk_widget = parent

        ui = self.ui = get_ui('new_project_popup.ui', self)

        self.setWindowTitle('Create a new project')

        ui.toolbutton_browse.clicked.connect(self.browse)
        ui.toolbutton_browse.setIcon(get_icon('folder.svg'))

        ui.lineedit_project_name.setValidator(QtGui.QRegExpValidator(
            QtCore.QRegExp(re_valid_run_name_qt)))
        ui.combobox_location.editTextChanged.connect(self.check_location)

    def check_location(self, new_text):
        dir_exists = os.path.isdir(new_text)
        self.ui.buttonBox.button(self.ui.buttonBox.Ok).setEnabled(dir_exists)

    def get(self, run_name='new_project'):

        ui = self.ui
        ui.lineedit_project_name.setText(run_name)

        locations = SETTINGS.value('project_locations', '').split(',')
        items = list(set(strip_trailing_slash(location)
                         for location in locations if strip_trailing_slash(location)))
        if not items:
            items = [os.getcwd()]
        ui.combobox_location.addItems(items)

        ok = self.exec_()

        # save items
        items = self.get_items()
        # only save 5 most recent ones
        # What if dirname has a comma in it?
        SETTINGS.setValue('project_locations', ','.join(items[-5:]))

        return (ok == QtWidgets.QDialog.Accepted,
                ui.lineedit_project_name.text(),
                ui.combobox_location.currentText())

    def get_items(self):
        ui = self.ui
        return [strip_trailing_slash(ui.combobox_location.itemText(i))
                for i in range(ui.combobox_location.count())]

    def browse(self):
        cb = self.ui.combobox_location
        loc = QtWidgets.QFileDialog.getExistingDirectory(self, 'Location', cb.currentText())
        loc = strip_trailing_slash(loc)
        if loc:
            if loc not in self.get_items():
                cb.addItem(loc)
            cb.setCurrentText(loc)


def main():

     # create the QApplication
    qapp = QtWidgets.QApplication([])

    ok, run_name, project_loc = NewProjectDialog(None).get('project')
    print(ok, run_name, project_loc)
    sys.exit()

if __name__  == '__main__':
    main()
