# -*- coding: utf-8 -*-
import os
import fnmatch

from qtpy.QtWidgets import QDialog

from mfixgui.constants import *
from mfixgui.tools import case_insensitive
from mfixgui.tools.qt import get_ui

class OutputSelectionPopup(QDialog):
    def __init__(self, parent=None):
        QDialog.__init__(self, parent)

        self.ui = get_ui('output_selection_popup.ui', self)

        self.setWindowTitle('Delete Files?')

    def exec_(self, files, force_remove=False, heading='Delete the following files?', title='Delete Files?'):

        ui = self.ui
        enable = not force_remove
        ui.checkbox_res.setEnabled(enable)
        ui.checkbox_spx.setEnabled(enable)
        ui.checkbox_vtk.setEnabled(enable)
        ui.checkbox_monitor.setEnabled(True)
        ui.checkbox_other.setEnabled(enable)

        ui.label_heading.setText(heading)
        self.setWindowTitle(title)

        self.sort_files(files)
        return QDialog.exec_(self)

    def sort_files(self, files):
        ui = self.ui
        ui.listwidget_restart.clear()
        ui.listwidget_spx.clear()
        ui.listwidget_vtk.clear()
        ui.listwidget_monitor.clear()
        ui.listwidget_other.clear()

        ui.checkbox_res.setChecked(True)
        ui.checkbox_spx.setChecked(True)
        ui.checkbox_vtk.setChecked(True)
        ui.checkbox_monitor.setChecked(True)
        ui.checkbox_other.setChecked(True)

        files = sorted(files)

        self.restart = []
        self.spx = []
        self.vtk = []
        self.monitor = []
        self.other = []

        def filter(files, pat): # case insensitive
            return fnmatch.filter(files, case_insensitive(pat))

        for s in RESTART_FILES:
            ff = filter(files, s)
            self.restart.extend(ff)
            ui.listwidget_restart.addItems([os.path.basename(f) for f in ff])
        for s in SPX_FILES:
            ff = filter(files, s)
            self.spx.extend(ff)
            ui.listwidget_spx.addItems([os.path.basename(f) for f in ff])
        for s in VTK_FILES:
            ff = filter(files, s)
            self.vtk.extend(ff)
            ui.listwidget_vtk.addItems([os.path.basename(f) for f in ff])
        for s in MONITOR_FILES:
            ff = filter(files, s)
            self.monitor.extend(ff)
            ui.listwidget_monitor.addItems([os.path.basename(f) for f in ff])
        for s in OTHER_FILES:
            ff = filter(files, s)
            self.other.extend(ff)
            ui.listwidget_other.addItems([os.path.basename(f) for f in ff])

    def get_output_files(self):
        ui = self.ui

        files = []
        for gb, fs in [(ui.checkbox_res, self.restart),
                       (ui.checkbox_spx, self.spx),
                       (ui.checkbox_vtk, self.vtk),
                       (ui.checkbox_monitor, self.monitor),
                       (ui.checkbox_other, self.other)]:
            if gb.isChecked():
                files.extend(fs)
        return set(files)
