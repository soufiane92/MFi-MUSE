# -*- coding: utf-8 -*-
"""
run with pytest
"""

from PyQt5.QtWidgets import QMainWindow

from mfixgui.widgets.regions_popup import RegionsPopup


def test_handle_regions_selection(mocker, qtbot):
    qmain = QMainWindow()
    p = RegionsPopup(qmain)
    ui = mocker.patch.object(p, 'ui')
    ui.table.rowHeight.return_value = 1
    ui.table.rowCount.return_value = 1
    ui.table.horizontalHeader.return_value.height.return_value = 1
    ui.table.horizontalHeader.return_value.width.return_value = 1
    ui.table.verticalScrollBar.return_value.isVisible.return_value = 1
    ui.table.verticalScrollBar.return_value.width.return_value = 1
    ui.label_top.size.return_value.width.return_value = 1
    p.show()
    p.popup("the label text")
    p.handle_regions_selection()
    p.close()

def test_handle_type(mocker, qtbot):
    qmain = QMainWindow()
    p = RegionsPopup(qmain)
    ui = mocker.patch.object(p, 'ui')
    ui.table.rowHeight.return_value = 1
    ui.table.rowCount.return_value = 1
    ui.table.horizontalHeader.return_value.height.return_value = 1
    ui.table.horizontalHeader.return_value.width.return_value = 1
    ui.table.verticalScrollBar.return_value.isVisible.return_value = 1
    ui.table.verticalScrollBar.return_value.width.return_value = 1
    ui.label_top.size.return_value.width.return_value = 1
    p.show()
    p.popup("the label text")
    p.handle_type("the val")
    p.close()

def test_enable_row(mocker, qtbot):
    qmain = QMainWindow()
    p = RegionsPopup(qmain)
    ui = mocker.patch.object(p, 'ui')
    ui.table.rowHeight.return_value = 1
    ui.table.rowCount.return_value = 1
    ui.table.horizontalHeader.return_value.height.return_value = 1
    ui.table.horizontalHeader.return_value.width.return_value = 1
    ui.table.verticalScrollBar.return_value.isVisible.return_value = 1
    ui.table.verticalScrollBar.return_value.width.return_value = 1
    ui.label_top.size.return_value.width.return_value = 1
    p.show()
    p.popup("the label text")
    p.enable_row(123, False)
    p.popup("the other label text")
    p.enable_row(123, True)
    p.close()

def test_reset_signals(mocker, qtbot):
    qmain = QMainWindow()
    p = RegionsPopup(qmain)
    p.show()
    p.reset_signals()
    p.close()

def test_clear(mocker, qtbot):
    qmain = QMainWindow()
    p = RegionsPopup(qmain)
    p.show()
    p.clear()
    p.close()

def test_add_row(mocker, qtbot):
    qmain = QMainWindow()
    p = RegionsPopup(qmain)
    p.show()
    p.add_row(["the name", "the shape", "is available"])
    p.close()

def test_get_selection_list(mocker, qtbot):
    qmain = QMainWindow()
    p = RegionsPopup(qmain)
    p.show()
    p.get_selection_list()
    p.close()

def test_popup_boundary(mocker, qtbot):
    qmain = QMainWindow()
    p = RegionsPopup(qmain)
    mocker.patch.object(p, 'parent')
    p.show()
    p.popup("boundary condition test case")
    p.handle_regions_selection()
    p.close()

def test_popup_surface(mocker, qtbot):
    qmain = QMainWindow()
    p = RegionsPopup(qmain)
    mocker.patch.object(p, 'parent')
    p.show()
    p.popup("internal surface test case")
    p.handle_regions_selection()
    p.close()

def test_popup_vtk(mocker, qtbot):
    qmain = QMainWindow()
    p = RegionsPopup(qmain)
    p.show()
    p.popup("VTK output test case")
    p.handle_regions_selection()
    p.close()


def test_popup_monitor(mocker, qtbot):
    qmain = QMainWindow()
    p = RegionsPopup(qmain)
    mocker.patch.object(p, 'parent')
    p.show()
    p.popup("monitor test case")
    p.handle_regions_selection()
    p.close()
