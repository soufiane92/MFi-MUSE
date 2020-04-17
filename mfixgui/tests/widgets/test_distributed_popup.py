# -*- coding: utf-8 -*-
"""
run with pytest
"""

import pytest

from PyQt5.QtWidgets import QMainWindow

from mfixgui.widgets.distributed_popup import DistributionPopUp

@pytest.fixture
def distpopup(mocker, qtbot):
    qmain = QMainWindow()
    qmain.copy_geometry = None
    qmain.geometrydict = None
    qmain.get_input_data = None
    mocker.patch.object(qmain, 'copy_geometry')
    mocker.patch.object(qmain, 'geometrydict')
    mocker.patch.object(qmain, 'get_input_data')
    mocker.patch('mfixgui.widgets.distributed_popup.vtk')
    p = DistributionPopUp(qmain)
    get_bounds = mocker.patch('mfixgui.widgets.distributed_popup.DistributionPopUp.get_bounds')
    get_bounds.return_value = (1, 2, 3, 4, 5, 6)
    qtbot.addWidget(p)
    return p

def test_dist_changed(qtbot):
    qmain = QMainWindow()
    p = DistributionPopUp(qmain)
    p.dist_changed()
    p.show()
    p.close()

def test_popup(distpopup):
    distpopup.show()
    distpopup.popup()
    distpopup.close()

def test_apply(distpopup):
    distpopup.show()
    distpopup.apply_()
    distpopup.close()

def test_gen_random(distpopup):
    distpopup.show()
    distpopup.gen_random("foo", "bar", [[1, 2, 3]])
    distpopup.close()

def test_gen_cubic(distpopup):
    distpopup.show()
    distpopup.gen_cubic("foo", "bar", 123, [11, 22, 33, 44, 55, 66])
    distpopup.close()

def test_gen_bcc(distpopup):
    distpopup.show()
    distpopup.gen_bcc("foo", "bar", 123, [11, 22, 33, 44, 55, 66])
    distpopup.close()

def test_distribute(distpopup):
    distpopup.show()
    distpopup.distribute("foo", "bar", [[1, 2, 3]])
    distpopup.close()

def test_points_in_geo(distpopup):
    distpopup.show()
    distpopup.points_in_geo("foo", "bar")
    distpopup.close()
