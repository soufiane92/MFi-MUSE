# -*- coding: utf-8 -*-
"""
run with pytest
"""

import pytest

import mfixgui.solids_pic

@pytest.fixture
def solids_pic(mocker):
    solids_pic = mfixgui.solids_pic.SolidsPIC()
    solids_pic.fluid_solver_disabled = None
    solids_pic.project = None
    solids_pic.ui = None
    solids_pic.unset_keyword = None
    solids_pic.update_keyword = None
    solids_pic.warn = None
    solids_pic.warning = None
    mocker.patch.object(solids_pic, 'fluid_solver_disabled')
    mocker.patch.object(solids_pic, 'ui')
    mocker.patch.object(solids_pic, 'unset_keyword')
    mocker.patch.object(solids_pic, 'update_keyword')
    mocker.patch.object(solids_pic, 'warn')
    mocker.patch.object(solids_pic, 'warning')
    project = mocker.patch.object(solids_pic, 'project')
    project.get_value.return_value = 1
    return solids_pic

def test_des_interp_2(solids_pic, qtbot):
    solids_pic.set_des_interp_2(123)

def test_des_interp_scheme_2(solids_pic, qtbot):
    solids_pic.set_des_interp_scheme_2(0)
    solids_pic.set_des_interp_scheme_2(1)
    solids_pic.set_des_interp_scheme_2(2)

def test_set_coupling_method_2(solids_pic, qtbot):
    solids_pic.set_coupling_method_2(0)
    solids_pic.set_coupling_method_2(1)
