# -*- coding: utf-8 -*-
"""
run with pytest
"""

import pytest

import mfixgui.solids_dem

@pytest.fixture
def solids_dem(mocker):
    solids_dem = mfixgui.solids_dem.SolidsDEM()
    solids_dem.add_tooltip = None
    solids_dem.fixup_solids_table = None
    solids_dem.fluid_solver_disabled = None
    solids_dem.project = None
    solids_dem.solids = {'foo': {'diameter': 1}}
    solids_dem.solids_dem_saved_solids_names = None
    solids_dem.ui = None
    solids_dem.unset_keyword = None
    solids_dem.update_keyword = None
    solids_dem.warn = None
    solids_dem.warning = None
    mocker.patch.object(solids_dem, 'add_tooltip')
    mocker.patch.object(solids_dem, 'fixup_solids_table')
    mocker.patch.object(solids_dem, 'fluid_solver_disabled')
    mocker.patch.object(solids_dem, 'solids_dem_saved_solids_names')
    mocker.patch.object(solids_dem, 'ui')
    mocker.patch.object(solids_dem, 'unset_keyword')
    mocker.patch.object(solids_dem, 'update_keyword')
    mocker.patch.object(solids_dem, 'warn')
    mocker.patch.object(solids_dem, 'warning')
    project = mocker.patch.object(solids_dem, 'project')
    project.get_value.return_value = 1
    return solids_dem


def test_set_gener_part_config(solids_dem, qtbot):
    """ run TestSolidsDem.default """
    solids_dem.set_gener_part_config(None)
    solids_dem.set_gener_part_config(False)
    solids_dem.set_gener_part_config(123)

def test_set_des_intg_method(solids_dem, qtbot):
    """ run TestSolidsDem.default """
    solids_dem.set_des_intg_method(0)
    solids_dem.set_des_intg_method(1)

def test_set_des_coll_model(solids_dem, qtbot):
    """ run TestSolidsDem.default """
    solids_dem.set_des_coll_model(0)
    solids_dem.set_des_coll_model(1)

def test_set_coupling_method(solids_dem, qtbot):
    """ run TestSolidsDem.default """
    solids_dem.set_coupling_method(0)
    solids_dem.set_coupling_method(1)

def test_set_des_interp(solids_dem, qtbot):
    """ run TestSolidsDem.default """
    solids_dem.set_des_interp(False)
    solids_dem.set_des_interp(123)

def test_set_des_interp_scheme(solids_dem, qtbot):
    """ run TestSolidsDem.default """
    solids_dem.set_des_interp_scheme(0)
    solids_dem.set_des_interp_scheme(1)
    solids_dem.set_des_interp_scheme(2)

def test_enable_des_diffuse_width(solids_dem, qtbot):
    """ run TestSolidsDem.default """
    solids_dem.enable_des_diffuse_width(False)
    solids_dem.enable_des_diffuse_width(123)

def test_set_cohesion_model(solids_dem, qtbot):
    """ run TestSolidsDem.default """
    solids_dem.set_cohesion_model(None)
    solids_dem.set_cohesion_model(123)

def test_enable_des_usr_var_size(solids_dem, qtbot):
    """ run TestSolidsDem.default """
    solids_dem.enable_des_usr_var_size(None)
    solids_dem.enable_des_usr_var_size(0)
    solids_dem.enable_des_usr_var_size(1)
    solids_dem.enable_des_usr_var_size(123)

def test_set_des_neighbor_search(solids_dem, qtbot):
    """ run TestSolidsDem.default """
    solids_dem.set_des_neighbor_search(None)
    solids_dem.set_des_neighbor_search(0)
    solids_dem.set_des_neighbor_search(1)
    solids_dem.set_des_neighbor_search(123)
