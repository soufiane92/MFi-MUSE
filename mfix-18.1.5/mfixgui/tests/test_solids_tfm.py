# -*- coding: utf-8 -*-
"""
run with pytest
"""

import pytest

import mfixgui.solids_tfm


@pytest.fixture
def solids_tfm(mocker):
    app = mocker.Mock()
    solids_tfm = mfixgui.gui.MfixGui(app)
    solids_tfm.project = None
    solids_tfm.ui = None
    solids_tfm.unset_keyword = None
    solids_tfm.update_keyword = None
    solids_tfm.warn = None
    mocker.patch.object(solids_tfm, 'ui')
    mocker.patch.object(solids_tfm, 'unset_keyword')
    mocker.patch.object(solids_tfm, 'update_keyword')
    mocker.patch.object(solids_tfm, 'warn')
    project = mocker.patch.object(solids_tfm, 'project')
    project.get_value.return_value = 1
    return solids_tfm

def test_set_kt_type(solids_tfm, qtbot):
    solids_tfm.set_kt_type(0)
    solids_tfm.set_kt_type(1)
    solids_tfm.set_kt_type(2)
    solids_tfm.set_kt_type(3)
    solids_tfm.set_kt_type(4)
    solids_tfm.set_kt_type(5)
    solids_tfm.set_kt_type(6)
    solids_tfm.set_kt_type(7)

def test_set_friction_model(solids_tfm, qtbot):
    solids_tfm.set_friction_model(0)
    solids_tfm.set_friction_model(1)
    solids_tfm.set_friction_model(2)

def test_set_rdf_type(solids_tfm, qtbot):
    solids_tfm.set_rdf_type(0)
    solids_tfm.set_rdf_type(1)
    solids_tfm.set_rdf_type(2)
    solids_tfm.set_rdf_type(3)
    solids_tfm.set_rdf_type(4)

def test_set_blending_function(solids_tfm, qtbot):
    solids_tfm.set_blending_function(0)
    solids_tfm.set_blending_function(1)
    solids_tfm.set_blending_function(2)

def test_set_max_packing_correlation(solids_tfm, qtbot):
    solids_tfm.set_max_packing_correlation(0)
    solids_tfm.set_max_packing_correlation(1)
    solids_tfm.set_max_packing_correlation(2)
    solids_tfm.set_max_packing_correlation(3)
    solids_tfm.set_max_packing_correlation(123)
