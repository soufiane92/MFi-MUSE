# -*- coding: utf-8 -*-
"""
run with pytest
"""

import pytest

import mfixgui.numerics

@pytest.fixture
def numerics_pane(mocker):
    numerics = mfixgui.numerics.Numerics()
    numerics.add_tooltip = None
    numerics.project = None
    numerics.ui = None
    numerics.unset_keyword = None
    numerics.update_keyword = None
    numerics.warning = None
    mocker.patch.object(numerics, 'add_tooltip')
    mocker.patch.object(numerics, 'ui')
    mocker.patch.object(numerics, 'unset_keyword')
    mocker.patch.object(numerics, 'update_keyword')
    mocker.patch.object(numerics, 'warning')
    project = mocker.patch.object(numerics, 'project')
    project.get_value.return_value = 1
    numerics.numerics_current_tab = mfixgui.numerics.TAB_RESIDUALS
    return numerics

def test_set_cn_on(numerics_pane, qtbot):
    numerics_pane.set_cn_on(0)
    numerics_pane.set_cn_on(1)

def test_set_discretize(numerics_pane, qtbot):
    for i in range(2):
        for j in range(1, 11):
            numerics_pane.set_discretize(i, j)

def test_set_leq_method(numerics_pane, qtbot):
    for i in range(2):
        for j in range(1, 11):
            numerics_pane.set_leq_method(i, j)

def test_set_preconditioner(numerics_pane, qtbot):

    for i in range(3):
        numerics_pane.set_preconditioner(i, 123)

def test_set_sweep(numerics_pane, qtbot):
    for i in range(2):
        for j in range(1, 11):
            numerics_pane.set_sweep(i, j)

def test_numerics_setup_residuals_tab(numerics_pane, qtbot):
    numerics_pane.numerics_setup_residuals_tab()

def test_numerics_setup_discretization_tab(numerics_pane, qtbot):
    numerics_pane.numerics_setup_discretization_tab()

def test_numerics_setup_linear_solver_tab(numerics_pane, qtbot):
    numerics_pane.numerics_setup_linear_solver_tab()

def test_numerics_setup_preconditioner_tab(numerics_pane, qtbot):
    numerics_pane.numerics_setup_preconditioner_tab()

def test_numerics_setup_advanced_tab(numerics_pane, qtbot):
    numerics_pane.numerics_setup_advanced_tab()
