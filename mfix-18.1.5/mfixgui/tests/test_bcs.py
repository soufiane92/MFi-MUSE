# -*- coding: utf-8 -*-
"""
run with pytest
"""

import pytest

from mfixgui.bcs import FLUID_TAB, SOLIDS_TAB, SCALAR_TAB, CYCLIC_TAB, BCS
from mfixgui.project import Keyword

import mfixgui.bcs

@pytest.fixture
def bcs_pane(mocker):
    bcs = mfixgui.bcs.BCS()
    bcs.add_tooltip = None
    bcs.bcs = {}
    bcs.bcs_current_indices = [1]
    bcs.bcs_current_solid = None
    bcs.find_navigation_tree_item = None
    bcs.find_species_phase = None
    bcs.fluid_solver_disabled = None
    bcs.project = None
    bcs.reaction_edited = None
    bcs.solids_species = None
    bcs.solids = {'foo': {'diameter': 1} }
    bcs.bcs_saved_solids_names = None
    bcs.bcs_region_dict = None
    bcs.species_all_aliases = None
    bcs.animate_stacked_widget = None
    bcs.ui = None
    bcs.unset_keyword = None
    bcs.update_keyword = None
    bcs.warn = None
    bcs.warning = None
    bcs.working_reaction = None
    mocker.patch.object(bcs, 'add_tooltip')
    # mocker.patch.object(bcs, 'bcs')
    mocker.patch.object(bcs, 'bcs_current_solid')
    mocker.patch.object(bcs, 'bcs_saved_solids_names')
    mocker.patch.object(bcs, 'bcs_region_dict')
    mocker.patch.object(bcs, 'find_navigation_tree_item')
    mocker.patch.object(bcs, 'find_species_phase')
    mocker.patch.object(bcs, 'fluid_solver_disabled')
    mocker.patch.object(bcs, 'reaction_edited')
    mocker.patch.object(bcs, 'animate_stacked_widget')
    mocker.patch.object(bcs, 'species_all_aliases')
    mocker.patch.object(bcs, 'solids_species')
    mocker.patch.object(bcs, 'unset_keyword')
    mocker.patch.object(bcs, 'update_keyword')
    mocker.patch.object(bcs, 'warn')
    mocker.patch.object(bcs, 'warning')
    mocker.patch.object(bcs, 'working_reaction')
    ui = mocker.patch.object(bcs, 'ui')
    ui.boundary_conditions.tablewidget_regions.rowCount.return_value = 1
    project = mocker.patch.object(bcs, 'project')

    project.get_value.side_effect = lambda *a, **kw: {
        'bc_jj_m': 123,
        'bc_jj_ps': '123',
        'bc_type': 'W',
        'jenkins': 123,
        'mmax': 123,
        'nmax_s': 123,
        'nscalar': 123,
    }.get(a[0])
    bcs.bcs_current_tab = FLUID_TAB
    return bcs

def test_setup_bcs(bcs_pane, qtbot):
    bcs_pane.fluid_phase_name = "foo_fluid"
    bcs_pane.setup_bcs()

def test_bcs_extract_regions(bcs_pane, qtbot):
    bcs_pane.bcs_extract_regions()
