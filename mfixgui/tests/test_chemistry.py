# -*- coding: utf-8 -*-
"""
run with pytest
"""

import pytest

import mfixgui.chemistry

@pytest.fixture
def chemistry_pane(mocker):
    chemistry = mfixgui.chemistry.Chemistry()
    chemistry.add_tooltip = None
    chemistry.current_reaction_name = None
    chemistry.find_navigation_tree_item = None
    chemistry.find_species_phase = None
    chemistry.project = None
    chemistry.reaction_edited = None
    chemistry.species_all_aliases = None
    chemistry.ui = None
    chemistry.unset_keyword = None
    chemistry.update_keyword = None
    chemistry.warning = None
    chemistry.working_reaction = None
    mocker.patch.object(chemistry, 'add_tooltip')
    mocker.patch.object(chemistry, 'current_reaction_name')
    mocker.patch.object(chemistry, 'find_navigation_tree_item')
    mocker.patch.object(chemistry, 'find_species_phase')
    mocker.patch.object(chemistry, 'reaction_edited')
    mocker.patch.object(chemistry, 'species_all_aliases')
    mocker.patch.object(chemistry, 'unset_keyword')
    mocker.patch.object(chemistry, 'update_keyword')
    mocker.patch.object(chemistry, 'warning')
    mocker.patch.object(chemistry, 'working_reaction')
    ui = mocker.patch.object(chemistry, 'ui')
    ui.chemistry.tablewidget_reactants.rowCount.return_value = 1
    ui.chemistry.tablewidget_products.rowCount.return_value = 1
    project = mocker.patch.object(chemistry, 'project')
    project.get_value.return_value = 1
    return chemistry

def test_handle_dh_checkbox(chemistry_pane, qtbot):
    chemistry_pane.working_reaction = {}
    chemistry_pane.handle_dh_checkbox(True)
    chemistry_pane.handle_dh_checkbox(False)

def test_handle_dh(mocker, chemistry_pane, qtbot):
    chemistry_pane.working_reaction = {}
    chemistry_pane.handle_dh(mocker.Mock(), {'dh': None}, mocker.Mock())

def test_set_reaction_name(chemistry_pane, qtbot):
    chemistry_pane.set_reaction_name()

def test_chemistry_restrict_species(chemistry_pane, qtbot):
    chemistry_pane.chemistry_restrict_species()

def test_chemistry_handle_selection(chemistry_pane, qtbot):
    chemistry_pane.chemistry_handle_selection()

def test_format_chem_eq(chemistry_pane, qtbot):
    reactants = (('foo species', 123), )
    products = (('bar species', 456), )
    chem_eq = chemistry_pane.format_chem_eq(reactants, products)
    assert chem_eq == '123*foo species --> 456*bar species'

def test_chemistry_update_chem_eq(chemistry_pane, qtbot):
    reactants = (('foo species', 123), )
    chemistry_pane.project.reactants.get.return_value = reactants
    chemistry_pane.chemistry_update_chem_eq('foo species')

def test_chemistry_update_totals(chemistry_pane, qtbot):
    chemistry_pane.chemistry_update_totals()

def test_chemistry_handle_reactant_selection(chemistry_pane, qtbot):
    chemistry_pane.chemistry_handle_reactant_selection()

def test_chemistry_handle_product_selection(chemistry_pane, qtbot):
    chemistry_pane.chemistry_handle_product_selection()

def test_chemistry_update_enabled(chemistry_pane, qtbot):
    chemistry_pane.fluid_species = []
    chemistry_pane.solids_species = {}
    chemistry_pane.solids = []
    chemistry_pane.chemistry_update_enabled()

def test_chemistry_add_reaction(chemistry_pane, qtbot):
    chemistry_pane.chemistry_add_reaction()

def test_chemistry_delete_reaction(chemistry_pane, qtbot):
    chemistry_pane.chemistry_delete_reaction()

def test_chemistry_appy_changes(chemistry_pane, qtbot):
    chemistry_pane.chemistry_apply_changes()

def test_chemistry_add_reactant(chemistry_pane, qtbot):
    chemistry_pane.chemistry_add_reactant()

def test_chemistry_delete_reactant(chemistry_pane, qtbot):
    chemistry_pane.chemistry_delete_reactant()

def test_chemistry_add_product(chemistry_pane, qtbot):
    chemistry_pane.chemistry_add_product()

def test_chemistry_delete_product(chemistry_pane, qtbot):
    chemistry_pane.chemistry_delete_product()

def test_chemistry_species_in_use(chemistry_pane, qtbot):
    chemistry_pane.project.reactions.items.return_value = []
    chemistry_pane.chemistry_check_species_in_use(None)

def test_chemistry_rename_species(chemistry_pane, qtbot):
    chemistry_pane.project.reactions.items.return_value = []
    chemistry_pane.chemistry_rename_species('foo', 'bar')
