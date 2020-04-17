# -*- coding: utf-8 -*-
"""
run with pytest
"""

import pytest

import mfixgui.ics

@pytest.fixture
def ics_pane(mocker):
    ics = mfixgui.ics.ICS()
    ics.add_tooltip = None
    ics.fluid_solver_disabled = None
    ics.ics = {}
    ics.ics_region_dict = None
    ics.project = None
    ics.solids = {'foo': {'diameter': 1}}
    ics.solids_species = None
    ics.ui = None
    ics.update_keyword = None
    mocker.patch.object(ics, 'add_tooltip')
    mocker.patch.object(ics, 'ics_region_dict')
    mocker.patch.object(ics, 'solids_species')
    mocker.patch.object(ics, 'update_keyword')
    ui = mocker.patch.object(ics, 'ui')
    ui.initial_conditions.tablewidget_regions.rowCount.return_value = 1
    project = mocker.patch.object(ics, 'project')

    project.get_value.side_effect = lambda *a, **kw: {
        'nscalar': 123,
        'ic_x_g': 123,
    }.get(a[0])
    return ics


def test_ics_extract_regions(ics_pane, qtbot):
    ics_pane.ics_extract_regions()
