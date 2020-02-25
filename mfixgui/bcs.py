# -*- coding: utf-8 -*-
"""Boundary Conditions pane"""

from collections import OrderedDict

from qtpy.QtCore import Qt
from qtpy.QtWidgets import (QGridLayout, QGroupBox, QHeaderView,
                            QHBoxLayout, QLabel, QPushButton, QWidget, QTableWidgetItem)
from qtpy.QtGui import QPicture
UserRole = Qt.UserRole

from mfixgui.constants import *
from mfixgui.widgets.base import LineEdit, ComboBox

from mfixgui.project import FloatExp

from mfixgui.tools import safe_float
from mfixgui.tools.qt import (set_item_noedit, set_item_enabled,
                           get_combobox_item, get_selected_row,
                           widget_iter, sub_icon_height)

from mfixgui.tools.keyword_args import mkargs, keys_by_type

# We don't need extended JSON here
from json import JSONDecoder, JSONEncoder

FLUID_TAB = 0
SOLIDS_TAB_DUMMY_L = 1
SOLIDS_TAB = 2
SOLIDS_TAB_DUMMY_R = 3
SCALAR_TAB = 4
CYCLIC_TAB = 5

PAGE_WALL, PAGE_INFLOW, PAGE_PO, PAGE_MO = range(4) #page indices in stackedwidget

AXIAL, VOLFLOW, MASSFLOW = range(3)

SPECIFIED_MASS_FRACTION = 1

# Columns in tablewidget_regions
COLUMN_REGION, COLUMN_TYPE, COLUMN_ID = range(3)

# map xyz shown in labels to uvw used in key names
xmap = {'X':'u', 'Y':'v', 'Z': 'w'}

class BCS(object):
    #Boundary Conditions Task Pane Window: This section allows a user to define the boundary
    #conditions for the described model. This section relies on regions named in the Regions section.

    def init_bcs(self):
        ui = self.ui.boundary_conditions

        self.bcs = {} # key: index.  value: data dictionary for boundary cond
        self.bcs_current_indices = [] # List of BC indices
        self.bcs_current_regions = [] # And the names of the regions which define them
        self.bcs_region_dict = None
        self.bcs_saved_solids_names = [] # track changes in solids phase names
        self.bcs_fluid_species_boxes = OrderedDict() # In the 'wall' tab each species needs a groupbox

        # The top of the task pane is where users define/select BC regions
        # (see handle_bcs_region_selection)
        #
        #Icons to add/remove/duplicate boundary conditions are given at the top
        #Clicking the 'add' and 'duplicate' buttons triggers a popup window where the user must select
        #a region to apply the boundary condition.
        ui.toolbutton_add.clicked.connect(self.bcs_show_regions_popup)
        ui.toolbutton_delete.clicked.connect(self.bcs_delete_regions)
        # Up/down buttons
        ui.toolbutton_up.clicked.connect(self.bcs_row_up)
        ui.toolbutton_down.clicked.connect(self.bcs_row_down)

        # TODO implement 'duplicate' (what does this do?)
        for tb in (ui.toolbutton_delete, ui.toolbutton_up, ui.toolbutton_down):
            tb.setEnabled(False) # Need a selection
        ui.tablewidget_regions.itemSelectionChanged.connect(self.handle_bcs_region_selection)

        self.bcs_current_tab = FLUID_TAB # #  If fluid is disabled, we will switch
        self.bcs_current_solid = self.P = None
        ui.pushbutton_fluid.pressed.connect(lambda: self.bcs_change_tab(FLUID_TAB,None))
        ui.pushbutton_scalar.pressed.connect(lambda: self.bcs_change_tab(SCALAR_TAB,None))
        ui.pushbutton_cyclic.pressed.connect(lambda: self.bcs_change_tab(CYCLIC_TAB,None))

        # Trim width of "Fluid" and "Scalar" and "Cyclic" buttons, like we do for
        # dynamically-created "Solid #" buttons
        for b in (ui.pushbutton_fluid, ui.pushbutton_scalar, ui.pushbutton_cyclic):
            w = b.fontMetrics().boundingRect(b.text()).width() + 20
            b.setMaximumWidth(w)

        # Checkbox callbacks
        cb = ui.checkbox_bc_jj_ps_args_BC
        cb.dtype = int
        cb.clicked.connect(self.set_bc_jj_ps)
        cb.key = 'bc_jj_ps'
        cb.args = ['BC']
        self.add_tooltip(cb, 'bc_jj_ps')

        cb = ui.checkbox_cyclic_pd
        cb.clicked.connect(self.set_cyclic_pd)
        self.add_tooltip(cb, 'cyclic_x_pd')

        cb = ui.checkbox_flux_g
        cb.clicked.connect(self.enable_flux_g)
        self.add_tooltip(cb, 'flux_g')

        # Combobox callbacks
        for name in ('fluid_energy_eq', #'fluid_species_eq' (created below per-species),
                     'bc_jj_ps',
                     'solids_energy_eq', 'solids_granular_energy_eq'): # no solids_species_eq?
            item = getattr(ui, 'combobox_%s_type' % name)
            setter = getattr(self, 'set_bcs_%s_type' % name)
            item.currentIndexChanged.connect(setter)

        # Tangential velocities are not auto-registered
        for suffix in ('1', '2', '1_3', '2_3'):
            widget = getattr(ui, 'lineedit_fluid_velocity_'+suffix)
            widget.value_updated.connect(self.project.submit_change)
        for suffix in ('1', '2', '1_4', '2_4'):
            widget = getattr(ui, 'lineedit_solids_velocity_'+suffix)
            widget.value_updated.connect(self.project.submit_change)

        # Inflow/outflow pane has some special inputs
        for phase_type in 'fluid', 'solids': # type
            for flow_direction in 'inflow', 'outflow': #direction
                #le = ui.lineedit_fluid_inflow
                le = getattr(ui, 'lineedit_%s_%s' % (phase_type, flow_direction), None)
                le.value_updated.connect(self.bcs_handle_flow_input)
                le.key = 'Unset' # diagnostic
                le.args = ['BC'] if phase_type=='fluid' else ['BC', 'P']
                le.dtype = float
                le.allow_parameters = True
                # Tooltip added dynamically
                cb = getattr(ui, 'combobox_%s_%s_type' % (phase_type, flow_direction))
                cb.value_updated.connect(self.bcs_handle_flow_type)
                cb.key = '%s_%s' % (phase_type, flow_direction)
                cb.help_text = 'Select %s %s specification type' % (phase_type, flow_direction)
                cb.setToolTip(cb.help_text)

        # Not auto-registered with project manager
        key = 'bc_ep_s'
        for widget in (ui.lineedit_bc_ep_s_args_BC_P, ui.lineedit_bc_ep_s_2_args_BC_P):
            widget.value_updated.connect(self.handle_bcs_volume_fraction)
            widget.key = key
            widget.args = ['BC', 'P']
            widget.dtype = float
            widget.allow_parameters = True
            self.add_tooltip(widget, key)

        key = 'bc_dt_0'
        for widget in (ui.lineedit_bc_dt_0, ui.lineedit_bc_dt_0_4):
            widget.value_updated.connect(self.handle_bc_dt_0)
            widget.key = key
            widget.args = ['BC']
            widget.dtype = float
            widget.allow_parameters = True
            self.add_tooltip(widget, key)
            # Error Check: Value should be positive (non-negative)
            widget.min = 0

        le = ui.lineedit_delp
        le.dtype = float
        le.allow_parameters = True
        le.value_updated.connect(self.project.submit_change)

        cb = ui.combobox_bc_type
        for i in range(len(cb)):
            self.add_tooltip(get_combobox_item(cb, i), key='bc_type', value=BC_TYPES[i])
        self.add_tooltip(get_combobox_item(cb, BC_TYPES.index('CYCLIC')),
                        key=None, description='Cyclic boundary condition')

        cb.activated.connect(self.change_bc_type)

        # enable parameters
        for item in widget_iter(ui):
            if isinstance(item, LineEdit) and item.dtype in (int, float):
                item.allow_parameters = True


    def change_bc_type(self, idx):
        if not self.bcs_current_indices:
            return
        BC0 = self.bcs_current_indices[0]
        ui = self.ui.boundary_conditions
        row = get_selected_row(ui.tablewidget_regions)
        if row is None:
            return
        new_type = BC_TYPES[idx]

        if new_type == 'CYCLIC': # nothing to do
            return
        old_type = self.project.get_value('bc_type', args=[BC0])
        if old_type.startswith('CG_'):
            old_type = old_type[3:]
        if new_type == old_type:
            return
        new_name = BC_NAMES[idx]
        item = QTableWidgetItem(new_name)
        set_item_noedit(item)
        ui.tablewidget_regions.setItem(row, COLUMN_TYPE, item)
        for (BC, r) in zip(self.bcs_current_indices, self.bcs_current_regions):
            prefix = 'CG_' if self.bcs_region_dict[r]['type'] == 'STL' else ''
            self.update_keyword('bc_type', prefix+new_type, args=[BC])
            for kw in list(self.project.keywordItems()):
                if kw.key.startswith('bc_') and kw.args and kw.args[0]==BC:
                    if kw.key not in ('bc_type',
                                      'bc_x_e', 'bc_x_w',
                                      'bc_y_s', 'bc_y_n',
                                      'bc_z_b', 'bc_z_t'):
                        self.unset_keyword(kw.key, args=kw.args)
            self.bcs_set_default_keys(BC)

        self.bcs_setup_current_tab()
        # New tab may be smaller than prev so scroll to top
        ui.scrollarea_detail.ensureVisible(0, 0)

    def bcs_set_volume_fraction_limit(self):
        # Set bc_ep_g from bc_ep_s, like we do for ICs
        if not self.bcs_current_indices:
            return
        if not self.bcs_current_solid:
            return
        BC0 = self.bcs_current_indices[0]
        P = self.bcs_current_solid
        ui = self.ui.boundary_conditions
        key = 'bc_ep_s'

        total = sum(safe_float(self.project.get_value(key, default=0, args=[BC0, i]))
                    for i in range(1, len(self.solids)+1) if i != P)

        lim = max(0, 1.0 - total)
        lim = round(lim, 10) # avoid problem with 1 - 0.9 != 0.1

        for widget in (ui.lineedit_bc_ep_s_args_BC_P, ui.lineedit_bc_ep_s_2_args_BC_P):
            widget.min = 0.0
            widget.max = lim


    def handle_bcs_volume_fraction(self, widget, val, args):
        if not self.bcs_current_indices:
            return
        BC0 = self.bcs_current_indices[0]
        if not self.bcs_current_solid:
            return
        key = 'bc_ep_s'
        self.project.submit_change(widget, val, args)

        total = sum(safe_float(self.project.get_value(key, default=0, args=[BC0, i]))
                for i in range(1, len(self.solids)+1))
        if total > 1.0:
            self.warning("Volume fractions sum to %s, must be &lt;= 1.0" % total,
                         popup=True)
            return # ?

        # set bc_ep_g from bc_ep_s (issues/121)
        val = round(1.0 - total, 10)
        for BC in self.bcs_current_indices:
            self.update_keyword('bc_ep_g', val, args=[BC])


    def bcs_show_regions_popup(self):
        # Users cannot select inapplicable regions.
        # BC regions must be planes, volumes, or STLs (not volumes or points)
        # No region can define more than one boundary condition.
        ui = self.ui.boundary_conditions
        rp = self.regions_popup
        rp.clear()
        for (name,data) in self.bcs_region_dict.items():
            shape = data.get('type', '---')
            available = (data.get('available', True)
                         and (shape in ('STL', 'box')
                              or 'plane' in shape))
            row = (name, shape, available)
            rp.add_row(row)
        for axis in 'X', 'Y', 'Z':
            name = axis + '-cyclic'
            shape = 'cyclic'
            available = not bool(self.project.get_value('cyclic_%s' % axis)) # (already in use)
            if axis == 'Z' and self.project.get_value('no_k'):
                available = False
            row = (name, shape, available)
            rp.add_row(row)

        rp.reset_signals()
        rp.save.connect(self.bcs_add_regions)
        rp.cancel.connect(self.bcs_cancel_add)
        for item in (ui.tablewidget_regions,
                     ui.bottom_frame,
                     ui.toolbutton_add,
                     ui.toolbutton_delete,
                     ui.toolbutton_up,
                     ui.toolbutton_down):
            item.setEnabled(False)
        rp.popup('Select region(s) for boundary conditions')


    def bcs_cancel_add(self):
        ui = self.ui.boundary_conditions
        tw = ui.tablewidget_regions
        for item in (ui.toolbutton_add,
                     ui.tablewidget_regions):
            item.setEnabled(True)
        row = get_selected_row(tw)
        nrows = ui.tablewidget_regions.rowCount()
        if row is not None:
            for item in (ui.bottom_frame,
                         ui.toolbutton_delete):
                item.setEnabled(True)
            cyclic = self.bc_is_cyclic(tw.item(row, 0).data(UserRole)[0][0])
            prev_cyclic = row > 0 and self.bc_is_cyclic(tw.item(row-1, 0).data(UserRole)[0][0])
            ui.toolbutton_up.setEnabled(row > 0 and not cyclic and not prev_cyclic)
            ui.toolbutton_down.setEnabled(not cyclic and row != nrows-1)
        else:
            ui.toolbutton_up.setEnabled(False)
            ui.toolbutton_down.setEnabled(False)

    def bc_is_cyclic(self, idx):
        # we use pseudo-indices to represent the cyclic pseudo-regions
        return 'cyclic' in str(idx).lower()

    def get_bc_type(self, idx):
        return 'CYCLIC' if self.bc_is_cyclic(idx) else self.project.get_value('bc_type', args=[idx])

    def bcs_add_regions(self):
        # Interactively add regions to define BCs
        # DEFAULT - No slip wall
        # Error check: mass fractions must sum to 1.0
        # (selection logic implemented in regions_popup.py)
        rp = self.regions_popup
        self.bcs_cancel_add() # Reenable input widgets
        selections = rp.get_selection_list()
        if not selections:
            return
        bc_type = BC_TYPES[rp.combobox.currentIndex()]

        # Check if region is STL.
        if self.bcs_region_dict is None:
            self.bcs_region_dict = self.ui.regions.get_region_dict()
        # Note, there may be no entry in region_dict (eg cyclic boundary)
        shape = self.bcs_region_dict.get(selections[0], {}).get('type', '')
        if shape == 'STL':
            bc_type = 'CG_' + bc_type
        if bc_type == 'CG_PI': # Shouldn't happen!
            self.error("Invalid bc_type %s" % bc_type)
            return

        self.bcs_add_regions_1(selections, bc_type=bc_type, indices=None, autoselect=True)
        self.bcs_setup_current_tab() # Update the widgets


    def bcs_add_regions_1(self, selections,
                          bc_type=None, indices=None, autoselect=False):
        # Used by both interactive and load-time add-region handlers
        if bc_type is None:
            self.error('Type not defined for boundary condition %s' % '+'.join(selections))

        if self.bcs_region_dict is None:
            self.bcs_region_dict = self.ui.regions.get_region_dict()

        ui = self.ui.boundary_conditions
        tw = ui.tablewidget_regions
        nrows = tw.rowCount()

        def make_item(val):
            item = QTableWidgetItem('' if val is None else str(val))
            set_item_noedit(item)
            return item
        item = make_item('+'.join(selections))

        if indices is None: # interactive
            indices = [None] * len(selections)
        else: # loading file
            assert len(selections) == len(indices)
        if bc_type == 'CYCLIC':
            region_name = selections[0]
            axis = region_name[0].lower()
            key = 'cyclic_%s' % axis
            self.update_keyword(key, True)
            indices = selections #!  string pseudo-indices for pseudo-regions
            cyclic_x = self.project.get_value('cyclic_x', default=False)
            cyclic_y = self.project.get_value('cyclic_y', default=False)
            cyclic_z = self.project.get_value('cyclic_z', default=False)
            if axis == 'x':
                new_row = 0
            elif axis == 'y':
                new_row = int(cyclic_x)
            elif axis == 'z':
                new_row = int(cyclic_x + cyclic_y)

        else: # Non-cyclic
            new_row = nrows
            for (i, region_name) in enumerate(selections):
                idx = indices[i]
                if idx is None:
                    idx = self.bcs_find_index()
                    indices[i] = idx
                self.bcs[idx] = {'region': region_name}

                region_data = self.bcs_region_dict.get(region_name)
                if region_data is None: # ?
                    self.warn("no data for region %s bc_type %s" % (region_name, bc_type))
                    continue
                self.bcs_set_region_keys(region_name, idx, region_data, bc_type)
                self.bcs_region_dict[region_name]['available'] = False # Mark as in-use

        tw.insertRow(new_row)
        item.setData(UserRole, (tuple(indices), tuple(selections)))
        tw.setItem(new_row, COLUMN_REGION, item)

        if bc_type != 'CYCLIC':
            item = make_item(','.join(map(str, indices)))
            tw.setItem(new_row, COLUMN_ID, item)

        # remove prefix so we can construct the long name
        tmp = bc_type
        if tmp.startswith('CG_'):
            tmp = tmp[3:]
        name = BC_NAMES[BC_TYPES.index(tmp)]
        item = make_item(name)
        tw.setItem(new_row, COLUMN_TYPE, item)

        self.fixup_bcs_table(tw)

        if autoselect:
            tw.setCurrentCell(new_row, 0)

        # Issues/183
        self.bcs_check_wall_keys([i for i in indices if not self.bc_is_cyclic(i)])
        # Issues/513
        for i in indices:
            self.bcs_set_default_keys(i)


    def bcs_find_index(self):
        # Always add new BC at end, do not fill holes
        return 1 if not self.bcs else 1 + max(self.bcs)


    def bcs_row_up(self):
        ui = self.ui.boundary_conditions
        tw = ui.tablewidget_regions
        row = get_selected_row(tw)
        if row is None: # No selection
            return
        if row == 0: # Cannot move up
            return
        row_1 = row-1 # Previous row
        (indices, regions) = tw.item(row,COLUMN_REGION).data(UserRole)
        (indices_1, regions_1) = tw.item(row_1,COLUMN_REGION).data(UserRole)
        self.bcs_permute(indices_1+indices, indices+indices_1)
        for col in (COLUMN_REGION, COLUMN_TYPE): # Just swap the table items
            tmp = tw.takeItem(row_1, col)
            tw.setItem(row_1, col, tw.takeItem(row, col))
            tw.setItem(row, col, tmp)

        # i1, i            i1,  i
        # 5, (6, 7)   ->   (5, 6), 7
        itup = tuple(indices_1) + tuple(indices)
        ilen = len(indices)
        indices_1, indices = itup[:ilen], itup[ilen:]
        regions_1, regions = tuple(regions), tuple(regions_1)
        tw.item(row, COLUMN_REGION).setData(UserRole, (indices, regions))
        tw.item(row_1, COLUMN_REGION).setData(UserRole, (indices_1, regions_1))

        def make_item(val):
            item = QTableWidgetItem('' if val is None else str(val))
            set_item_noedit(item)
            return item
        item = make_item(','.join(map(str, indices)))
        tw.setItem(row, COLUMN_ID, item)
        item = make_item(','.join(map(str, indices_1)))
        tw.setItem(row_1, COLUMN_ID, item)

        tw.setCurrentCell(row_1, COLUMN_REGION) # Select the moved row


    def bcs_row_down(self):
        ui = self.ui.boundary_conditions
        tw = ui.tablewidget_regions
        row = get_selected_row(tw)
        if row is None: # No selection
            return
        if row >= tw.rowCount() -1: # Cannot move down
            return
        row_1 = row+1 # Next row
        (indices, regions) = tw.item(row,COLUMN_REGION).data(UserRole)
        (indices_1, regions_1) = tw.item(row_1,COLUMN_REGION).data(UserRole)
        self.bcs_permute(indices+indices_1, indices_1+indices)
        for col in (COLUMN_REGION, COLUMN_TYPE): # Just swap the table items
            tmp = tw.takeItem(row_1, col)
            tw.setItem(row_1, col, tw.takeItem(row, col))
            tw.setItem(row, col, tmp)

        # i, i1        i,    i1
        # 5, (6, 7) -> (5,6), 7
        itup = tuple(indices) + tuple(indices_1)
        ilen = len(indices_1)
        indices, indices_1 = itup[:ilen], itup[ilen:]
        regions, regions_1 = tuple(regions_1), tuple(regions)
        tw.item(row, COLUMN_REGION).setData(UserRole, (indices, regions))
        tw.item(row_1, COLUMN_REGION).setData(UserRole, (indices_1, regions_1))
        def make_item(val):
            item = QTableWidgetItem('' if val is None else str(val))
            set_item_noedit(item)
            return item
        item = make_item(','.join(map(str, indices)))
        tw.setItem(row, COLUMN_ID, item)
        item = make_item(','.join(map(str, indices_1)))
        tw.setItem(row_1, COLUMN_ID, item)
        tw.setCurrentCell(row_1, COLUMN_REGION) # Select the moved row


    def bcs_permute(self, old_order, new_order):
        data = {x: {} for x in old_order}
        for key in keys_by_type['bc']:
            for args in self.project.get_key_indices(key):
                if args and args[0] in old_order:
                    data[args[0]][(key, args[1:])]=self.project.get_value(key, args=args)
        bcs_copy = self.bcs.copy()
        for (old, new) in zip(old_order, new_order):
            # Update keys
            for ((key, extra_args), val) in data[new].items():
                self.update_keyword(key, val, args=(old,)+extra_args)
            # Clear any remaining keys
            for ((key, extra_args), val) in data[old].items():
                if (key, extra_args) not in data[new]:
                    self.unset_keyword(key, args=(old,)+extra_args)

            # Update internal memoized values
            tmp = bcs_copy.pop(new, None)
            if tmp:
                self.bcs[old] = tmp
            else:
                self.bcs.pop(old, None)


    def bcs_delete_regions(self):
        ui = self.ui.boundary_conditions
        tw = ui.tablewidget_regions
        row = get_selected_row(tw)
        if row is None: # No selection
            return

        # Unset keywords
        kwlist = list(self.project.keywordItems())
        for kw in kwlist:
            key, args = kw.key, kw.args
            if key.startswith('bc_') and args and args[0] in self.bcs_current_indices:
                self.unset_keyword(key, args=args)

        # Note, BC indices do not need to be sequential

        for r in self.bcs_current_regions:
            if r in self.bcs_region_dict:
                self.bcs_region_dict[r]['available'] = True

        for i in self.bcs_current_indices:
            if self.bc_is_cyclic(i):
                axis = i[0].lower()
                self.unset_keyword('cyclic_%s' % axis)
                if self.project.get_value('cyclic_%s_pd' % axis):
                    self.unset_keyword('cyclic_%s_pd' % axis)
                    self.unset_keyword('flux_g')
            else:
                del self.bcs[i]

        self.bcs_current_regions = []
        self.bcs_current_indices = []

        tw.removeRow(row)
        self.fixup_bcs_table(tw)
        self.bcs_setup_current_tab()
        self.update_nav_tree()
        nrows = tw.rowCount()
        row = get_selected_row(tw)
        cyclic =  row is not None and 'cyclic' in tw.item(row, COLUMN_ID).text().lower()
        prev_cyclic = row is not None and row > 0 and 'cyclic' in tw.item(row-1, COLUMN_ID).text().lower()
        ui.toolbutton_up.setEnabled(row is not None and row > 0 and not cyclic and not prev_cyclic)
        ui.toolbutton_down.setEnabled(row is not None and not cyclic and row != nrows-1)

    def bcs_delete_fluid_species(self, species_index):
        """Update internal memoized values when fluid species deleted"""
        keys = ['fluid_species_eq_type']
        for bc in self.bcs.values():
            for key in keys:
                d = bc.get(key)
                if d:
                    for i in range(species_index, 1+max(d.keys())):
                        d[i] = d.get(i+1)


    def bcs_delete_solids_phase(self, phase_index):
        """Update internal memoized values when solids phase deleted"""
        keys = ['solids_energy_eq_type',
                'solids_granular_energy_eq_type',
                'solids_inflow_type',
                'solids_outflow_type']

        for bc in self.bcs.values():
            for key in keys:
                d = bc.get(key)
                if d:
                    for i in range(phase_index, 1+max(d.keys())):
                        d[i] = d.get(i+1)

        if (self.bcs_current_solid is not None and
            self.bcs_current_solid >= phase_index):
            self.bcs_current_solid -= 1
            if self.bcs_current_solid == 0:
                self.bcs_current_solid = 1


    def handle_bcs_region_selection(self):
        ui = self.ui.boundary_conditions
        tw = ui.tablewidget_regions
        row = get_selected_row(tw)
        nrows = tw.rowCount()
        if row is None:
            indices = []
            regions = []
        else:
            (indices, regions) = tw.item(row,COLUMN_REGION).data(UserRole)
        BC0 = indices[0] if indices else None
        self.bcs_current_indices = indices
        self.bcs_current_regions = regions
        enabled = (row is not None)

        for item in (ui.toolbutton_delete,
                     ui.bottom_frame):
            item.setEnabled(enabled)
        if not enabled:
            ui.toolbutton_up.setEnabled(False)
            ui.toolbutton_down.setEnabled(False)
        else:
            cyclic = self.bc_is_cyclic(tw.item(row, 0).data(UserRole)[0][0])
            prev_cyclic = row > 0 and self.bc_is_cyclic(tw.item(row-1, 0).data(UserRole)[0][0])
            ui.toolbutton_up.setEnabled(row > 0 and not cyclic and not prev_cyclic)
            ui.toolbutton_down.setEnabled(not cyclic and row != nrows-1)

        if not enabled:
            #ui.combobox_bc_type.setCurrentIndex(NO_SLIP_WALL) # default type, but do we really need to set this ?  no.
            # Clear
            for widget in widget_iter(ui.bottom_frame):
                if isinstance(widget, LineEdit):
                    widget.setText('')
            return

        # see gui.py.enable_input
        if not ui.input_enabled:
            for item in (ui.label_bc_type,
                         ui.toolbutton_add,
                         ui.toolbutton_delete,
                         ui.toolbutton_up,
                         ui.toolbutton_down):
                item.setEnabled(False)

        if self.bc_is_cyclic(BC0):
            # Setup bc_type combobox. Do not allow changing from cyclic to
            # any other type, since there's no associated region

            cb = ui.combobox_bc_type
            cb.setCurrentIndex(CYCLIC_BOUNDARY)
            self.add_tooltip(cb, key=None, description='Cyclic boundary condition')
            for i in range(len(BC_TYPES)):
                get_combobox_item(cb, i).setEnabled(i==CYCLIC_BOUNDARY)

            for i in range(ui.tab_layout.columnCount()-1): # Skip 'Cyclic'
                item = ui.tab_layout.itemAtPosition(0, i)
                if not item:
                    continue
                widget = item.widget()
                if not widget:
                    continue
                widget.setEnabled(False)
            ui.pushbutton_cyclic.setEnabled(True)
            ui.pushbutton_cyclic.setToolTip(None)
        else: # Not cyclic
            # Setup bc_type combobox
            cb = ui.combobox_bc_type
            key = 'bc_type'
            bc_type = self.project.get_value(key, args=[BC0], default='')
            cg = bc_type.startswith("CG_")
            if cg:
                bc_type = bc_type[3:]
            if bc_type not in BC_TYPES:
                self.error("Unknown BC_TYPE %s" % bc_type)
                bc_type = 'NSW'
                for BC in self.bcs_current_indices:
                    self.update_keyword(key, bc_type, args=[BC])
            cb.setCurrentIndex(BC_TYPES.index(bc_type))
            self.add_tooltip(cb, key='bc_type', value=bc_type)

            #regions = [self.bcs[r].get('region') for r in self.bcs_current_indices]
            region_types = [self.bcs_region_dict.get(r,{}).get('type') for r in regions]

            #Available selections:
            # Mass Inflow
            # Plane regions set keyword BC_TYPE(#) to 'MI'
            # STL regions set keyword BC_TYPE(#) to 'CG_MI'
            # Not available for volume regions
            item = get_combobox_item(cb, MASS_INFLOW)
            enabled = ('box' not in region_types)
            item.setEnabled(enabled)

            # Pressure Outflow
            # Plane regions set keyword BC_TYPE(#) to 'PO'
            # STL regions set keyword BC_TYPE(#) to 'CG_PO'
            # Not available for volume regions
            item = get_combobox_item(cb, PRESSURE_OUTFLOW)
            enabled = ('box' not in region_types)
            item.setEnabled(enabled)

            # No Slip Wall
            # Volume and plane regions set keyword BC_TYPE(#) to 'NSW'
            # STL regions set keyword BC_TYPE(#) to 'CG_NSW'
            item = get_combobox_item(cb, NO_SLIP_WALL)
            item.setEnabled(True)

            # Free Slip Wall
            # Volume and plane regions set keyword BC_TYPE(#) to 'FSW'
            # STL regions set keyword BC_TYPE(#) to 'CG_FSW'
            item = get_combobox_item(cb, FREE_SLIP_WALL)
            item.setEnabled(True)

            # Partial Slip Wall
            # Volume and plane regions set keyword BC_TYPE(#) to 'PSW'
            # STL regions set keyword BC_TYPE(#) to 'CG_PSW'
            item = get_combobox_item(cb, PARTIAL_SLIP_WALL)
            item.setEnabled(True)

            #Pressure Inflow
            # Plane regions set keyword BC_TYPE(#) to 'PI'
            # Not available for volume regions
            # Not available for STL regions
            item = get_combobox_item(cb, PRESSURE_INFLOW)
            enabled = not ('box' in region_types or 'STL' in region_types)
            item.setEnabled(enabled)

            #Mass Outflow
            # Plane regions set keyword BC_TYPE(#) to 'MO'
            # STL regions set keyword BC_TYPE(#) to 'CG_MO'
            # Not available for volume regions
            item = get_combobox_item(cb, MASS_OUTFLOW)
            enabled = 'box' not in region_types
            item.setEnabled(enabled)

            #Cyclic
            # No region to select
            item = get_combobox_item(cb, CYCLIC_BOUNDARY)
            item.setEnabled(False)

            ui.pushbutton_fluid.setEnabled(not self.fluid_solver_disabled)
            if self.fluid_solver_disabled:
                ui.pushbutton_fluid.setToolTip("Fluid solver disabled")
            else:
                ui.pushbutton_fluid.setToolTip(None)
            nscalar = self.project.get_value('nscalar',default=0)
            ui.pushbutton_scalar.setEnabled(nscalar != 0)
            if nscalar == 0:
                ui.pushbutton_scalar.setToolTip("No scalar equations defined")
            else:
                ui.pushbutton_scalar.setToolTip(None)

            for i in range(1, ui.tab_layout.columnCount()-2): # Skip 'fluid', 'scalar' and 'cyclic'
                item = ui.tab_layout.itemAtPosition(0, i)
                if item is None:
                    continue
                widget = item.widget()
                if not widget:
                    continue
                widget.setEnabled(True)

            ui.pushbutton_cyclic.setEnabled(False)
            ui.pushbutton_cyclic.setToolTip("Only available for cyclic boundary conditions")

        # Reinitialize all widgets
        self.setup_bcs()
        # Scroll to top
        ui.scrollarea_detail.ensureVisible(0, 0)


    def bcs_find_valid_tab(self):
        if self.bcs_current_indices and self.bc_is_cyclic(self.bcs_current_indices[0]):
            return(CYCLIC_TAB, None)
        elif not self.fluid_solver_disabled:
            return (FLUID_TAB, None)
        elif self.solids:
            return (SOLIDS_TAB, 1)
        elif self.project.get_value('nscalar', default=0) != 0:
            return (SCALAR_TAB, None)
        else:
            self.error("Boundary condition:  all tabs disabled!")
        return (FLUID_TAB, None) # What else to do?


    def fixup_bcs_table(self, tw, stretch_column=0):
        ui = self.ui.boundary_conditions
        hv = QHeaderView
        resize = tw.horizontalHeader().setSectionResizeMode
        ncols = tw.columnCount()
        for n in range(0, ncols):
            resize(n, hv.Stretch if n==stretch_column else hv.ResizeToContents)

        # trim excess vertical space - can't figure out how to do this in designer
        header_height = tw.horizontalHeader().height()

        # Note - scrollbar status can change outside of this function.
        # Do we need to call this everytime window geometry changes?
        scrollbar_height = tw.horizontalScrollBar().isVisible() * (4+tw.horizontalScrollBar().height())
        nrows = tw.rowCount()
        if nrows==0:
            row_height = 0
            height = header_height+scrollbar_height
        else:
            row_height = tw.rowHeight(0)
            height =  (header_height+scrollbar_height
                       + nrows*row_height + 4) # extra to avoid unneeded scrollbar

        if tw == ui.tablewidget_regions:
            icon_height = sub_icon_height() + 8# +/- buttons
            ui.top_frame.setMaximumHeight(height+icon_height)
            ui.top_frame.setMinimumHeight(header_height+icon_height+row_height*min(nrows,5))
            ui.top_frame.updateGeometry()
            tw.setMaximumHeight(height)
            tw.setMinimumHeight(header_height)
        else:
            tw.setMaximumHeight(height)
            tw.setMinimumHeight(height)
        tw.updateGeometry() #? needed?


    def bcs_update_enabled(self):
        disabled = False # Since we have cyclic BCs, there's always something to do on this pane
        self.find_navigation_tree_item("Boundary Conditions").setDisabled(disabled)


    def bcs_change_tab(self, tab, solid):
        ui = self.ui.boundary_conditions
        index = (0 if tab==FLUID_TAB
                 else len(self.solids)+1 if tab==SCALAR_TAB
                 else len(self.solids)+2 if tab==CYCLIC_TAB
                 else solid)

        for i in range(ui.tab_layout.columnCount()):
            item = ui.tab_layout.itemAtPosition(0, i)
            if not item: # Why?
                continue
            widget = item.widget()
            if not widget: # Why?
                continue
            font = widget.font()
            font.setBold(i==index)
            widget.setFont(font)

        current_index = ui.stackedwidget.currentIndex()
        # If we're switching from solid m to solid n, we need some
        # special handling, because both tabs are really the same
        # widget.  We make a picture of the current tab, display that
        # in a dummy pane, then slide back to the solids tab
        if tab == current_index == SOLIDS_TAB:
            if solid == self.bcs_current_solid:
                return # nothing to do

            if solid > (self.bcs_current_solid or 0):
                dummy_label = ui.label_dummy_solids_L
                dummy_tab = SOLIDS_TAB_DUMMY_L
            else:
                dummy_label = ui.label_dummy_solids_R
                dummy_tab = SOLIDS_TAB_DUMMY_R

            p = QPicture()
            ui.page_solids.render(p, flags=QWidget.DrawChildren) # avoid rendering bg
            dummy_label.setPicture(p)
            ui.stackedwidget.setCurrentIndex(dummy_tab)

        self.bcs_current_tab = tab
        self.bcs_current_solid = self.P = solid if tab==SOLIDS_TAB else None
        self.bcs_setup_current_tab()

        # change stackedwidget contents
        self.animate_stacked_widget(
            ui.stackedwidget,
            ui.stackedwidget.currentIndex(),
            tab,
            direction='horizontal',
            line=ui.tab_underline,
            to_btn=ui.tab_layout.itemAtPosition(0, index),
            btn_layout=ui.tab_layout)
        # Scroll to top
        ui.scrollarea_detail.ensureVisible(0, 0)


    def bcs_check_region_in_use(self, name):
        # Should we allow any change of region type?  eg. xy plane -> xz plane?
        #  Probably not
        return any(data.get('region')==name for data in self.bcs.values())


    def bcs_update_region(self, name, data):
        for (i,bc) in self.bcs.items():
            if bc.get('region') == name:
                self.bcs_set_region_keys(name, i, data)


    def bcs_change_region_name(self, old_name, new_name):
        ui = self.ui.boundary_conditions
        for (key, val) in self.bcs.items():
            if val.get('region') == old_name:
                self.bcs[key]['region'] = new_name
                tw = ui.tablewidget_regions
                for i in range(tw.rowCount()):
                    data = tw.item(i,COLUMN_REGION).data(UserRole)
                    indices, names = data
                    if old_name in names:
                        item = tw.item(i,COLUMN_REGION)
                        names = [new_name if n==old_name else n for n in names]
                        item.setData(UserRole, (tuple(indices), tuple(names)))
                        item.setText('+'.join(names))
                        break
                break


    def bcs_set_region_keys(self, name, idx, data, bc_type=None):
        # Update the keys which define the region the BC applies to
        if bc_type is not None:
            self.update_keyword('bc_type', bc_type, args=[idx])
            if bc_type.startswith('CG_'): # STL region, don't set keys
                return

        no_k = self.project.get_value('no_k')
        for (key, val) in zip(('x_w', 'y_s', 'z_b',
                               'x_e', 'y_n', 'z_t'),
                              data['from']+data['to']):
            # bc_z_t and bc_z_b keywords should not be added when no_k=True
            if no_k and key in ('z_t', 'z_b'):
                continue
            self.update_keyword('bc_'+key, val, args=[idx])

    def bcs_set_default_keys(self, BC):
        # Ensure all needed keys are set when adding BC interactively
        #  Fixes issues/513
        # There is a fair amount of overlap between this function and the
        #  setup_bcs_*_tab functions.  It would be nice to remove some
        #  of the defaults handling from those functions, but we still
        #  need it there to fix up projects created outside the GUI,
        #  or created by the GUI and affected by issues/513
        # Perhaps in the future, the code can be consolidated here.
        # Idea:  call this on handle_bcs_region_selection each time a
        #  row is selected, then remove the defaults-handling code from
        #  the setup_bcs_*_tab methods.  In order to do that, the
        #  bcs_set_default_keys methods will need to be expanded
        #  to handle non-default energy eq type, and not to clobber
        #  existing settings (if we're calling it as a check, after
        #  BCs have been defined)
        #
        # But first, fix issues/513!

        bc_type = self.get_bc_type(BC)
        # 'endswith' tests also match 'CG_' type BCs
        if bc_type.endswith('W'):
            self.bcs_set_default_keys_W(BC)
        elif bc_type.endswith('I'):
            self.bcs_set_default_keys_I(BC)
        elif bc_type.endswith('PO'):
            self.bcs_set_default_keys_PO(BC)
        elif bc_type.endswith('MO'):
            self.bcs_set_default_keys_MO(BC)
        elif bc_type == 'CYCLIC':
            self.bcs_set_default_keys_CYCLIC(BC)
        else:
            self.error("Unknown BC_TYPE %s"% bc_type)

    def bcs_set_default_keys_W(self, BC):
        bc_type = self.get_bc_type(BC)
        # Assign some keywords to locals, since we use them repeatedly
        energy_eq = self.project.get_value('energy_eq', default=True)
        mmax = self.project.get_value('mmax', default=0)
        nmax_g = self.project.get_value('nmax_g', default=0)
        nmax_s = [None] + [self.project.get_value('nmax_s', default=0, args=[i])
                           for i in range(1,1+mmax)]
        species_eq = [self.project.get_value('species_eq', default=True, args=[i])
                      for i in range(1+mmax)] # including index 0 = fluid
        solids_model = [None] + [self.project.get_value('solids_model', args=[i])
                                 for i in range(1,1+mmax)]
        solids_models = set(solids_model[1:])
        friction_model = self.project.get_value('friction_model', default=DEFAULT_FRICTION_MODEL)
        kt_type = self.project.get_value('kt_type', default=DEFAULT_KT_TYPE)
        nscalar = self.project.get_value('nscalar', default=0)
        cartesian_grid = self.project.get_value('cartesian_grid', default=False)
        # Fluids
        # Per Jordan, we can set these keys whether or not fluid solver is enabled
        # Define transfer coefficient
        # Define Wall [UVW]-velocity
        # Specification only available with PSW/CG_PSW
        if bc_type in ('PSW', 'CG_PSW'):
            key = 'bc_hw_g'
            default = 0.0
            self.set_keyword_default(key, default, args=[BC])
            for c in 'uvw':
                key = 'bc_%sw_g' % c
                self.set_keyword_default(key, default, args=[BC])

        #Select energy equation boundary type:
        # Selection only available when solving energy equations
        # No-Flux (adiabatic) [DEFAULT]
        #  Sets keyword BC_HW_T_G(#) to 0.0
        #  Sets keyword BC_C_T_G(#) to 0.0
        #  Sets keyword BC_TW_G(#) to UNDEFINED
        if energy_eq:
            default = 0.0
            for key in 'bc_hw_t_g', 'bc_c_t_g':
                self.set_keyword_default(key, default, args=[BC])

        #Select species equation boundary type:
        # Selection only available when solving species equations
        # No-Flux [DEFAULT]
        #  Sets keyword BC_HW_X_G(#,#) to 0.0
        #  Sets keyword BC_C_X_G(#,#) to 0.0
        #  Sets keyword BC_XW_G(#,#) to UNDEFINED
        if species_eq[0]:
            default = 0.0
            for i in range(1, 1+nmax_g):
                for key in 'bc_hw_x_g', 'bc_c_x_g':
                    self.set_keyword_default(key, default, args=[BC,i])

        #Wall, solids
        #Enable Jackson-Johnson partial slip boundary
        # Sets keyword BC_JJ_PS(#)
        # Disabled for DEM and PIC solids
        # Disabled (0.0) for CARTESIAN_GRID = .TRUE.
        # Disabled (0.0) for KT_TYPE = 'ALGEBRAIC'
        # Disabled (0.0) for KT_TYPE = 'GHD_2007'
        # DEFAULT 1.0 when not disabled
        bc_jj_ps = int('TFM' in solids_models
                       and not cartesian_grid
                       and kt_type not in ('ALGEBRAIC', 'GHD_2007'))
        self.set_keyword_default('bc_jj_ps', bc_jj_ps, args=[BC])

        #Select type of Jackson and Johnson BC:
        # Disabled for DEM and PIC solids
        # Selection only available BC_JJ_PS(#) = 1.0
        # Default Jackson-Johnson BC [DEFAULT]
        #  Sets keyword BC_JJ_M to .FALSE.
        #  Sets keyword JENKINS to .FALSE.
        if bc_jj_ps:
            for key in 'bc_jj_m', 'jenkins':
                if self.project.get_value(key) is None:
                    # Respect existing settings
                    self.set_keyword_default(key, False)
        bc_jj_m = self.project.get_value('bc_jj_m')
        jenkins = self.project.get_value('jenkins')

        #Define restitution coefficient
        # Specification only available with BC_JJ_PS(#) = 1.0
        # Sets keyword E_W
        # DEFAULT 1.0
        if bc_jj_ps:
            key = 'e_w'
            default = 1.0
            if self.project.get_value(key) is None:
                self.set_keyword_default(key, default)

        #Define specularity coefficient
        # Specification only available with BC_JJ_PS(#)=1.0 and JENKINS=.FALSE.
        # Sets keyword PHIP
        # DEFAULT 0.6
        if bc_jj_ps and not jenkins:
            key = 'phip'
            default = 0.6
            if self.project.get_value(key) is None:
                self.set_keyword_default(key, default)

        # DEFAULT for phip0 is 'unset', so ignore it here

        #Define angle of internal friction
        # Disabled for DEM and PIC solids# (covered by bc_jj_ps)
        # Sets keyword PHI_W
        # Specification only available with BC_JJ_PS(#)=1.0 and
        # (JENKINS=.TRUE. or FRICTION_MODEL=SRIVASTAVA)
        #  DEFAULT 11.31
        if bc_jj_ps and (jenkins or friction_model=='SRIVASTAVA'):
            key = 'phi_w'
            default = 11.31
            if self.project.get_value(key) is None: # respect existing setting
                self.set_keyword_default(key, default)

        #Define transfer coefficient
        # Disabled for DEM and PIC solids
        # Specification only available with PSW/CG_PSW
        # Sets keyword BC_HW_S(#,#)
        # DEFAULT 0.0
        if bc_type in ('PSW', 'CG_PSW'):
            key = 'bc_hw_s'
            default = 0.0
            for P in range(1, 1+mmax):
                if solids_model[P] not in ('DEM', 'PIC'):
                    self.set_keyword_default(key, default, args=[BC,P])

        #Define Wall [UVW]-velocity
        # Disabled for DEM and PIC solids
        # Specification only available with PSW/CG_PSW or BC_JJ_PS(#) = 1.0
        # Sets keyword BC_[UVW]W_S(#,#)
        # DEFAULT 0.0
        if bc_type in ('PSW', 'CG_PSW') or bc_jj_ps:
            for P in range(1, 1+mmax):
                if solids_model[P] not in ('DEM', 'PIC'):
                    for c in 'uvw':
                        key = 'bc_%sw_s'%c
                        default = 0.0
                        self.set_keyword_default(key, default, args=[BC,P])

        #Select energy equation boundary type:
        # Disabled for PIC solids
        # Selection only available when solving energy equations
        # No-Flux (adiabatic) [DEFAULT]
        #  Sets keyword BC_HW_T_S(#,#) to 0.0
        #  Sets keyword BC_C_T_S(#,#) to 0.0
        #  Sets keyword BC_TW_S(#,#) to UNDEFINED
        if energy_eq:
            for P in range(1, 1+mmax):
                if solids_model[P] != 'PIC':
                    default = 0.0
                    for key in 'bc_hw_t_s', 'bc_c_t_s':
                        self.set_keyword_default(key, default, args=[BC,P])

        #Select granular energy equation boundary type:
        # Disabled for DEM and PIC solids
        # Selection only available with BC_JJ_PS(#)=0.0 and KT_TYPE /= 'ALGEBRAIC'
        # No-Flux [DEFAULT]
        #  Sets keyword BC_HW_THETA_M(#,#) to 0.0
        #  Sets keyword BC_C_THETA_M (#,#) to 0.0
        #  Sets keyword BC_THETAW_M(#,#) to UNDEFINED
        if not bc_jj_ps and kt_type != 'ALGEBRAIC':
            for P in range(1, 1+mmax):
                if solids_model[P] not in ('DEM', 'PIC'):
                    default = 0.0
                    for key in 'bc_hw_theta_m', 'bc_c_theta_m':
                        self.set_keyword_default(key, default, args=[BC,P])

        #**When solving solids species equations:**
        # Set keyword BC_HW_X_S(#,#,#) to 0.0
        # Set keyword BC_C_X_S(#,#,#) to 0.0
        # Set keyword BC_XW_S(#,#,#) to UNDEFINED
        # Note, this overlaps with bcs_check_wall_keys
        for P in range(1, 1+mmax):
            if species_eq[P]:
                for s in range(1, 1+nmax_s[P]):
                    default = 0.0
                    for key in 'bc_hw_x_s', 'bc_c_x_s':
                        self.set_keyword_default(key, default, args=[BC,P,s])
        #Scalar
        #Select scalar boundary type:
        # No-Flux [DEFAULT]
        # Sets keyword BC_HW_SCALAR(#,#) to 0.0
        # Sets keyword BC_C_SCALAR(#,#) to 0.0
        # Sets keyword BC_SCALARW(#,#) to UNDEFINED
        for n in range(1, nscalar+1):
            default = 0.0
            for key in 'bc_hw_scalar', 'bc_c_scalar':
                self.set_keyword_default(key, default, args=[BC,n])


    def get_bc_normal(self, BC):
        # return 'u', 'v', 'w' or '' depending on orientation of BC
        # empty string for STL or undefined

        if self.bcs_region_dict is None:
            self.bcs_region_dict = self.ui.regions.get_region_dict()

        # Instead of looking in the region dict, we could look at extents
        # to determine plane orientation - that would be more robust at
        # load time, not depending on order of inititialization
        region_name = self.bcs.get(BC, {}).get('region', {})
        if not region_name:
            self.error("No region for BC %s" % BC)
            return ''
        else:
            region_data = self.bcs_region_dict.get(region_name)
            shape = region_data.get('type')
            if not shape:
                self.error("No shape for BC %s" % region_name)
                return ''
        tangents = [c for c in 'XYZ' if c in shape]
        if len(tangents) == 2:
            normal = [c for c in 'XYZ' if c not in tangents][0]
            return xmap[normal]
        else: # STL or error
            return ''


    def bcs_set_default_keys_I(self, BC):
        bc_type = self.get_bc_type(BC)
        normal = self.get_bc_normal(BC)
        mmax = self.project.get_value('mmax', default=0)
        nmax_g = self.project.get_value('nmax_g', default=0)
        nmax_s = [None] + [self.project.get_value('nmax_s', default=0, args=[i])
                           for i in range(1,1+mmax)]
        turbulence_model = self.project.get_value('turbulence_model', default=DEFAULT_TURBULENCE_MODEL)
        kt_type = self.project.get_value('kt_type', default=DEFAULT_KT_TYPE)

        # Fluid
        #Define volume fraction
        # Specification always available
        # Sets keyword BC_EP_G(#)
        # DEFAULT 1.0 for MI and CG_MI; leave [UNDEFINED] for PI
        # Error Check: For MI and CG_MI, BC_EP_G(#) + BC_EP_S(#,:) = 1.0
        if bc_type != 'PI':
            key = 'bc_ep_g'
            default = 1.0
            self.set_keyword_default(key, default, args=[BC])

        # Set velocities to 0, unless mass or volumetric flow
        if bc_type != 'CG_MI':
            flowrate = any(self.project.get_value(key, args=[BC]) is not None
                       for key in ('bc_massflow_g', 'bc_volflow_g'))
            default = 0.0
            for c in 'uvw':
                if flowrate and c==normal: # Don't clobber flowrate, issues/605
                    continue
                key = 'bc_%s_g' % c
                self.set_keyword_default(key, default, args=[BC])

        # For BC_TYPE='CG_MI'
        # Specify gas mass flow rate (required):
        # Sets keyword BC_MASSFLOW_g(#)
        # DEFAULT 0.0
        if bc_type == 'CG_MI':
            key = 'bc_massflow_g'
            default = 0.0
            self.set_keyword_default(key, default, args=[BC])

        # Define temperature
        # Specification always available
        # Sets keyword BC_T_G(#)
        # DEFAULT 293.15
        # Note - just set the default, regardless of conditions
        key = 'bc_t_g'
        default = 293.15
        self.set_keyword_default(key, default, args=[BC])

        # Define pressure
        # Specification always available
        #  Sets keyword BC_P_G(#)
        #  DEFAULT 101.325d3
        key = 'bc_p_g'
        default = FloatExp('101.325e3')
        self.set_keyword_default(key, default, args=[BC])

        # Select species and set mass fractions (table format)
        #  Specification always available
        #  Input required for species equations
        #  Drop down menu of fluid species
        #  Sets keyword BC_X_G(#,#)
        #  DEFAULT -  last defined species has mass fraction of 1.0
        #  Error check: mass fractions must sum to 1.0
        key = 'bc_x_g'
        for i in range(1, 1+nmax_g):
            default = 0
            self.set_keyword_default(key, default, args=[BC, i])
        total = sum(safe_float(self.project.get_value(key, default=0, args=[BC,i]))
                    for i in range(1, nmax_g)) # All but last
        if total == 0 and nmax_g > 0:  # Set last species to 1, only if all other are 0
            self.update_keyword(key, 1.0, args=[BC, nmax_g])

        #  Turbulence: Define k-ε turbulent kinetic energy
        #  Specification only available with K-Epsilon turbulence model
        #  Sets keyword BC_K_TURB_G(#)
        #  DEFAULT 0.0
        if turbulence_model == 'K_EPSILON':
            key = 'bc_k_turb_g'
            default = 0.0
            self.set_keyword_default(key, default, args=[BC])

        #  Turbulence: Define k-ε turbulent dissipation
        #  Specification only available with K-Epsilon turbulence model
        #  Sets keywords BC_E_TURB_G(#)
        #  DEFAULT 0.0
        if turbulence_model == 'K_EPSILON':
            key = 'bc_e_turb_g'
            default = 0.0
            self.set_keyword_default(key, default, args=[BC])

        # Solids
        # FIXME correct handling of bc_ep_s
        # Set all solids volume fractions to 0
        # Sets keyword BC_EP_S(#,#)
        #key = 'bc_ep_s'
        #default = 0.0
        #for P in range(1, 1+mmax):
        #    self.set_keyword_default(key, default, args=[BC,P])

        # Define volume fraction
        # Sets keyword BC_EP_S(#,#)
        # DEFAULT 1.0 - (sum of previous tabs) for MI and CG_MI; leave [UNDEFINED] for PI
        # Note, bc_ep_g is 1.0, if defined
        key = 'bc_ep_s'
        default = 0.0
        if bc_type != 'PI':
            for P in range(1, 1+mmax):
                self.set_keyword_default(key, default, args=[BC,P])

        # Define inflow properties
        # Set velocities to 0 unless mass or volumetric flowrate
        if bc_type != 'CG_MI':
            default = 0.0
            for c in 'uvw':
                key = 'bc_%s_s' % c
                for P in range(1, 1+mmax):
                    flowrate = any(self.project.get_value(key, args=[BC,P]) is not None
                                   for key in ('bc_massflow_s', 'bc_volflow_s'))
                    if flowrate and c==normal: # Don't clobber flowrate, issues/605
                        continue
                    self.set_keyword_default(key, default, args=[BC,P])

        #For BC_TYPE=’CG_MI’
        #Specify solids mass flow rate (required):
        #Sets keyword BC_MASSFLOW_s(#,#)
        #DEFAULT 0.0
        if bc_type == 'CG_MI':
            key = 'bc_massflow_s'
            default = 0.0
            for P in range(1, 1+mmax):
                self.set_keyword_default(key, default, args=[BC,P])

        #Define temperature
        #Specification always available
        #Input required when energy equations are solved
        #Sets keyword BC_T_S(#,#)
        #DEFAULT 293.15
        key = 'bc_t_s'
        default = 293.15
        for P in range(1, 1+mmax):
            self.set_keyword_default(key, default, args=[BC,P])

        #Select species and set mass fractions (table format)
        #Specification always available
        #Input required for species equations
        #Sets keyword BC_X_S(#,#,#)
        #DEFAULT - last defined species has mass fraction of 1.0
        key = 'bc_x_s'
        for P in range(1, 1+mmax):
            for i in range(1, 1+nmax_s[P]):
                default = 0.0
                self.set_keyword_default(key, default, args=[BC, P, i])
            total = sum(safe_float(self.project.get_value(key, default=0, args=[BC,P,i]))
                        for i in range(1, nmax_s[P])) # All but last
            if total == 0 and nmax_s[P] > 0:  # Set last species to 1, only if all other are 0
                self.update_keyword(key, 1.0, args=[BC,P,nmax_s[P]])

        #Define solids phase granular temperature at the BC plane.
        #Sets keyword BC_THETA_M
        #DEFAULT value ZERO
        #Available only for KT_TYPE /= “ALGEBRAIC"
        key = 'bc_theta_m'
        default = 0.0
        if kt_type != 'ALGEBRAIC':
            for P in range(1, 1+mmax):
                self.set_keyword_default(key, default, args=[BC, P])

        #Scalars
        #  Define initial scalar value
        #  Sets keyword BC_SCALAR(#,#)
        #  DEFAULT 0.0
        nscalar = self.project.get_value('nscalar', default=0)
        key = 'bc_scalar'
        default = 0.0
        for N in range(1, 1+nscalar):
            self.set_keyword_default(key, default, args=[BC,N])

    def bcs_set_default_keys_PO(self, BC):
        #Define pressure
        # Sets keyword BC_P_G(#)
        # DEFAULT 101.325d3
        key = 'bc_p_g'
        default = 101.325e3
        self.set_keyword_default(key, default, args=[BC])
        #Solids-# Tab
        #All inputs are optional
        #Scalars
        #All inputs are optional

    def bcs_set_default_keys_MO(self, BC):
        bc_type = self.get_bc_type(BC)
        normal = self.get_bc_normal(BC)
        mmax = self.project.get_value('mmax', default=0)
        nmax_g = self.project.get_value('nmax_g', default=0)
        nmax_s = [None] + [self.project.get_value('nmax_s', default=0, args=[i])
                           for i in range(1,1+mmax)]
        turbulence_model = self.project.get_value('turbulence_model', default=DEFAULT_TURBULENCE_MODEL)
        kt_type = self.project.get_value('kt_type', default=DEFAULT_KT_TYPE)

        # Specify all velocity components unless mass or volumetric
        # Sets keyword BC_{U,V,W}_G(#)
        # DEFAULT 0.0
        flowrate = any(self.project.get_value(key, args=[BC]) is not None
                       for key in ('bc_massflow_g', 'bc_volflow_g'))
        default = 0.0
        for c in 'uvw':
            if flowrate and c==normal: # Don't clobber flowrate, issues/605
                continue
            key = 'bc_%s_g' % c
            self.set_keyword_default(key, default, args=[BC])

        # Define duration to average outflow rate.
        # Sets keyword BC_DT_0(#)
        # DEFAULT 0.1
        key = 'bc_dt_0'
        default = 0.1
        self.set_keyword_default(key, default, args=[BC])

        # Solids
        # Specify velocity components, unless mass or volumetric flow
        # Sets keyword BC_{U,V,W}_S(#,#)
        # DEFAULT 0.0
        default = 0.0
        for c in 'uvw':
            key = 'bc_%s_s' % c
            for P in range(1, 1+mmax):
                flowrate = any(self.project.get_value(key, args=[BC,P]) is not None
                               for key in ('bc_massflow_s', 'bc_volflow_s'))
                if flowrate and c==normal: # Don't clobber flowrate, issues/605
                    continue
                self.set_keyword_default(key, default, args=[BC,P])
        # Scalars?

    def bcs_set_default_keys_CYCLIC(self, BC):
        # Set DELP_{XYZ} to False?  Not needed
        pass

    def bcs_check_wall_keys(self, indices=None):
        # SRS p 37:
        # When solving solids species equations:
        #    Set keyword BC_HW_X_S(#,#,#) to 0.0
        #    Set keyword BC_C_X_S(#,#,#) to 0.0
        #    Set keyword BC_XW_S(#,#,#) to UNDEFINED
        #
        #  Note: this needs to be set:
        #  1:  when toggling solids species eq
        #  2:  when adding a new solids phase or species
        #  3:  when adding a new BC
        #  When deleting BC/phase/species, the keys also need to be deleted,
        #  but this does not need to be done explicitly - all associated keys
        #  are deleted automatically when one of these objects is destroyed
        #
        # See also issues/183

        if indices is None: # Check all BCs
            indices = self.bcs.keys()
        for BC in indices:
            bc_type = self.get_bc_type(BC)
            if bc_type == 'CYCLIC':
                continue
            for P in range(1, 1+self.project.get_value('mmax', default=0)):
                hw, c, xw = None, None, None # Unset all 3 keys by default
                if bc_type is not None and bc_type.endswith('W'): # Wall
                    if self.project.get_value('species_eq', args=[P]):
                        hw, c, xw = 0.0, 0.0, None

                for S in range(1, 1+self.project.get_value('nmax_s', default=0, args=[P])):
                    for (key, val) in (('bc_hw_x_s', hw),
                                       ('bc_c_x_s', c),
                                       ('bc_xw_s', xw)):
                        self.update_keyword(key, val, args=[BC,P,S])


    def reset_bcs(self):
        self.bcs.clear()
        self.bcs_current_indices = []
        self.bcs_current_regions = []
        self.bcs_region_dict = None
        ui = self.ui.boundary_conditions
        tw = ui.tablewidget_regions
        tw.clearContents()
        tw.setRowCount(0)
        # anything else to do here?
        # TODO remove dynamically created input widgets, although this should
        #  get handled next time we call 'setup'

    def bc_regions_to_str(self):
        ui = self.ui.boundary_conditions
        tw = ui.tablewidget_regions
        data = [tw.item(i,COLUMN_REGION).data(UserRole) for i in range(tw.rowCount())]
        return JSONEncoder().encode(data)


    def bcs_regions_from_str(self, s):
        if not s:
            return
        data = JSONDecoder().decode(s)
        for (indices, regions) in data:
            if not indices:
                continue # should not get empty tuple
            # bc_type keyword should be set already when we call this
            bc_type = self.get_bc_type(indices[0])
            self.bcs_add_regions_1(regions, bc_type=bc_type, indices=indices, autoselect=False)


    def setup_bcs(self):
        ui = self.ui.boundary_conditions
        BC0 = self.bcs_current_indices[0] if self.bcs_current_indices else None

        # Grab a fresh copy, may have been updated
        self.bcs_region_dict = self.ui.regions.get_region_dict()

        # Mark regions which are in use (this gets reset each time we get here)
        for (i, data) in self.bcs.items():
            region = data['region']
            if region in self.bcs_region_dict:
                self.bcs_region_dict[region]['available'] = False

        self.fixup_bcs_table(ui.tablewidget_regions)
        row = get_selected_row(ui.tablewidget_regions)
        # Autoselect if only 1 row
        if row is None and ui.tablewidget_regions.rowCount() == 1:
            row = 0
            ui.tablewidget_regions.setCurrentCell(row, COLUMN_REGION)
        enabled = (row is not None)
        ui.bottom_frame.setEnabled(enabled)
        ui.toolbutton_delete.setEnabled(enabled and ui.input_enabled)

        # Tabs group boundary condition parameters for phases and additional equations. Tabs are
        # unavailable if no input is required from the user.
        #
        #Fluid tab - Unavailable if the fluid phase was disabled.
        cyclic = self.bc_is_cyclic(BC0)

        b = ui.pushbutton_fluid
        b.setText(self.fluid_phase_name)
        enabled = not (cyclic or self.fluid_solver_disabled)
        b.setEnabled(enabled)
        font = b.font()
        font.setBold(self.bcs_current_tab == FLUID_TAB)
        b.setFont(font)

        #  Each solid phase will have its own tab. The tab name should be the name of the solid
        solids_names = list(self.solids.keys())
        if self.bcs_saved_solids_names != solids_names:
            # Clear out the old ones
            n_cols = ui.tab_layout.columnCount()
            for i in range(n_cols-1, 0, -1):
                item = ui.tab_layout.itemAtPosition(0, i)
                if not item:
                    continue
                widget = item.widget()
                if not widget:
                    continue
                if widget in (ui.pushbutton_fluid, ui.pushbutton_scalar, ui.pushbutton_cyclic):
                    continue
                ui.tab_layout.removeWidget(widget)
                widget.setParent(None)
                widget.deleteLater()
            # And make new ones
            for (i, solid_name) in enumerate(solids_names, 1):
                b = QPushButton(text=solid_name)
                w = b.fontMetrics().boundingRect(solid_name).width() + 20
                b.setMaximumWidth(w)
                b.setFlat(True)
                b.setEnabled(not cyclic)
                font = b.font()
                font.setBold(self.bcs_current_tab==SOLIDS_TAB and i==self.bcs_current_solid)
                b.setFont(font)
                b.pressed.connect(lambda i=i: self.bcs_change_tab(SOLIDS_TAB, i))
                ui.tab_layout.addWidget(b, 0, i)

        #Scalar (tab) - Tab only available if scalar equations are solved
        # Move the 'Scalar' button to the right of all solids, if needed
        b = ui.pushbutton_scalar
        font = b.font()
        font.setBold(self.bcs_current_tab==SCALAR_TAB)
        b.setFont(font)
        nscalar = self.project.get_value('nscalar', default=0)
        enabled = (nscalar > 0) and not cyclic
        b.setEnabled(enabled)
        if len(self.solids) != len(self.bcs_saved_solids_names):
            ui.tab_layout.removeWidget(b)
            ui.tab_layout.addWidget(b, 0, 1+len(self.solids))

        # Move the 'Cyclic' button to the right of all solids, if needed
        b = ui.pushbutton_cyclic
        font = b.font()
        font.setBold(self.bcs_current_tab==CYCLIC_TAB)
        b.setFont(font)
        enabled = cyclic
        b.setEnabled(enabled)
        if len(self.solids) != len(self.bcs_saved_solids_names):
            ui.tab_layout.removeWidget(b)
            ui.tab_layout.addWidget(b, 0, 2+len(self.solids))

        self.bcs_saved_solids_names = solids_names
        self.P = self.bcs_current_solid

        # Don't stay on a disabled tab
        index = self.bcs_tab_to_index(self.bcs_current_tab, self.bcs_current_solid)
        item = None if index is None else ui.tab_layout.itemAtPosition(0, index)
        b = item.widget() if item else None
        if ui.isEnabled() and not (b and b.isEnabled()):
            self.bcs_change_tab(*self.bcs_find_valid_tab())
        else:
            self.bcs_setup_current_tab()

        # make sure underline is in the right place, as # of solids may
        # have changed (lifted from animate_stacked_widget, which we
        # don't want to call here)
        tab = self.bcs_current_tab
        line_to = self.bcs_tab_to_index(tab, self.bcs_current_solid)
        line = ui.tab_underline
        btn_layout = ui.tab_layout
        if line_to is not None:
            btn_layout.addItem(btn_layout.takeAt(
                btn_layout.indexOf(line)), 1, line_to)


    def bcs_tab_to_index(self, tab, solid):
        return (0 if tab==FLUID_TAB
                else len(self.solids)+1 if tab==SCALAR_TAB
                else len(self.solids)+2 if tab==CYCLIC_TAB
                else solid)

    def bcs_setup_current_tab(self):
        self.setup_bcs_tab(self.bcs_current_tab)

    def setup_bcs_tab(self, tab):
        if self.bcs_current_tab == FLUID_TAB:
            self.setup_bcs_fluid_tab()
        elif self.bcs_current_tab == SOLIDS_TAB:
            self.setup_bcs_solids_tab(self.bcs_current_solid)
        elif self.bcs_current_tab == SCALAR_TAB:
            self.setup_bcs_scalar_tab()
        elif self.bcs_current_tab == CYCLIC_TAB:
            self.setup_bcs_cyclic_tab()
        else:
            raise ValueError("invalid tab %s" % tab)

    def bcs_extract_regions(self):
        ui = self.ui.boundary_conditions

        if ui.tablewidget_regions.rowCount() > 0:
            # We assume that BC regions have been initialized correctly
            # from mfix_gui_comments.
            # TODO: verify that there is an BC region for each BC
            return

        if self.bcs_region_dict is None:
            self.bcs_region_dict = self.ui.regions.get_region_dict()

        # TODO: if we wanted to be fancy, we could find regions where
        # BC values matched, and merge into a new BC region.  That
        # is only needed for projects created outside the GUI (otherwise
        # we have already stored the BC regions).  Also would be nice
        # to offer a way to split compound regions.
        for bc in self.project.bcs:
            d = bc.keyword_dict
            extent = [d.get('bc_'+k, None) for k in ('x_w', 'y_s', 'z_b',
                                                     'x_e', 'y_n', 'z_t')]
            # should we distinguish 0 from unset?  in the region_dict we get
            #  from the regions_widget, the values are 0, while in the project,
            #  keywords are simply unset (value None) rather than set to 0

            extent = [0.0 if x is None else x.value for x in extent]

            bc_type = d.get('bc_type')
            if bc_type is None:
                self.error("No type for boundary condition %s" % bc.ind)
                continue
            bc_type = bc_type.value #

            is_stl = (bc_type.startswith('CG_'))

            # Check dimensionality?
            #if any (x is None for x in extent):
            #    self.warn("boundary condition %s: invalid extents %s" %
            #               (bc.ind, extent))
            #    continue

            for (region_name, data) in self.bcs_region_dict.items():

                ext2 = [0 if x is None else x for x in
                        data.get('from',[]) + data.get('to',[])]

                # TODO this only works for a single STL
                if (is_stl and data.get('type')=='STL'
                    or not is_stl and ext2==extent):

                    if data.get('available', True):
                        if bc_type is None:
                            self.warn("no bc_type for region %s" % bc.ind)
                        if not (bc_type in BC_TYPES or (is_stl and bc_type[3:] in BC_TYPES)):
                            self.warn("invalid bc_type %s for region %s" % (bc_type, bc.ind))
                        else:
                            self.bcs_add_regions_1([region_name],
                                                   bc_type=bc_type,
                                                   indices=[bc.ind],
                                                   autoselect=False)
                            break
            else:
                self.warn("boundary condition %s: could not match defined region %s" %
                          (bc.ind, extent))
                kwlist = list(self.project.keywordItems())
                for kw in kwlist:
                    key, args = kw.key, kw.args
                    if key.startswith('bc_') and args and args[0]==bc.ind:
                        self.unset_keyword(key, args=args)

        # Handle cyclic BCs which don't really have a region
        for axis in 'X', 'Y', 'Z':
            q = axis.lower()
            keys = ('cyclic_'+q, 'cyclic_%s_pd'%q, 'delp_'+q)
            if any(self.project.get_value(key) for key in keys):
                name = axis + '-cyclic'
                self.bcs_add_regions_1([name], bc_type='CYCLIC', indices=None, autoselect=False)


    def set_bcs_fluid_energy_eq_type(self, eq_type, in_setup=False):
        if not self.bcs_current_indices:
            return
        #Select energy equation boundary type:
        # Available selections:
        #  No-Flux (adiabatic) [DEFAULT]
        if eq_type == NO_FLUX:
            #    Sets keyword BC_HW_T_G(#) to 0.0
            hw = 0.0
            #    Sets keyword BC_C_T_G(#) to 0.0
            c = 0.0
            #    Sets keyword BC_TW_G(#) to UNDEFINED
            tw = None
        #  Specified Temperature
        elif eq_type == SPECIFIED_TEMPERATURE:
            #    Sets keyword BC_HW_T_G(#) to UNDEFINED
            hw = None
            #    Sets keyword BC_C_T_G(#) to 0.0
            c = 0.0
            #    Requires BC_TW_G(#)
            tw = True
        #  Specified Flux
        elif eq_type == SPECIFIED_FLUX:
            #    Sets keyword BC_HW_T_G(#) to 0.0
            hw = 0.0
            #    Requires BC_C_T_G(#)
            c = True
            #    Sets keyword BC_TW_G(#) to UNDEFINED
            tw = None
        elif eq_type == CONVECTIVE_FLUX:
            #    Requires BC_HW_T_G(#)
            hw = True
            #    Sets keyword BC_C_T_G(#) to 0.0
            c = 0.0
            #    Requires BC_TW_G(#)
            tw = True
        else:
            self.error("Invalid fluid energy_eq type %s" % eq_type)
            return

        for BC in self.bcs_current_indices:
            self.bcs[BC]['fluid_energy_eq_type'] = eq_type
            for (key, val) in (('bc_hw_t_g', hw), ('bc_c_t_g', c), ('bc_tw_g', tw)):
                if val is True:
                    pass
                else:
                    self.update_keyword(key, val, args=[BC])

        if not in_setup: #Avoid recursive call to setup
            self.setup_bcs_fluid_tab()


    def set_bcs_fluid_species_eq_type(self, eq_type, species_index, in_setup=False):
        if not self.bcs_current_indices:
            return
        #Select species equation boundary type:
        # Selection only available when solving species equations
        # Available selections:
        #  No-Flux [DEFAULT]
        if eq_type == NO_FLUX:
            #    Sets keyword BC_HW_X_G(#,#) to 0.0
            hw = 0.0
            #    Sets keyword BC_C_X_G(#,#) to 0.0
            c = 0
            #    Sets keyword BC_XW_G(#,#) to UNDEFINED
            xw = None
        #  Specified Mass Fraction
        elif eq_type == SPECIFIED_MASS_FRACTION:
            #    Sets keyword BC_HW_X_G(#,#) to UNDEFINED
            hw = None
            #    Sets keyword BC_C_X_G(#,#) to 0.0
            c = 0.0
            #    Requires BC_XW_G(#,#)
            xw = True
        #  Specified Flux
        elif eq_type == SPECIFIED_FLUX:
            #    Sets keyword BC_HW_X_G(#,#) to 0.0
            hw = 0.0
            #    Requires BC_C_X_G(#,#)
            c = True
            #    Sets keyword BC_XW_G(#,#) to UNDEFINED
            xw = None
        #  Convective Flux
        elif eq_type == CONVECTIVE_FLUX:
            #,#    Requires BC_HW_X_G(#,#)
            hw = True
            #    Sets keyword BC_C_X_G(#,#) to 0.0
            c = 0.0
            #    Requires BC_XW_G(#,#)
            xw = True
        else:
            self.error("Invalid fluid species_eq type %s" % eq_type)
            return

        for BC in self.bcs_current_indices:
            if 'fluid_species_eq_type' not in self.bcs[BC]:
                self.bcs[BC]['fluid_species_eq_type'] = {}
            self.bcs[BC]['fluid_species_eq_type'][species_index] = eq_type
            for (key, val) in (('bc_hw_x_g', hw), ('bc_c_x_g', c), ('bc_xw_g', xw)):
                if val is True:
                    pass # 'required'
                else:
                    self.update_keyword(key, val, args=[BC, species_index])
        if not in_setup:
            self.setup_bcs_fluid_tab()


    def set_bc_jj_ps(self, val):
        # Note, BC_JJ_PS is integer, not bool!
        val = int(val)
        if not self.bcs_current_indices:
            return
        key = 'bc_jj_ps'
        for BC in self.bcs_current_indices:
            self.update_keyword(key, val, args=BC)
        self.setup_bcs_solids_tab(self.bcs_current_solid)


    def set_bcs_bc_jj_ps_type(self, val):
        # Available selections:
        #  Default Jackson-Johnson BC [DEFAULT]
        #    Sets keyword BC_JJ_M to .FALSE.
        #    Sets keyword JENKINS to .FALSE.
        #  Variable specularity coefficient
        #    Sets keyword BC_JJ_M to .TRUE.
        #    Sets keyword JENKINS to .FALSE.
        #  Jenkins small frictional boundary
        #    Sets keyword BC_JJ_M to .FALSE.
        #    Sets keyword JENKINS to .TRUE.
        if val < 0 or val > 2:
            self.warn("Invalid bc_jj_ps_type % val")
            return
        jenkins, bc_jj_m = int(val/2), val%2
        self.update_keyword('bc_jj_m', bool(bc_jj_m))
        self.update_keyword('jenkins', bool(jenkins))
        self.setup_bcs_solids_tab(self.bcs_current_solid)


    def set_bcs_solids_energy_eq_type(self, eq_type, in_setup=False):
        if not self.bcs_current_indices:
            return
        P = self.bcs_current_solid
        if P is None:
            return
        # Available selections:
        #  No-Flux (adiabatic) [DEFAULT]
        if eq_type == NO_FLUX:
            #    Sets keyword BC_HW_T_S(#,#) to 0.0
            hw = 0.0
            #    Sets keyword BC_C_T_S(#,#) to 0.0
            c = 0.0
            #    Sets keyword BC_TW_S(#,#) to UNDEFINED
            tw = None
        #  Specified Temperature
        elif eq_type == SPECIFIED_TEMPERATURE:
            #    Sets keyword BC_HW_T_S(#,#) to UNDEFINED
            hw = None
            #    Sets keyword BC_C_T_S(#,#) to 0.0
            c = 0.0
            #    Requires BC_TW_S(#,#)
            tw = True
        #  Specified Flux
        elif eq_type == SPECIFIED_FLUX:
            #    Sets keyword BC_HW_T_S(#,#) to 0.0
            hw = 0.0
            #    Requires BC_C_T_S(#)
            c = True
            #    Sets keyword BC_TW_S(#,#) to UNDEFINED
            tw = None
        #  Convective Flux
        elif eq_type == CONVECTIVE_FLUX:
            #    Requires BC_HW_T_S(#,#)
            hw = True
            #    Sets keyword BC_C_T_S(#,#) to 0.0
            c = 0.0
            #    Requires BC_TW_S(#,#)
            tw = True
        else:
            self.error("Invalid solid energy_eq type %s" % eq_type)
            return

        for BC in self.bcs_current_indices:
            if 'solids_energy_eq_type' not in self.bcs[BC]:
                self.bcs[BC]['solids_energy_eq_type'] = {}
            self.bcs[BC]['solids_energy_eq_type'][P] = eq_type
            for (key, val) in (('bc_hw_t_s', hw), ('bc_c_t_s', c), ('bc_tw_s', tw)):
                if val is True:
                    pass # 'required'
                else:
                    self.update_keyword(key, val, args=[BC,P])

        if not in_setup:
            self.setup_bcs_solids_tab(self.bcs_current_solid)


    def set_bcs_solids_granular_energy_eq_type(self, eq_type, in_setup=False):
        if not self.bcs_current_indices:
            return
        P = self.bcs_current_solid
        if P is None:
            return
        # Available selections:
        #  No-Flux [DEFAULT]

        if eq_type == NO_FLUX:
            #    Sets keyword BC_HW_THETA_M(#,#) to 0.0
            hw = 0.0
            #    Sets keyword BC_C_THETA_M (#,#) to 0.0
            c = 0.0
            #    Sets keyword BC_THETAW_M(#,#) to UNDEFINED
            theta = None
        #  Specified Temperature
        elif eq_type == SPECIFIED_TEMPERATURE:
            #    Sets keyword BC_HW_THETA_M(#,#) to UNDEFINED
            hw = None
            #    Sets keyword BC_C_THETA_M(#,#) to 0.0
            c = 0.0
            #    Requires BC_THETAW_M(#,#)
            theta = True
        #  Specified Flux
        elif eq_type == SPECIFIED_FLUX:
            #    Sets keyword BC_HW_THETA_M(#,#) to 0.0
            hw = 0.0
            #    Requires BC_C_THETA_M(#)
            c = True
            #    Sets keyword BC_THETAW_M(#,#) to UNDEFINED
            theta = None
        else:
            self.error("Invalid granualar energy_eq type %s" % eq_type)
            return

        for BC in self.bcs_current_indices:
            if 'solids_granular_energy_eq_type' not in self.bcs[BC]:
                self.bcs[BC]['solids_granular_energy_eq_type'] = {}
            self.bcs[BC]['solids_granular_energy_eq_type'][P] = eq_type
            for (key, val) in (('bc_hw_theta_m', hw), ('bc_c_theta_m', c), ('bc_thetaw_m', theta)):
                if val is True:
                    pass # 'required'
                else:
                    self.update_keyword(key, val, args=[BC,P])

        if not in_setup:
            self.setup_bcs_solids_tab(self.bcs_current_solid)


    def setup_bcs_fluid_tab(self):
        #Fluid (tab)
        if self.fluid_solver_disabled:
            # we shouldn't be on this tab!
            return
        if not self.bcs_current_indices:
            return # Disable inputs?
        BC0 = self.bcs_current_indices[0]
        if self.bc_is_cyclic(BC0):
            # we shouldn't be on this tab!
            return

        bc_type = self.get_bc_type(BC0)
        if bc_type is None:
            self.error("bc_type not set for region %s" % BC0)
        if bc_type.endswith('W'):
            self.setup_bcs_fluid_wall_tab()
        elif bc_type.endswith('I'):
            self.setup_bcs_fluid_inflow_tab()
        elif bc_type.endswith('PO'):
            self.setup_bcs_fluid_po_tab()
        elif bc_type.endswith('MO'):
            self.setup_bcs_fluid_mo_tab()
        else:
            self.error("Invalid bc_type %s" % bc_type)


    def setup_bcs_fluid_wall_tab(self):
        # Subtask Pane Tab for Wall type (NSW, FSW, PSW, CG_NSW, CG_FSW, CG_PSW) Boundary Condition Regions
        ui = self.ui.boundary_conditions
        ui.page_fluid.setCurrentIndex(PAGE_WALL)

        if not self.bcs_current_indices:
            return # Nothing selected.  (Clear out all lineedits?)
        BC0 = self.bcs_current_indices[0]

        bc_type = self.project.get_value('bc_type', args=[BC0])
        if bc_type is None:
            self.error("bc_type not set for region %s" % BC0)
            return

        def get_widget(key, ui):
            for pat in ('lineedit_keyword_%s_args_BC',
                        'lineedit_keyword_%s_args_BC_species',
                        'lineedit_%s_args_BC',
                        'lineedit_%s_args_BC_species'):
                widget = getattr(ui, pat % key, None)
                if widget:
                    return widget
            self.error('no widget for key %s' % key)

        def setup_key_widget(key, default=None, enabled=True,
                             species_index=None, box=None, suffix=''):
            ui = box or self.ui.boundary_conditions # widgets nested into groupboxes
            for pat in ('label_%s', 'label_%s_units',
                        'lineedit_keyword_%s_args_BC',
                        'lineedit_keyword_%s_args_BC_species'):
                name = pat%(key+suffix)
                item = getattr(ui, name, None)
                if item:
                    item.setEnabled(enabled)
            args = mkargs(key, bc=BC0, species=species_index)
            val = self.project.get_value(key, args=args)
            if val is None and enabled:
                val = default
            get_widget(key+suffix, ui).updateValue(key, val, args=args)
            for BC in self.bcs_current_indices:
                self.update_keyword(key, val, args=mkargs(key, bc=BC, species=species_index))


        def make_fluid_species_box(title, species_index):
            box = QGroupBox(title, parent=ui.page_fluid_wall)
            box.setStyleSheet("QGroupBox { font-weight: bold; } ")
            box.setFlat(True)
            box_layout = QGridLayout()
            #margins = ui.groupbox_fluid_energy_eq.getContentsMargins()
            box.setFlat(True)
            margins = (5, 10, 0, 5) #left top right bottom
            box_layout.setContentsMargins(*margins)
            box.setLayout(box_layout)
            cb = box.combobox_fluid_species_eq_type = ComboBox()
            for item in ('No-Flux', 'Specified Mass Fraction',
                         'Specified Flux', 'Convective Flux'):
                cb.addItem(item)
            cb.setCurrentIndex(NO_FLUX)
            cb.currentIndexChanged.connect(
                lambda index, species_index=species_index: self.set_bcs_fluid_species_eq_type(index, species_index))
            hbox = QHBoxLayout()
            label = QLabel('Type')
            # TODO: tooltips on label and combobox
            hbox.addWidget(label)
            hbox.addWidget(cb)
            hbox.addStretch()
            box_layout.addLayout(hbox, 0, 0, 1, 3)
            row = 0
            for (label_text, key, units) in (('Wall mass fraction', 'bc_xw_g', None),
                                             ('Constant flux', 'bc_c_x_g', '/m'),
                                             ('Transfer coefficient', 'bc_hw_x_g', None),
                                             ('Free stream mass frac.', 'bc_xw_g', None)):
                row += 1
                suffix = '_2' if label_text.startswith('Free') else ''
                label = QLabel(label_text)
                self.add_tooltip(label, key)
                box_layout.addWidget(label, row, 0, 1, 1)
                setattr(box, 'label_%s'%(key+suffix), label)
                le = LineEdit()
                le.key = key
                le.args = ['BC', species_index]
                le.dtype = float
                le.allow_parameters = True
                # TODO: LIMITS
                self.add_tooltip(le, key)
                box_layout.addWidget(le, row, 1)
                setattr(box, 'lineedit_keyword_%s_args_BC_species'%(key+suffix), le)
                self.project.register_widget(le, [key], ['BC', species_index])
                if units:
                    label = QLabel(units)
                    box_layout.addWidget(label, row, 2)
                    setattr(box, 'label_%s_units'%(key+suffix), label)
            page_layout = ui.page_fluid_wall.layout()
            page_layout.insertWidget(page_layout.count()-1, box)
            return box

        #Fluid (tab)

        #    Define transfer coefficient
        # Specification only available with PSW/CG_PSW
        # Sets keyword BC_HW_G(#)
        # DEFAULT 0.0

        #    Define Wall U-velocity
        # Specification only available with PSW/CG_PSW
        # Sets keyword BC_UW_G(#)
        # DEFAULT 0.0

        #    Define Wall V-velocity
        # Specification only available with PSW/CG_PSW
        # Sets keyword BC_VW_G(#)
        # DEFAULT 0.0

        #    Define Wall W-velocity
        # Specification only available with PSW/CG_PSW
        # Sets keyword BC_WW_G(#)
        # DEFAULT 0.0

        enabled = (bc_type in ('PSW','CG_PSW'))
        default = 0.0 if enabled else None
        for c in 'huvw':
            key = 'bc_%sw_g' % c
            setup_key_widget(key, default, enabled)
        # Enable/disable entire groupbox
        ui.groupbox_fluid_momentum_eq.setEnabled(enabled) # Hide it???

        #Select energy equation boundary type:
        # Selection only available when solving energy equations
        energy_eq = self.project.get_value('energy_eq', default=True)
        enabled = bool(energy_eq)
        ui.groupbox_fluid_energy_eq.setEnabled(enabled)
        eq_type = None
        if enabled:
            eq_type = self.bcs[BC0].get('fluid_energy_eq_type')
            if eq_type is None: # Attempt to infer from keywords
                hw = self.project.get_value('bc_hw_t_g', args=[BC0])
                c = self.project.get_value('bc_c_t_g', args=[BC0])
                tw = self.project.get_value('bc_tw_g', args=[BC0])
                if hw==0.0 and c==0.0 and tw is None:
                    eq_type = NO_FLUX
                elif hw is None and c==0.0 and tw is not None:
                    eq_type = SPECIFIED_TEMPERATURE
                elif hw==0.0 and c!=0.0 and tw is None:
                    eq_type = SPECIFIED_FLUX
                elif hw is not None and c==0.0 and tw is not None:
                    eq_type = CONVECTIVE_FLUX
                else:
                    #self.error("Cannot determine type for fluid energy boundary equation %s" % BC0)
                    eq_type = NO_FLUX # Default
                    self.set_bcs_fluid_energy_eq_type(eq_type, in_setup=True)

            if eq_type is not None:
                ui.combobox_fluid_energy_eq_type.setCurrentIndex(eq_type)

        if energy_eq:
            #Define wall temperature
            # Specification only available with 'Specified Temperature' BC type
            # Sets keyword BC_TW_G(#)
            # DEFAULT 293.15
            enabled = (eq_type==SPECIFIED_TEMPERATURE)
            key = 'bc_tw_g'
            default = 293.15 if enabled else None
            setup_key_widget(key, default, enabled)
            # Hack to prevent dup. display
            if enabled:
                ui.lineedit_keyword_bc_tw_g_2_args_BC.setText('')
            else:
                ui.lineedit_keyword_bc_tw_g_args_BC.setText('')
            #Define constant flux
            # Specification only available with 'Specified Flux' BC type
            # Sets keyword BC_C_T_G(#)
            # DEFAULT 0.0
            enabled = (eq_type==SPECIFIED_FLUX)
            key = 'bc_c_t_g'
            default = 0.0
            setup_key_widget(key, default, enabled)

            #Define transfer coefficient
            # Specification only available with 'Convective Flux' BC type
            # Sets keyword BC_HW_T_G(#)
            # DEFAULT 0.0
            enabled = (eq_type==CONVECTIVE_FLUX)
            key = 'bc_hw_t_g'
            default = 0.0
            setup_key_widget(key, default, enabled)

            #Define free stream temperature
            # Specification only available with 'Convective Flux' BC type
            # Sets keyword BC_TW_G(#)
            # DEFAULT 0.0
            enabled = (eq_type==CONVECTIVE_FLUX)
            key = 'bc_tw_g'
            default = 0.0 if enabled else None
            setup_key_widget(key, default, enabled, suffix='_2')
            # Hack to prevent dup. display
            if enabled:
                ui.lineedit_keyword_bc_tw_g_args_BC.setText('')
            else:
                ui.lineedit_keyword_bc_tw_g_2_args_BC.setText('')

        #Select species equation boundary type:
        # Selection only available when solving species equations
        species_eq = self.project.get_value('species_eq', default=True, args=[0])#0 for fluid-phase
        enabled = bool(species_eq)

        #  Note - we need a copy of this groupbox for each fluid species
        # Let's only do this when the species have changed
        box_keys = list(self.bcs_fluid_species_boxes.keys())
        species_names = list(self.fluid_species.keys())
        species_keys = [self.project.get_value('species_alias_g', default=name, args=[i])
                           for (i, name) in enumerate(species_names, 1)]
        if box_keys != species_keys:
            n_boxes = len(box_keys)
            n_species = len(species_keys)
            if n_boxes > n_species:
                for i in range(n_boxes-1, n_species-1, -1):
                    box_key = box_keys[i]
                    box = self.bcs_fluid_species_boxes[box_key]
                    for w in widget_iter(box):
                        self.project.unregister_widget(w)
                        w.deleteLater()
                    box.hide()
                    box = None
                    self.bcs_fluid_species_boxes[box_key].deleteLater()

            elif n_boxes < n_species:
                for i in range(n_boxes, n_species):
                    species_key = species_keys[i]
                    box = make_fluid_species_box(species_key, i+1)
                    self.bcs_fluid_species_boxes[species_key] = box

            tmp_dict = OrderedDict()
            for (key, box) in zip(species_keys, self.bcs_fluid_species_boxes.values()):
                tmp_dict[key] = box
                box.setTitle("Species Equation: %s" % key)
            self.bcs_fluid_species_boxes = tmp_dict

        for (species_index, box) in enumerate(self.bcs_fluid_species_boxes.values(), 1):
            enabled = bool(species_eq)
            box.setEnabled(enabled)
            eq_type = None
            if enabled:
                eq_type = self.bcs[BC0].get('fluid_species_eq_type', {}).get(species_index)
                if eq_type is None: # Attempt to infer from keywords
                    hw = self.project.get_value('bc_hw_x_g', args=[BC0, species_index])
                    c = self.project.get_value('bc_c_x_g', args=[BC0, species_index])
                    xw = self.project.get_value('bc_xw_g', args=[BC0, species_index])
                    if hw==0.0 and c==0.0 and xw is None:
                        eq_type = NO_FLUX
                    elif hw is None and c==0.0 and xw is not None:
                        eq_type = SPECIFIED_MASS_FRACTION
                    elif hw==0.0 and c!=0.0 and xw is None:
                        eq_type = SPECIFIED_FLUX
                    elif hw is not None and c==0.0 and xw is not None:
                        eq_type = CONVECTIVE_FLUX
                    else:
                        #self.error("Cannot determine type for fluid species boundary equation %s" % BC0)
                        eq_type = NO_FLUX # Default
                    self.set_bcs_fluid_species_eq_type(eq_type, species_index, in_setup=True)

            if eq_type is not None:
                box.combobox_fluid_species_eq_type.setCurrentIndex(eq_type)

            if species_eq:
                #Define wall mass fraction
                # Specification only available with 'Specified Mass Fraction' BC type
                # Sets keyword BC_XW_G(#)
                # DEFAULT 0.0
                enabled = (eq_type==SPECIFIED_MASS_FRACTION)
                key = 'bc_xw_g'
                default = 0.0 if enabled else None
                setup_key_widget(key, default, enabled, species_index=species_index, box=box)
                # Hack to prevent dup. display
                if enabled:
                    box.lineedit_keyword_bc_xw_g_2_args_BC_species.setText('')
                else:
                    box.lineedit_keyword_bc_xw_g_args_BC_species.setText('')

                #Define constant flux
                # Specification only available with 'Specified Flux' BC type
                # Sets keyword BC_C_X_G(#)
                # DEFAULT 0.0
                enabled = (eq_type==SPECIFIED_FLUX)
                key = 'bc_c_x_g'
                default = 0.0
                setup_key_widget(key, default, enabled, species_index=species_index, box=box)

                #Define transfer coefficient
                # Specification only available with 'Convective Flux' BC type
                # Sets keyword BC_HW_X_G(#)
                # DEFAULT 0.0
                enabled = (eq_type==CONVECTIVE_FLUX)
                key = 'bc_hw_x_g'
                default = 0.0
                setup_key_widget(key, default, enabled, species_index=species_index, box=box)

                #Define free stream mass fraction
                # Specification only available with 'Convective Flux' BC type
                # Sets keyword BC_XW_G(#)
                # DEFAULT 0.0
                enabled = (eq_type==CONVECTIVE_FLUX)
                key = 'bc_xw_g'
                default = 0.0 if enabled else None
                setup_key_widget(key, default, enabled, species_index=species_index, box=box, suffix='_2')
                # Hack to prevent dup. display
                if enabled:
                    box.lineedit_keyword_bc_xw_g_args_BC_species.setText('')
                else:
                    box.lineedit_keyword_bc_xw_g_2_args_BC_species.setText('')


    def setup_bcs_solids_tab(self, P):
        #Solids-# (tab) - (Replace with phase name defined by the user)
        # Note, solids phases are numbered 1-N
        self.bcs_current_solid = self.P = P
        if P is None: # Nothing to do
            return

        if not self.bcs_current_indices: # No region selected
            # TODO clear all widgets (?)
            return
        BC0 = self.bcs_current_indices[0]

        bc_type = self.project.get_value('bc_type', args=[BC0])
        if bc_type is None:
            self.error("bc_type not set for region %s" % BC0)
            return
        if bc_type.endswith('W'):
            self.setup_bcs_solids_wall_tab(P)
        elif bc_type.endswith('I'):
            self.setup_bcs_solids_inflow_tab(P)
        elif bc_type.endswith('PO'):
            self.setup_bcs_solids_po_tab(P)
        elif bc_type.endswith('MO'):
            self.setup_bcs_solids_mo_tab(P)
        else:
            self.error("Invalid bc_type %s" % bc_type)


    def setup_bcs_solids_wall_tab(self, P):
        #Subtask Pane Tab for Wall type (NSW, FSW, PSW, CG_NSW, CG_FSW, CG_PSW) Boundary
        #Comment on Solids Wall BCs: Most of the solids wall BCs are only needed for TFM solids. PIC
        # does not support ANY of the wall BC specifications. DEM only supports keywords associated with
        # the energy equations.
        #
        #Solids-# (tab) - (Replace with phase name defined by the user)
        # Note, solids phases are numbered 1-N

        ui = self.ui.boundary_conditions
        ui.page_solids.setCurrentIndex(PAGE_WALL)

        self.bcs_current_solid = self.P = P
        if P is None: # Nothing to do
            return

        if not self.bcs_current_indices: # No region selected
            # TODO clear all widgets (?)
            return

        BC0 = self.bcs_current_indices[0]

        bc_type = self.project.get_value('bc_type', args=[BC0])
        if bc_type is None:
            self.error("bc_type not set for region %s" % BC0)
            return

        def get_widget(key):
            for pat in ('lineedit_keyword_%s_args_BC_P',
                        'lineedit_keyword_%s_args_BC',
                        'lineedit_%s_args_BC_P',
                        'lineedit_%s_args_BC',
                        'lineedit_keyword_%s',
                        'lineedit_%s'):
                widget = getattr(ui, pat % key, None)
                if widget:
                    return widget
            self.error('no widget for key %s' % key)

        def setup_key_widget(key, default=None, enabled=True, suffix=''):
            for pat in ('label_%s', 'label_%s_units',
                        'lineedit_keyword_%s_args_BC_P',
                        'lineedit_keyword_%s_args_BC',
                        'lineedit_%s_args_BC_P',
                        'lineedit_%s_args_BC',
                        'lineedit_keyword_%s',
                        'lineedit_%s'):
                name = pat % (key+suffix)
                item = getattr(ui, name, None)
                if item:
                    item.setEnabled(enabled)
            args = mkargs(key, bc=BC0, phase=P)
            val = self.project.get_value(key, args=args)
            if val is None and enabled:
                val = default
            get_widget(key+suffix).updateValue(key, val, args=args)
            for BC in self.bcs_current_indices:
                self.update_keyword(key, val, args=mkargs(key, bc=BC, phase=P))

        #    Enable Jackson-Johnson partial slip boundary
        # Disabled (0.0) for CARTESIAN_GRID = .TRUE.
        # Disabled for DEM and PIC solids
        # Disabled (0.0) for KT_TYPE = 'ALGEBRAIC'
        # Disabled (0.0) for KT_TYPE = 'GHD_2007'
        # Sets keyword BC_JJ_PS(#)
        # DEFAULT 1.0 when not disabled
        #
        # Note, according to doc it's an int, not float
        cartesian_grid = bool(self.project.get_value('cartesian_grid', default=False))
        kt_type = self.project.get_value('kt_type', default=DEFAULT_KT_TYPE)
        solids_model = self.project.get_value('solids_model', args=[P])
        enabled = (bool(cartesian_grid)==False
                   and kt_type not in ('ALGEBRAIC', 'GHD_2007')
                   and solids_model == 'TFM')
        key = 'bc_jj_ps'
        default = 1 if enabled else 0
        widget = ui.checkbox_bc_jj_ps_args_BC
        val = self.project.get_value(key, default, args=BC0) if enabled else 0
        if val and not enabled:
            val = 0
            for BC in self.bcs_current_indices:
                self.update_keyword('bc_jj_ps', 0, args=[BC])
        if enabled: # Default 1.0 when not disabled
            for BC in self.bcs_current_indices:
                self.update_keyword('bc_jj_ps', val, args=[BC])
        widget.setChecked(val)

        widget.setEnabled(enabled)
        # TODO should this also depend on energy_eq ?
        ui.groupbox_solids_momentum_eq.setEnabled(enabled or bc_type in ('PSW', 'CG_PSW'))

        #    Select type of Jackson and Johnson BC:
        # Disabled for DEM and PIC solids
        # Selection only available BC_JJ_PS(#) = 1.0
        bc_jj_ps = int(self.project.get_value('bc_jj_ps', default=0, args=BC0))
        enabled = (bc_jj_ps==1) and solids_model not in ('DEM', 'PIC')
        for widget in (ui.label_bc_jj_ps_type, ui.combobox_bc_jj_ps_type):
            widget.setEnabled(enabled)

        # Available selections:
        #  Default Jackson-Johnson BC [DEFAULT]
        #    Sets keyword BC_JJ_M to .FALSE.
        #    Sets keyword JENKINS to .FALSE.
        #  Variable specularity coefficient
        #    Sets keyword BC_JJ_M to .TRUE.
        #    Sets keyword JENKINS to .FALSE.
        #  Jenkins small frictional boundary
        #    Sets keyword BC_JJ_M to .FALSE.
        #    Sets keyword JENKINS to .TRUE.
        bc_jj_m = self.project.get_value('bc_jj_m', default=False)
        jenkins = self.project.get_value('jenkins', default=False)
        val = 2*jenkins + bc_jj_m
        if val > 2:
            self.warn("boundary condition %s: invalid combination of bc_jj_m=%s, jenkins=%s" %
                      (BC0, bc_jj_m, jenkins))
        else:
            widget.setCurrentIndex(val)

        #Define restitution coefficient
        # Disabled for DEM and PIC solids
        # Specification only available with BC_JJ_PS(#) = 1.0
        # Sets keyword E_W
        # DEFAULT 1.0
        # Required when available # TODO implement 'required'
        enabled = (bc_jj_ps==1) and solids_model not in ('DEM', 'PIC')
        key = 'e_w'
        default = 1.0
        setup_key_widget(key, default, enabled)

        #Define specularity coefficient
        # Disabled for DEM and PIC solids
        # Specification only available with BC_JJ_PS(#)=1.0 and JENKINS=.FALSE.
        # Sets keyword PHIP
        # DEFAULT 0.6
        # Required when available
        jenkins = self.project.get_value('jenkins', default=False)
        enabled = bc_jj_ps==1 and jenkins==False and solids_model not in ('DEM', 'PIC')
        key = 'phip'
        default = 0.6
        setup_key_widget(key, default, enabled)

        #Define specularity coefficient at zero slip
        # Disabled for DEM and PIC solids
        # Specification only available with BC_JJ_PS(#)=1.0 and BC_JJ_M=.TRUE.
        # Sets keyword PHIP0
        # DEFAULT -blank
        # Optional when available
        bc_jj_m = self.project.get_value('bc_jj_m')
        enabled = bc_jj_ps==1 and bc_jj_m==True and solids_model not in ('DEM', 'PIC')
        key = 'phip0'
        default = None
        setup_key_widget(key, default, enabled)

        #Define angle of internal friction
        # Disabled for DEM and PIC solids
        # Sets keyword PHI_W
        # Specification only available with BC_JJ_PS(#)=1.0 and (JENKINS=.TRUE.  or FRICTION_MODEL=SRIVASTAVA)
        friction_model = self.project.get_value('friction_model', default=DEFAULT_FRICTION_MODEL)
        enabled = (solids_model not in ('DEM', 'PIC') and
                   bc_jj_ps==1 and
                   (jenkins or friction_model=='SRIVASTAVA'))

        # DEFAULT 11.31 = atan(0.2) * (180/pi)
        key = 'phi_w'
        default = 11.31 # Equation('atan(0.2) * (180/pi)')
        # Required when available
        setup_key_widget(key, default, enabled)

        #Define transfer coefficient
        # Disabled for DEM and PIC solids
        # Specification only available with PSW/CG_PSW
        # Sets keyword BC_HW_S(#,#)
        # DEFAULT 0.0
        key = 'bc_hw_s'
        enabled = (bc_type in ('PSW', 'CG_PSW')
                   and solids_model not in ('DEM', 'PIC'))
        default = 0.0 if enabled else None
        setup_key_widget(key, default, enabled)

        #Define Wall U-velocity
        # Disabled for DEM and PIC solids
        # Specification only available with PSW/CG_PSW or BC_JJ_PS(#) = 1.0
        # Sets keyword BC_UW_S(#,#)
        # DEFAULT 0.0
        #
        #Define Wall V-velocity
        # Specification only available with PSW/CG_PSW or BC_JJ_PS(#) = 1.0
        # Sets keyword BC_VW_S(#,#)
        # DEFAULT 0.0
        #
        #Define Wall W-velocity
        # Specification only available with PSW/CG_PSW or BC_JJ_PS(#) = 1.0
        # Sets keyword BC_WW_S(#,#)
        # DEFAULT 0.0
        enabled = ((bc_type in ('PSW', 'CG_PSW') or (bc_jj_ps==1))
                   and solids_model not in ('DEM', 'PIC'))
        default = 0 if enabled else None
        for c in 'uvw':
            key = 'bc_%sw_s' % c
            setup_key_widget(key, default, enabled)

        #Select energy equation boundary type:
        # Disabled for PIC solids
        # Selection only available when solving energy equations
        energy_eq = self.project.get_value('energy_eq', default=True)
        enabled = bool(energy_eq)==True and solids_model != 'PIC'
        ui.groupbox_solids_energy_eq.setEnabled(enabled)
        eq_type = None
        if enabled:
            eq_type = self.bcs[BC0].get('solids_energy_eq_type',{}).get(P)
            if eq_type is None: # Attempt to infer from keywords
                hw = self.project.get_value('bc_hw_t_s', args=[BC0,P])
                c =  self.project.get_value('bc_c_t_s', args=[BC0,P])
                tw = self.project.get_value('bc_tw_s', args=[BC0,P])
                # Available selections:
                #  No-Flux (adiabatic) [DEFAULT]
                #    Sets keyword BC_HW_T_S(#,#) to 0.0
                #    Sets keyword BC_C_T_S(#,#) to 0.0
                #    Sets keyword BC_TW_S(#,#) to UNDEFINED
                if hw==0.0 and c==0.0 and tw==None:
                    eq_type = NO_FLUX
                #  Specified Temperature
                #    Sets keyword BC_HW_T_S(#,#) to UNDEFINED
                #    Sets keyword BC_C_T_S(#,#) to 0.0
                #    Requires BC_TW_S(#,#)
                elif hw is None and c==0.0 and tw is not None:
                    eq_type = SPECIFIED_TEMPERATURE
                #  Specified Flux
                #    Sets keyword BC_HW_T_S(#,#) to 0.0
                #    Requires BC_C_T_S(#)
                #    Sets keyword BC_TW_S(#,#) to UNDEFINED
                elif hw==0.0 and c is not None and tw is None:
                    eq_type = SPECIFIED_FLUX
                #  Convective Flux
                #    Disabled for DEM and PIC solids
                #    Requires BC_HW_T_S(#,#)
                #    Sets keyword BC_C_T_S(#,#) to 0.0
                #    Requires BC_TW_S(#,#)
                elif hw is not None and c==0.0 and tw is None:
                    eq_type = CONVECTIVE_FLUX
                    if solids_model in ('PIC', 'DEM'):
                        self.error("Convective flux not allowed for %s solids" % solids_model)
                        # What to do?
                else:
                    #self.error("Cannot determine type for solid %s energy boundary equation %s" % (P,BC0))
                    eq_type = NO_FLUX # Default
                    self.set_bcs_solids_energy_eq_type(eq_type, in_setup=True)

            cb = ui.combobox_solids_energy_eq_type
            if eq_type is not None:
                cb.setCurrentIndex(eq_type)
            #  Convective Flux
            #    Disabled for DEM and PIC solids
            set_item_enabled(get_combobox_item(cb, CONVECTIVE_FLUX),
                             solids_model not in ('DEM', 'PIC'))

        if energy_eq:
            #Define wall temperature
            # Disabled for PIC solids
            # Specification only available with 'Specified Temperature' BC type
            # Sets keyword BC_TW_S(#,#)
            # DEFAULT 293.15
            enabled = (eq_type==SPECIFIED_TEMPERATURE) and solids_model != 'PIC'
            key = 'bc_tw_s'
            default = 293.15 if enabled else None
            setup_key_widget(key, default, enabled)
            # Hack to prevent dup. display
            if enabled:
                ui.lineedit_keyword_bc_tw_s_2_args_BC_P.setText('')
            else:
                ui.lineedit_keyword_bc_tw_s_args_BC_P.setText('')

            #Define constant flux
            # Disabled for PIC solids
            # Specification only available with 'Specified Flux' BC type
            # Sets keyword BC_C_T_S(#,#)
            # DEFAULT 0.0
            enabled = (eq_type==SPECIFIED_FLUX) and solids_model != 'PIC'
            key = 'bc_c_t_s'
            default = 0.0
            setup_key_widget(key, default, enabled)

            #Define transfer coefficient
            # Disabled for PIC solids
            # Specification only available with 'Convective Flux' BC type
            # Sets keyword BC_HW_T_S(#,#)
            # DEFAULT 0.0
            enabled = (eq_type==CONVECTIVE_FLUX) and solids_model != 'PIC'
            key = 'bc_hw_t_s'
            default = 0.0
            setup_key_widget(key, default, enabled)

            #Define free stream temperature
            # Disabled for PIC solids
            # Specification only available with 'Convective Flux' BC type
            # Sets keyword BC_TW_S(#,#)
            # DEFAULT 0.0
            enabled = (eq_type==CONVECTIVE_FLUX) and solids_model != 'PIC'
            key = 'bc_tw_s'
            default = 0.0 if enabled else None
            setup_key_widget(key, default, enabled, suffix='_2')
            # Hack to prevent dup. display
            if enabled:
                ui.lineedit_keyword_bc_tw_s_args_BC_P.setText('')
            else:
                ui.lineedit_keyword_bc_tw_s_2_args_BC_P.setText('')

        # FIXME should this also depend on 'granular_energy' keyword ?
        #Select granular energy equation boundary type:
        # Disabled for DEM and PIC solids
        # Selection only available with BC_JJ_PS(#)=0.0 and KT_TYPE /= 'ALGEBRAIC'
        enabled = (bc_jj_ps==0) and (kt_type != 'ALGEBRAIC') and solids_model not in ('DEM', 'PIC')
        ui.groupbox_solids_granular_energy_eq.setEnabled(enabled)
        eq_type = None
        if enabled:
            eq_type = self.bcs[BC0].get('solids_granular_energy_eq_type',{}).get(P)
            if eq_type is None:
                hw = self.project.get_value('bc_hw_theta_m', args=[BC0,P])
                c =  self.project.get_value('bc_c_theta_m', args=[BC0,P])
                theta =  self.project.get_value('bc_thetaw_m', args=[BC0,P])
                #  No-Flux [DEFAULT]
                #    Sets keyword BC_HW_THETA_M(#,#) to 0.0
                #    Sets keyword BC_C_THETA_M (#,#) to 0.0
                #    Sets keyword BC_THETAW_M(#,#) to UNDEFINED
                if (hw==0.0) and (c==0.0) and (theta is None):
                    eq_type = NO_FLUX
                #  Specified Temperature
                #    Sets keyword BC_HW_THETA_M(#,#) to UNDEFINED
                #    Sets keyword BC_C_THETA_M(#,#) to 0.0
                #    Requires BC_THETAW_M(#,#)
                elif (hw is None) and (c==0.0) and theta is not None:
                    eq_type = SPECIFIED_TEMPERATURE
                #  Specified Flux
                #    Sets keyword BC_HW_THETA_M(#,#) to 0.0
                #    Requires BC_C_THETA_M(#)
                #    Sets keyword BC_THETAW_M(#,#) to UNDEFINED
                elif (hw==0.0) and (c is not None) and (theta is None):
                    eq_type = SPECIFIED_FLUX
                else:
                    #self.error("Cannot determine type for solid %s granular energy boundary equation %s" % (P,BC0))
                    eq_type = NO_FLUX # Default
                    self.set_bcs_solids_granular_energy_eq_type(eq_type, in_setup=True)

            if eq_type is not None:
                ui.combobox_solids_granular_energy_eq_type.setCurrentIndex(eq_type)

        if enabled:
            #Define granular temperature
            # Disabled for DEM and PIC solids
            # Specification only available with 'Specified Temperature' BC type
            # Sets keyword BC_THETAW_M(#,#)
            # DEFAULT 0.0
            enabled = (eq_type==SPECIFIED_TEMPERATURE) and solids_model not in ('DEM', 'PIC')
            key = 'bc_thetaw_m'
            default = 0.0
            setup_key_widget(key, default, enabled)

            #Define constant flux
            # Disabled for DEM and PIC solids
            # Specification only available with 'Specified Flux' BC type
            # Sets keyword BC_C_THETA_M(#,#)
            # DEFAULT 0.0
            enabled = (eq_type==SPECIFIED_FLUX) and solids_model not in ('DEM', 'PIC')
            key = 'bc_c_theta_m'
            default = 0.0
            setup_key_widget(key, default, enabled)


    def setup_bcs_scalar_tab(self):
        #Scalar (tab) - Tab only available if scalar equations are solved
        nscalar = self.project.get_value('nscalar', default=0)
        if nscalar==0: # We shouldn't be here
            # Clear out pane?
            return
        if not self.bcs_current_indices: # nothing selected
            return
        BC0 = self.bcs_current_indices[0]
        bc_type = self.project.get_value('bc_type', args=BC0)
        if bc_type is None:
            self.error("bc_type not set for region %s" % BC0)
            return

        if bc_type.endswith('W'):
            self.setup_bcs_scalar_wall_tab()
        elif bc_type.endswith('I'):
            self.setup_bcs_scalar_inflow_tab()
        elif bc_type.endswith('PO'):
            self.setup_bcs_scalar_po_tab()
        elif bc_type.endswith('MO'):
            self.setup_bcs_scalar_mo_tab()
        else:
            self.error("Invalid bc_type %s" % bc_type)


    def clear_bcs_scalar_tab(self):
        # returns spacer item, if present, so it can be re-added
        ui = self.ui.boundary_conditions
        page = ui.page_scalar
        page_layout = page.layout()

        spacer = None
        for i in range(page_layout.count()-1, -1, -1):
            item = page_layout.itemAt(i)
            if not item:
                continue
            widget = item.widget()
            if not widget:
                spacer = item
                continue
            if isinstance(widget, LineEdit):
                self.project.unregister_widget(widget)
            widget.setParent(None)
            for w2 in widget_iter(widget):
                if isinstance(w2, (LineEdit, ComboBox)):
                    self.project.unregister_widget(w2)
            widget.deleteLater()

        if spacer:
            page_layout.removeItem(spacer)
        return spacer


    def set_bcs_scalar_eq_type(self, eq_type, i, in_setup=False):
        ui = self.ui.boundary_conditions
        if not self.bcs_current_indices:
            return
        cb = getattr(ui, 'combobox_scalar_%s_eq_type' % i, None)
        if cb is None:
            self.error("Invalid scalar_eq %s" % i)
            return
        # Available selections
        #  No-Flux [DEFAULT]
        if eq_type == NO_FLUX:
            #    Sets keyword BC_HW_SCALAR(#,#) to 0.0
            hw = 0.0
            #    Sets keyword BC_C_SCALAR(#,#) to 0.0
            c = 0.0
            #    Sets keyword BC_SCALARW(#,#) to UNDEFINED
            sw = None
        #  Specified Temperature
        elif eq_type == SPECIFIED_TEMPERATURE:
            #    Sets keyword BC_HW_T_S(#,#) to UNDEFINED
            #                 ^^^^ presumably BC_HW_SCALAR
            hw = None
            #    Sets keyword BC_C_SCALAR (#,#) to 0.0
            c = 0.0
            #    Requires BC_SCALARW (#,#)
            sw = True
        #  Specified Flux
        elif eq_type == SPECIFIED_FLUX:
            #    Sets keyword BC_HW_T_S(#,#) to 0.0
            #                 ^^^^ presumably BC_HW_SCALAR
            hw = 0.0
            #    Requires BC_C_SCALAR (#)
            c = True
            #    Sets keyword BC_SCALARW (#,#) to UNDEFINED
            sw = None
        #  Convective Flux
        elif eq_type == CONVECTIVE_FLUX:
            #    Requires BC_HW_T_S(#,#)
            #                 ^^^^ presumably BC_HW_SCALAR
            hw = True
            #    Sets keyword BC_C_SCALAR (#,#) to 0.0
            c = 0.0
            #    Requires BC_SCALARW (#,#)
            sw = True
        else:
            self.error("Invalid scalar energy_eq type %s" % eq_type)
            return

        for BC in self.bcs_current_indices:
            # memoize value.
            if 'scalar_eq_type' not in self.bcs[BC]:
                self.bcs[BC]['scalar_eq_type'] = {}
            self.bcs[BC]['scalar_eq_type'][i] = eq_type

            for (key, val) in (('bc_hw_scalar', hw), ('bc_c_scalar', c), ('bc_scalarw', sw)):
                if val is True:
                    pass # 'required'
                else:
                    self.update_keyword(key, val, args=[BC,i])

        self.setup_bcs_scalar_tab()


    def setup_bcs_scalar_wall_tab(self):
        # Subtask Pane Tab for Wall type (NSW, FSW, PSW, CG_NSW, CG_FSW, CG_PSW) Boundary
        #Scalar (tab) - Tab only available if scalar equations are solved
        #Note that this tab should only be available if scalar
        #equations are being solved.
        if not self.bcs_current_indices:
            return # No selection
        BC0 = self.bcs_current_indices[0]

        ui = self.ui.boundary_conditions
        nscalar = self.project.get_value('nscalar', default=0)
        old_nscalar = getattr(ui, 'nscalar_wall', None)
        old_scalar_bc_type = getattr(ui, 'scalar_bc_type', None)
        ui.nscalar_wall = nscalar
        ui.scalar_bc_type = 'WALL'

        if nscalar == 0:
            # tab is disabled
            return

        page = ui.page_scalar
        page_layout = page.layout()

        spacer = None
        if nscalar != old_nscalar or old_scalar_bc_type != 'WALL':
            spacer = self.clear_bcs_scalar_tab()

            for i in range(1, nscalar+1):
                groupbox = QGroupBox('Scalar %s' % i)
                groupbox.setStyleSheet("QGroupBox { font-weight: bold; } ")
                groupbox.setFlat(True)
                page_layout.addWidget(groupbox, i-1, 0, 1, -1)
                groupbox_layout = QGridLayout()
                groupbox_layout.setContentsMargins(5, 0, 0, 5)
                groupbox.setLayout(groupbox_layout)
                #  Select scalar boundary type:
                hbox = QHBoxLayout()
                label = QLabel("Type")
                hbox.addWidget(label)
                cb = ComboBox()
                setattr(ui, "combobox_scalar_%s_eq_type"%i, cb)
                # Available selections:
                cb.addItem("No-Flux")
                cb.addItem("Specified Temperature")
                cb.addItem("Specified Flux")
                cb.addItem("Convective Flux")
                cb.setCurrentIndex(NO_FLUX)
                cb.currentIndexChanged.connect(lambda eq_type, i=i: self.set_bcs_scalar_eq_type(eq_type, i))
                hbox.addWidget(cb)
                hbox.addStretch()

                groupbox_layout.addLayout(hbox, 0, 0, 1, -1)

                row = 0
                for (key, label_text, suffix) in (
                        ('bc_scalarw', 'Wall value', ''),
                        ('bc_c_scalar', 'Constant flux', ''),
                        ('bc_hw_scalar', 'Transfer coefficient', ''),
                        ('bc_scalarw', 'Free stream value', '_2')):
                    row += 1 # Skip over "type"
                    args = ['BC', i]
                    label = QLabel(label_text)
                    self.add_tooltip(label, key)
                    setattr(ui, 'label_%s_args_BC_%s' % (key+suffix, i), label)
                    groupbox_layout.addWidget(label, row, 0)
                    le = LineEdit()
                    le.key = key
                    le.args = ['BC', i]
                    le.dtype = float
                    le.allow_parameters = True
                    self.add_tooltip(le, key)
                    le.default_value = 0.0 #?
                    groupbox_layout.addWidget(le, row, 1)
                    self.project.register_widget(le, [key], ['BC', i])
                    setattr(ui, 'lineedit_keyword_%s_args_BC_%s' % (key+suffix, i), le)

                    units = '/m' if ('_c_' in key or '_hw_' in key) else '' # Scalars are dimensionless
                    label = QLabel(units)
                    groupbox_layout.addWidget(label, row, 2)

                    for col in (0, 1, 2):
                        groupbox_layout.setColumnStretch(col, col==1)

        # Clear memoized data above current nscalar if nscalar decreased
        if old_nscalar is None:
            old_nscalar = 0
        for i in range(nscalar+1, old_nscalar+2):
            for bc in self.bcs.values():
                d = bc.get('scalar_eq_type')
                if d:
                    d.pop(i,None)


        def get_widget(key, i):
            for pat in ('lineedit_keyword_%s_args_BC_%s',
                        'lineedit_%s_args_BC_%s'):
                name = pat % (key, i)
                widget = getattr(ui, name, None)
                if widget:
                    return widget
            self.error('no widget for key %s' % key)

        def setup_key_widget(key, i, default=None, enabled=True, suffix=''):
            for pat in ('label_%s_args_BC_%s',
                         'lineedit_keyword_%s_args_BC_%s'):
                name = pat%(key+suffix, i)
                item = getattr(ui, name, None)
                if item:
                    item.setEnabled(enabled)
            args = mkargs(key, bc=BC0, scalar=i)
            val = self.project.get_value(key, args=args)
            if val is None and enabled:
                val = default
            get_widget(key+suffix, i).updateValue(key, val, args=args)
            for BC in self.bcs_current_indices:
                self.update_keyword(key, val, args=mkargs(key, bc=BC, scalar=i))

        for i in range(1, nscalar+1):
            hw = self.project.get_value('bc_hw_scalar', args=[BC0,i])
            c = self.project.get_value('bc_c_scalar', args=[BC0,i])
            sw = self.project.get_value('bc_scalarw', args=[BC0,i])
            eq_type = self.bcs[BC0].get('scalar_eq_type',{}).get(i)
            cb = getattr(ui, 'combobox_scalar_%s_eq_type'%i)
            if eq_type is None:
                #  No-Flux [DEFAULT]
                #    Sets keyword BC_HW_SCALAR(#,#) to 0.0
                #    Sets keyword BC_C_SCALAR(#,#) to 0.0
                #    Sets keyword BC_SCALARW(#,#) to UNDEFINED
                if (hw==0) and (c==0) and (sw is None):
                    eq_type = NO_FLUX
                #  Specified Temperature
                #    Sets keyword BC_HW_SCALAR(#,#) to UNDEFINED
                #    Sets keyword BC_C_SCALAR (#,#) to 0.0
                #    Requires BC_SCALARW (#,#)
                elif (hw is None) and (c==0.0) and (sw is not None):
                    eq_type = SPECIFIED_TEMPERATURE
                #  Specified Flux
                #    Sets keyword BC_HW_T_S(#,#) to 0.0
                #                 ^^^^ presumably BC_HW_SCALAR
                #    Requires BC_C_SCALAR (#)
                #    Sets keyword BC_SCALARW (#,#) to UNDEFINED
                elif (hw==0.0) and (c is not None) and (sw is None):
                    eq_type = SPECIFIED_FLUX
                #  Convective Flux
                #    Requires BC_HW_T_S(#,#)
                #                 ^^^^ presumably BC_HW_SCALAR
                #    Sets keyword BC_C_SCALAR (#,#) to 0.0
                #    Requires BC_SCALARW (#,#)
                elif (hw is not None) and (c==0.0) and (sw is not None):
                    eq_type = CONVECTIVE_FLUX

                if eq_type is None:
                    eq_type = NO_FLUX # default
                    self.set_bcs_scalar_eq_type(eq_type, i, in_setup=True)
                    # Message?

            cb.setCurrentIndex(eq_type)

            #    Define wall temperature
            # Specification only available with 'Specified Temperature' BC type
            # Sets keyword BC_SCALARW (#,#)
            # DEFAULT 0.0
            enabled = (eq_type==SPECIFIED_TEMPERATURE)
            key = 'bc_scalarw'
            default = 0.0 if enabled else None
            setup_key_widget(key, i,  default, enabled)
            # Hack to prevent dup. display
            if enabled:
                le = getattr(ui, 'lineedit_keyword_bc_scalarw_2_args_BC_%s' %i)
                le.setText('')
            else:
                le = getattr(ui, 'lineedit_keyword_bc_scalarw_args_BC_%s' %i)
                le.setText('')

            #    Define constant flux
            # Specification only available with 'Specified Flux' BC type
            # Sets keyword BC_C_SCALAR (#,#)
            # DEFAULT 0.0
            enabled = (eq_type==SPECIFIED_FLUX)
            key = 'bc_c_scalar'
            default = 0.0
            setup_key_widget(key, i, default, enabled)

            #    Define transfer coefficient
            # Specification only available with 'Convective Flux' BC type
            # Sets keyword BC_HW_SCALAR(#,#)
            # DEFAULT 0.0
            enabled = (eq_type==CONVECTIVE_FLUX)
            key = 'bc_hw_scalar'
            default = 0.0
            setup_key_widget(key, i, default, enabled)

            #    Define free stream temperature
            # Specification only available with 'Convective Flux' BC type
            # Sets keyword BC_SCALARW (#,#)
            # DEFAULT 0.0
            enabled = (eq_type==CONVECTIVE_FLUX)
            key = 'bc_scalarw'
            default = 0.0 if enabled else None
            setup_key_widget(key, i, default, enabled, suffix='_2')
            # Hack to prevent dup. display
            if enabled:
                le = getattr(ui, 'lineedit_keyword_bc_scalarw_args_BC_%s' %i)
                le.setText('')
            else:
                le = getattr(ui, 'lineedit_keyword_bc_scalarw_2_args_BC_%s' %i)
                le.setText('')
        if spacer:
            page_layout.addItem(spacer)


    def bcs_handle_flow_input(self, widget, data, ignore_key):
        if not data:
            self.error('bcs_handle_flow_input: no data')
            return
        key, val = data.popitem()
        P = self.bcs_current_solid
        for BC in self.bcs_current_indices:
            self.update_keyword(widget.key, val, args=mkargs(key, bc=BC, phase=P))


    def handle_bc_dt_0(self, widget, data, ignore_key):
        ui = self.ui.boundary_conditions
        if not self.bcs_current_indices:
            return
        BC0 = self.bcs_current_indices[0]
        # BC_DT_0 specification should persist across the gas and solids tabs.
        # If the user sets it in the gas phase tab, but then changes it under a solids tab,
        # a warning message indicating that this value is 'constant' across all phases should be given.
        key = 'bc_dt_0'
        prev_val = self.project.get_value(key, args=[BC0])
        new_key, new_val = data.popitem()
        if new_val == prev_val:
            return

        resp = self.message(text="Setting bc_dt_0 applies to all fluid and solid phases\nAre you sure?",
                            buttons=['yes', 'no'],
                            default='no')

        if resp != 'yes':
            widget.updateValue(key, prev_val)
            return

        for BC in self.bcs_current_indices:
            self.update_keyword(key, new_val, args=[BC])


    def bcs_handle_flow_type(self, widget, data, ignore_key):
        # This handles inflow and outflow for fluid and solids phases
        #  The combobox has a (non-keyword) 'key' set to distinguish where
        #  this is being called from
        if not data:
            self.error('bcs_handle_flow_type: no data')
            return
        ui = self.ui.boundary_conditions
        if not self.bcs_current_indices:
            return
        index = widget.currentIndex()

        BC0 = self.bcs_current_indices[0]
        P = self.bcs_current_solid
        key, val = data.popitem()
        phase_type, flow_direction = widget.key.split('_')
        axis = get_combobox_item(widget, AXIAL).text()[0]
        if axis not in xmap: # CG_MI
            return
        if phase_type == 'fluid':
            subkeys = ['bc_%s_g' % xmap[axis], 'bc_volflow_g', 'bc_massflow_g']
        elif phase_type == 'solids':
            subkeys = ['bc_%s_s' % xmap[axis], 'bc_volflow_s', 'bc_massflow_s']
        else:
            raise ValueError(phase_type)

        subkey = subkeys[index]
        le = getattr(ui, 'lineedit_%s' % widget.key)
        prev_val = le.value

        if phase_type == 'solids':
            bc_key = 'solids_%s_type' % flow_direction
            for BC in self.bcs_current_indices:
                if bc_key not in self.bcs[BC]:
                    self.bcs[BC][bc_key] = {}
                self.bcs[BC][bc_key][P] = index
        else:
            bc_key = 'fluid_%s_type' % flow_direction
            for BC in self.bcs_current_indices:
                self.bcs[BC][bc_key] = index

        #    Available selections:
        # FLUID                                # SOLIDS
        #    Y-Axial Velocity (m/s) [DEFAULT]
        # Sets keyword BC_V_G(#)               # Sets keyword BC_V_S(#,#)
        # DEFAULT 0.0
        #    Volumetric Flow Rate (m3/s)
        # Sets keyword BC_VOLFLOW_G(#)         # Sets keyword BC_VOLFLOW_S(#,#)
        # DEFAULT 0.0
        #    Mass Flow Rate (kg/s)
        # Sets keyword BC_MASSFLOW_G(#)        # Sets keyword BC_MASSFLOW_S(#,#)
        # DEFAULT 0.0
        le.key = subkey
        le.args = ['BC', 'P'] if phase_type=='solids' else ['BC']
        le.dtype = float
        le.allow_parameters = True
        self.add_tooltip(le, subkey)

        for (i, k) in enumerate(subkeys):
            if i == index:
                continue
            for BC in self.bcs_current_indices:
                self.unset_keyword(k, args=mkargs(k, bc=BC, phase=P))

        val = self.project.get_value(subkeys[index], args=mkargs(subkeys[index], bc=BC0, phase=P))
        default = val if val is not None else prev_val if prev_val is not None else 0.0
        for BC in self.bcs_current_indices:
            self.update_keyword(subkey, default, args=mkargs(subkey, bc=BC, phase=P))
        le.updateValue(subkey, default)

        if widget.key == 'fluid_inflow':
            self.setup_bcs_fluid_inflow_tab()
        elif widget.key == 'fluid_outflow':
            self.setup_bcs_fluid_mo_tab()
        if widget.key == 'solids_inflow':
            self.setup_bcs_solids_inflow_tab(P)
        elif widget.key == 'solids_outflow':
            self.setup_bcs_solids_mo_tab(P)


    def update_bcs_fluid_mass_fraction_table(self):
        ui = self.ui.boundary_conditions
        table = ui.tablewidget_fluid_mass_fraction
        table.clearContents()
        table.setRowCount(0)
        if not (self.fluid_species and self.bcs_current_indices):
            self.fixup_bcs_table(table)
            ui.groupbox_fluid_composition.setEnabled(False)
            return
        ui.groupbox_fluid_composition.setEnabled(True)
        BC0 = self.bcs_current_indices[0]
        species = self.fluid_species
        if species:
            nrows = len(species) + 1 # 'Total' row at end
        else:
            nrows = 0
        table.setRowCount(nrows)
        def make_item(val):
            item = QTableWidgetItem('' if val is None else str(val))
            set_item_noedit(item)
            return item
        for (row, (species,data)) in enumerate(species.items()):
            alias = data.get('alias', species) # default to species if no alias
            table.setItem(row, 0, make_item(alias))
            # mass fraction
            le = LineEdit()
            le.setdtype('dp')
            le.setValInfo(min=0.0, max=1.0) # TODO adjust max dynamically
            key = 'bc_x_g'
            le.key = key
            le.args = [self.bcs_current_indices, row+1]
            le.dtype = float
            le.allow_parameters = True
            self.add_tooltip(le, key)
            val = self.project.get_value(key, args=[BC0, row+1], default=None)
            if val is not None:
                le.updateValue(key, val)
            le.value_updated.connect(self.handle_bcs_fluid_mass_fraction)
            table.setCellWidget(row, 1, le)
        if species:
            table.setItem(nrows-1, 0, make_item("Total"))
            table.setItem(nrows-1, 1, make_item(''))
            item = table.item(nrows-1, 0)
            font = item.font()
            font.setBold(True)
            item.setFont(font)
            self.update_bcs_fluid_mass_fraction_total()
        self.fixup_bcs_table(table)


    def handle_bcs_fluid_mass_fraction(self, widget, value_dict, args):
        key = 'bc_x_g'
        val = value_dict[key]
        widget.updateValue(key, val)
        if val == '':
            self.unset_keyword(key, args=args)
        else:
            self.update_keyword(key, val, args=args)

        # If any bc_x_g is set, all must be set
        # Slight hack to allow unsetting optional values for PO/CG_PO
        BC0 = args[0][0]
        bc_type = self.project.get_value('bc_type', args=[BC0])
        if bc_type.endswith('PO') and val == '':
            set_keys = False
        else:
            set_keys = True

        # DEFAULT - last defined species has mass fraction of 1.0
        # See also bcs_set_default_keys
        if set_keys: # TODO enforce sum to 1
            for BC in args[0]:
                N = len(self.fluid_species)
                total = sum(safe_float(self.project.get_value(key, default=0, args=[BC,i]))
                            for i in range(1, 1+N))
                for i in range(1, 1+N):
                    default = float(i==N) if total==0 else 0.0
                    self.set_keyword_default(key, default, args=[BC,i])
        self.update_bcs_fluid_mass_fraction_table()


    def update_bcs_fluid_mass_fraction_total(self):
        if not self.bcs_current_indices:
            return
        if not self.fluid_species:
            return
        BC0 = self.bcs_current_indices[0]
        ui = self.ui.boundary_conditions
        key = 'bc_x_g'
        table = ui.tablewidget_fluid_mass_fraction
        if table.rowCount() == 0:
            return
        nmax_g = self.project.get_value('nmax_g', default=0.0)
        total = sum(safe_float(self.project.get_value(key, default=0.0, args=[BC0,i]))
                    for i in range(1,nmax_g+1))
        total = round(total, 6)

        all_unset = all(self.project.get_value(key, args=[BC,i]) is None
                        for BC in self.bcs_current_indices
                        for i in range(1,1+nmax_g))

        # Total item
        item =  table.item(table.rowCount()-1, 1)
        # If no keys are set, don't show total
        if all_unset:
            item.setText('')
        else:
            item.setText(str(total))
            font = item.font()
            font.setBold(True)
            item.setFont(font)
            if total != 1.0:
                item.setForeground(Qt.red)
                #We should warn, but this creates too many popups while BC is being set up
                #self.warning("Mass fractions sum to %s, must be 1.0" % total, popup=True)
            elif ui.isEnabled():
                item.setForeground(Qt.black) # FIXME this looks wrong when UI greyed-out


    def setup_bcs_fluid_inflow_tab(self):
        #Subtask Pane Tab for INFLOW type (MI, PI, CG_MI) Boundary Condition Regions
        #Fluid (tab)
        ui = self.ui.boundary_conditions
        ui.page_fluid.setCurrentIndex(PAGE_INFLOW)

        def show_velocity_3(enabled):
            # Show third velocity input, for CG_MI inflows
            for w in ui.lineedit_keyword_bc_w_g_args_BC, ui.label_fluid_velocity_3, ui.label_fluid_velocity_3_units:
                w.setHidden(not enabled)
            if enabled:
                ui.label_fluid_velocities.setText('Velocity components (optional):')
                ui.label_fluid_velocity_1.setText('  X-axial velocity')
                le = ui.lineedit_fluid_velocity_1
                le.key = 'bc_u_g'
                le.args = ['BC']
                le.dtype = float
                le.allow_parameters = True
                self.add_tooltip(le, key=le.key)
                ui.label_fluid_velocity_2.setText('  Y-axial velocity')
                le = ui.lineedit_fluid_velocity_2
                le.key = 'bc_v_g'
                le.args = ['BC']
                le.dtype = float
                le.allow_parameters = True
                self.add_tooltip(le, key=le.key)
            else:
                ui.label_fluid_velocities.setText('Tangential velocities:')

        if not self.bcs_current_indices:
            show_velocity_3(False)
            return # Nothing selected.  (Clear out all lineedits?)
        BC0 = self.bcs_current_indices[0]
        bc_type = self.project.get_value('bc_type', args=[BC0])

        if bc_type is None:
            show_velocity_3(False)
            self.error("bc_type not set for region %s" % BC0)
            return

        def get_widget(key):
            for pat in ('lineedit_keyword_%s_args_BC',
                        'lineedit_%s_args_BC'):
                widget = getattr(ui, pat % key, None)
                if widget:
                    return widget
            self.error('no widget for key %s' % key)

        def setup_key_widget(key, default=None, enabled=True, suffix=''):
            for pat in ('label_%s', 'label_%s_units',
                         'lineedit_keyword_%s_args_BC'):
                name = pat%(key+suffix)
                item = getattr(ui, name, None)
                if item:
                    item.setEnabled(enabled)
            args = mkargs(key, bc=BC0)
            val = self.project.get_value(key, args=args)
            if val is None and enabled:
                val = default
            get_widget(key+suffix).updateValue(key, val, args=args)
            for BC in self.bcs_current_indices:
                self.update_keyword(key, val, args=mkargs(key, bc=BC))

        #    Define volume fraction
        # Specification always available
        # Sets keyword BC_EP_G(#)
        #  DEFAULT 1.0 for MI and CG_MI; leave [UNDEFINED] for PI
        #  Error Check: For MI and CG_MI, BC_EP_G(#) + BC_EP_S(#,:) = 1.0 #(see bcs_set_volume_fraction_limit)
        # TODO  Error Check: For PI - either all are defined and sum to 1, or all are undefined
        enabled = True
        key = 'bc_ep_g'
        default = 1.0 if bc_type in ('MI', 'CG_MI') else None
        setup_key_widget(key, default, enabled)
        get_widget(key).setReadOnly(True)
        #get_widget(key).setEnabled(False) # better way to indicate read-only?

        #    Define inflow properties: Mass inflow specification changes
        # based on the BC_TYPE and Region orientation (e.g., XZ-Plane)
        #
        region_name = self.bcs[BC0].get('region')
        if not region_name:  # should not happen!
            self.error("No region defined for BC %s" % BC0)
            return
        region_info = self.bcs_region_dict.get(region_name)
        if not region_info:
            self.error("No definition for region %s" % region_name)
            return
        region_type = region_info.get('type')
        if not region_type:
            self.error("No type for region %s" % region_name)
            return
        cartesian_grid = bool(self.project.get_value('cartesian_grid', default=False))

        show_velocity_3(bc_type == 'CG_MI')
        # For BC_TYPE='MI' and XZ-Plane region
        # For BC_TYPE='MI' and YZ-Plane region
        # For BC_TYPE='MI' and XY-Plane region
        if bc_type in ('MI', 'CG_MI'):
            #  Select mass inflow specification type:
            ui.stackedwidget_fluid_inflow.setCurrentIndex(0) # subpage_fluid_inflow_MI
            ui.label_fluid_velocities.show()

            cb = ui.combobox_fluid_inflow_type
            if bc_type == 'MI':
                if not region_type.endswith('plane'):
                    self.error("Invalid type %s for region %s" % (region_type, region_name))
                    return
                tangents = [C for C in 'XYZ' if C in region_type]
                normal = [C for C in 'XYZ' if C not in tangents]
                if len(normal) != 1 or len(tangents) != 2:
                    self.error("Invalid type %s for region %s" % (region_type, region_name))
                    return
                normal = normal[0]
                item = get_combobox_item(cb, AXIAL)
                item.setText('%s-axial velocity' % normal)
                axial_enabled = True
                item.setEnabled(True)
                item = get_combobox_item(cb, VOLFLOW)
                inflow_type = self.bcs[BC0].get('fluid_inflow_type')
                item.setEnabled(True)
            elif bc_type == 'CG_MI':
                # CG_MI forces massflow
                axial_enabled = False
                cb.setCurrentIndex(MASSFLOW)
                # Hack. The normals and tangents don't mean anything for CG_MI
                #  but are used in the (greyed-out) labels
                normal, tangents = 'X', ['Y', 'Z']
                inflow_type = MASSFLOW
            for i in (AXIAL, VOLFLOW):
                item = get_combobox_item(cb, i)
                item.setEnabled(axial_enabled)
            if not axial_enabled:
                get_combobox_item(cb, AXIAL).setText("Axial velocity")
            # Don't stay on disabled item

            if inflow_type == AXIAL and not axial_enabled: # TODO also trap volflow
                self.error("Axial velocity not available for STL boundaries")
                for BC in self.bcs_current_indices:
                    for c in 'uvw':
                        self.unset_keyword('bc_%s_g'%c, args=[BC])
                    try:
                        del self.bcs[BC]['fluid_inflow_type']
                    except KeyError:
                        pass
                    inflow_type = None

            if bc_type == 'CG_MI':
                inflow_type = MASSFLOW
                if self.project.get_value('bc_volflow_g', args=[BC0]) is not None:
                    self.error("bc_volflow_g not allowed for STL boundaries")
                    for BC in self.bcs_current_indices:
                        self.unset_keyword('bc_volflow_g', args=[BC])

            keys = ['bc_%s_g'%xmap[normal], 'bc_volflow_g', 'bc_massflow_g']
            vals = [self.project.get_value(k, args=[BC0]) for k in keys]
            if inflow_type is None:
                key_is_set = [v is not None for v in vals]
                count = sum(key_is_set)
                if count == 0:
                    inflow_type = AXIAL if axial_enabled else MASSFLOW
                else:
                    inflow_type = key_is_set.index(True)
                if count > 1:
                    self.error("Only one of %s may be set" % (' '.join(keys)))
                    first = True
                    for (k,v) in zip(keys, vals):
                        if v is not None:
                            if first:
                                first = False # Keep the first set key
                            else: # Unset the rest
                                for BC in self.bcs_current_indices:
                                    self.unset_keyword(k, args=[BC])

                for BC in self.bcs_current_indices:
                    self.bcs[BC]['fluid_inflow_type'] = inflow_type

            cb.setCurrentIndex(inflow_type)
            key = keys[inflow_type]
            val = vals[inflow_type]
            le = ui.lineedit_fluid_inflow
            le.key = key
            le.args = ['BC']
            le.dtype = float
            le.allow_parameters = True
            self.add_tooltip(le, key)
            le.updateValue(key , 0.0 if val is None else val)
            units = ['m/s', 'm³/s', 'kg/s', 'm/s'][inflow_type]
            ui.label_fluid_inflow_units.setText(units)

            if bc_type != 'CG_MI': # show_tangential
                ui.label_fluid_velocities.setText('Tangential velocities:')
                #  Define Tangential Velocities:
                # Tangential velocity 1
                # Set keyword BC_[UVW]_G(#)
                # DEFAULT 0.0
                key = 'bc_%s_g'%xmap[tangents[0]]
                default = 0.0
                label = ui.label_fluid_velocity_1
                label.setText('  %s-axial velocity' % tangents[0])
                self.add_tooltip(label, key)
                val = self.project.get_value(key, args=[BC0])
                if val is None:
                    val = default
                    for BC in self.bcs_current_indices:
                        self.update_keyword(key, val, args=[BC])
                widget = ui.lineedit_fluid_velocity_1
                widget.key = key
                widget.args = ['BC']
                widget.dtype = float
                widget.allow_parameters = True
                widget.updateValue(key, val)
                self.add_tooltip(widget, key)

                # Tangential velocity 2
                # Sets keyword BC_[UVW]_G(#)
                # DEFAULT 0.0
                key = 'bc_%s_g'%xmap[tangents[1]]
                default = 0.0
                label = ui.label_fluid_velocity_2
                label.setText('  %s-axial velocity' % tangents[1]) # Indent in GUI
                self.add_tooltip(label, key)
                val = self.project.get_value(key, args=[BC0])
                if val is None:
                    val = default
                    for BC in self.bcs_current_indices:
                        self.update_keyword(key, val, args=[BC])
                widget = ui.lineedit_fluid_velocity_2
                widget.key = key
                widget.args = ['BC']
                widget.dtype = float
                widget.allow_parameters = True
                widget.updateValue(key, val)
                self.add_tooltip(widget, key)

            else: # CG_MI
                for widget in (ui.lineedit_fluid_velocity_1,
                               ui.lineedit_fluid_velocity_2,
                               ui.lineedit_keyword_bc_w_g_args_BC):
                    val = self.project.get_value(widget.key, args=[BC0])
                    widget.updateValue(widget.key, val)

        elif bc_type == 'PI':
            ui.stackedwidget_fluid_inflow.setCurrentIndex(1) # subpage_fluid_inflow_PI
            #  Specify all velocity components (optional):
            ui.label_fluid_velocities.hide() # TODO show "optional" in label?
            #    Define X-Axial Velocity
            # Sets keyword BC_U_G(#)
            # DEFAULT 0.0
            key = 'bc_u_g'
            default = 0.0
            widget = ui.lineedit_fluid_inflow
            widget.key = key
            widget.args = ['BC']
            widget.dtype = float
            widget.allow_parameters = True
            self.add_tooltip(widget, key)
            val = self.project.get_value(key, args=[BC0])
            if val is None:
                val = default
                for BC in self.bcs_current_indices:
                    self.update_keyword(key, val, args=[BC])
            widget.updateValue(key, val)
            self.add_tooltip(widget, key)

            #    Define Y-Axial Velocity
            # Sets keyword BC_V_G(#)
            # DEFAULT 0.0
            key = 'bc_v_g'
            default = 0.0
            label =  ui.label_fluid_velocity_1
            label.setText('Y-axial velocity')
            self.add_tooltip(label, key)
            widget = ui.lineedit_fluid_velocity_1
            widget.key = key
            widget.args = ['BC']
            widget.dtype = float
            widget.allow_parameters = True
            val = self.project.get_value(key, args=[BC0])
            if val is None:
                val = default
                for BC in self.bcs_current_indices:
                    self.update_keyword(key, val, args=[BC])
            widget.updateValue(key, val)
            self.add_tooltip(widget, key)

            #    Define Z-Axial Velocity
            # Sets keyword  BC_W_G
            # DEFAULT 0.0
            key = 'bc_w_g'
            default = 0.0
            label =  ui.label_fluid_velocity_2
            label.setText('Z-axial velocity')
            self.add_tooltip(label, key)
            widget = ui.lineedit_fluid_velocity_2
            widget.key = key
            widget.args = ['BC']
            widget.dtype = float
            widget.allow_parameters = True
            self.add_tooltip(widget, key)
            val = self.project.get_value(key, args=[BC0])
            if val is None:
                val = default
                for BC in self.bcs_current_indices:
                    self.update_keyword(key, val, args=[BC])
            widget.updateValue(key, val)
            self.add_tooltip(widget, key)

        #Define temperature
        # Specification always available
        # Input required for any of the following
        #  Fluid density model: Ideal Gas Law
        #  Fluid viscosity model: Sutherland's Law
        #  Energy equations are solved
        # Sets keyword BC_T_G(#)
        # DEFAULT 293.15
        energy_eq = self.project.get_value('energy_eq', default=True)
        enabled = (self.fluid_density_model==OTHER
                   or self.fluid_viscosity_model==OTHER
                   or bool(energy_eq))
        key = 'bc_t_g'
        default = 293.15
        setup_key_widget(key, default, enabled)

        #Define pressure
        # Specification always available
        # Input required when combining ideal gas law and specified mass inflow rate
        # Input required for BC_TYPE = PI
        # TODO "required"
        # Sets keyword BC_P_G(#)
        # DEFAULT 101.325d3
        enabled = True
        key = 'bc_p_g'
        default = FloatExp('101.325e3')
        setup_key_widget(key, default, enabled)

        #Select species and set mass fractions (table format)
        # Specification always available
        # TODO Input required for species equations
        # Drop down menu of fluid species
        # Sets keyword BC_X_G(#,#)
        # DEFAULT - last defined species has mass fraction of 1.0
        # TODO Error check: mass fractions must sum to 1.0
        #
        # Move back the table, in case it got stolen
        comp = ui.groupbox_fluid_composition
        parent = comp.parent()
        if parent != ui.page_fluid_inflow:
            comp.hide()
            parent.layout().removeWidget(comp)
            layout = ui.page_fluid_inflow.layout()
            layout.insertWidget(layout.count()-2, comp) # Above 'turbulence' - should it be last for consistency?
            comp.show()
        self.update_bcs_fluid_mass_fraction_table()

        #Turbulence: Define k-ε turbulent kinetic energy
        turbulence_model = self.project.get_value('turbulence_model', default=DEFAULT_TURBULENCE_MODEL)
        enabled = (turbulence_model == 'K_EPSILON')
        ui.groupbox_inflow_turbulence.setEnabled(enabled)

        # Specification only available with K-Epsilon turbulence model
        # Sets keyword BC_K_TURB_G(#)
        # DEFAULT 0.0
        default = 0.0 if enabled else None
        key = 'bc_k_turb_g'
        setup_key_widget(key, default, enabled)

        #Turbulence: Define k-ε turbulent dissipation
        # Specification only available with K-Epsilon turbulence model
        # Sets keywords BC_E_TURB_G(#)
        # DEFAULT 0.0
        key = 'bc_e_turb_g'
        default = 0.0 if enabled else None
        setup_key_widget(key, default, enabled)


    def update_bcs_solids_mass_fraction_table(self):
        ui = self.ui.boundary_conditions
        table = ui.tablewidget_solids_mass_fraction
        table.clearContents()
        table.setRowCount(0)
        P = self.bcs_current_solid
        if not (P and self.solids_species.get(P) and self.bcs_current_indices):
            self.fixup_bcs_table(table)
            table.setEnabled(False)
            ui.groupbox_solids_composition.setEnabled(False)
            return
        ui.groupbox_solids_composition.setEnabled(True)
        table.setEnabled(True)
        BC0 = self.bcs_current_indices[0]
        species = self.solids_species[P]
        if species:
            nrows = len(species) + 1 # 'Total' row at end
        else:
            nrows = 0
        table.setRowCount(nrows)
        def make_item(val):
            item = QTableWidgetItem('' if val is None else str(val))
            set_item_noedit(item)
            return item
        for (row, (species,data)) in enumerate(species.items()):
            alias = data.get('alias', species) # default to species if no alias
            table.setItem(row, 0, make_item(alias))
            # mass fraction
            key = 'bc_x_s'
            le = LineEdit()
            le.key = key
            le.args = [self.bcs_current_indices, P, row+1]
            le.dtype = float
            le.allow_parameters = True
            le.setValInfo(min=0.0, max=1.0)
            self.add_tooltip(le, key)
            val = self.project.get_value(key, args=[BC0, P, row+1], default=None)
            if val is not None:
                le.updateValue(key, val)
            le.value_updated.connect(self.handle_bcs_solids_mass_fraction)
            table.setCellWidget(row, 1, le)
        if species:
            table.setItem(nrows-1, 0, make_item("Total"))
            table.setItem(nrows-1, 1, make_item(''))
            item = table.item(nrows-1, 0)
            font = item.font()
            font.setBold(True)
            item.setFont(font)
            self.update_bcs_solids_mass_fraction_total()
        self.fixup_bcs_table(table)


    def handle_bcs_solids_mass_fraction(self, widget, value_dict, args):
        key = 'bc_x_s'
        val = value_dict[key]
        widget.updateValue(key, val)
        if val == '':
            self.unset_keyword(key, args=args)
        else:
            self.update_keyword(key, val, args=args)
        # If any bc_x_s is set, all must be set
        # Slight hack to allow unsetting optional values for PO/CG_PO
        BC0 = args[0][0]
        bc_type = self.project.get_value('bc_type', args=[BC0])
        if bc_type.endswith('PO') and val == '':
            set_keys = False
        else:
            set_keys = True

        # DEFAULT - last defined species has mass fraction of 1.0
        # See also bcs_set_default_keys
        if set_keys: # TODO enforce sum to 1
            P = args[1]
            N = len(self.solids_species[P])
            for BC in args[0]:
                total = sum(safe_float(self.project.get_value(key, default=0, args=[BC,P,i]))
                            for i in range(1, 1+N))
                for i in range(1, 1+N):
                    default = float(i==N) if total==0 else 0.0
                    self.set_keyword_default(key, default, args=[BC,P,i])
        self.update_bcs_solids_mass_fraction_table()


    def update_bcs_solids_mass_fraction_total(self):
        if not self.bcs_current_indices:
            return
        BC0 = self.bcs_current_indices[0]
        P = self.bcs_current_solid
        if P is None:
            return
        species = self.solids_species.get(P)
        if not P:
            return
        ui = self.ui.boundary_conditions
        key = 'bc_x_s'
        table = ui.tablewidget_solids_mass_fraction
        if table.rowCount() == 0:
            return
        total = sum(safe_float(self.project.get_value(key, default=0.0, args=[BC0,P,i]))
                    for i in range(1,len(species)+1))
        total = round(total, 6)
        N = len(species)
        all_unset = all(self.project.get_value(key, args=[BC,P,i]) is None
                        for BC in self.bcs_current_indices
                        for i in range(1,1+N))

        item = table.item(table.rowCount()-1, 1)
        # If no keys are set, don't show total
        if all_unset:
            item.setText('')
        else:
            font = item.font()
            font.setBold(True)
            item.setFont(font)
            item.setText(str(total))
            if total != 1.0:
                item.setForeground(Qt.red)
                #We should warn, but this creates too many popups while BC is being set up
                #self.warning("Mass fractions sum to %s, must be 1.0" % total, popup=True)
            elif ui.isEnabled():
                item.setForeground(Qt.black) # FIXME looks wrong when greyed-out


    def setup_bcs_solids_inflow_tab(self, P):
        #Subtask Pane Tab for INFLOW type (MI, PI, CG_MI) Boundary Condition Regions
        #Solid-# (tab) - Rename tab to user provided solids name.
        ui = self.ui.boundary_conditions
        ui.page_solids.setCurrentIndex(PAGE_INFLOW)
        self.bcs_current_solid = self.P = P

        def show_velocity_3(enabled):
            # Show third velocity input, for CG_MI inflows
            for w in ui.lineedit_keyword_bc_w_s_args_BC_P, ui.label_solids_velocity_3, ui.label_solids_velocity_3_units:
                w.setHidden(not enabled)
            if enabled:
                ui.label_solids_velocities.setText('Velocity components (optional):')
                ui.label_solids_velocity_1.setText('  X-axial velocity')
                le = ui.lineedit_solids_velocity_1
                le.key = 'bc_u_s'
                le.args = ['BC', 'P']
                le.dtype = float
                le.allow_parameters = True
                self.add_tooltip(le, key=le.key)
                ui.label_solids_velocity_2.setText('  Y-axial velocity')
                le = ui.lineedit_solids_velocity_2
                le.key = 'bc_v_s'
                le.args = ['BC', 'P']
                le.dtype = float
                le.allow_parameters = True
                self.add_tooltip(le, key=le.key)

        if P is None: # Nothing to do
            show_velocity_3(False)
            return
        if not self.bcs_current_indices:
            show_velocity_3(False)
            return # Nothing selected.  (Clear out all lineedits?)

        BC0 = self.bcs_current_indices[0]
        bc_type = self.project.get_value('bc_type', args=[BC0])
        if bc_type is None:
            show_velocity_3(False)
            self.error("bc_type not set for region %s" % BC0)
            return

        show_velocity_3(bc_type =='CG_MI')

        self.bcs_set_volume_fraction_limit()

        def get_widget(key):
            for pat in ('lineedit_keyword_%s_args_BC_P',
                        'lineedit_keyword_%s_args_BC',
                        'lineedit_%s_args_BC_P',
                        'lineedit_%s_args_BC',
                        'lineedit_keyword_%s',
                        'lineedit_%s'):
                widget = getattr(ui, pat % key, None)
                if widget:
                    return widget
            self.error('no widget for key %s' % key)

        def setup_key_widget(key, default=None, enabled=True, suffix=''):
            for pat in ('label_%s', 'label_%s_units',
                        'lineedit_keyword_%s_args_BC_P',
                        'lineedit_keyword_%s_args_BC',
                        'lineedit_%s_args_BC_P',
                        'lineedit_%s_args_BC',
                        'lineedit_keyword_%s',
                        'lineedit_%s'):
                name = pat % (key+suffix)
                item = getattr(ui, name, None)
                if item:
                    item.setEnabled(enabled)

            args = mkargs(key, bc=BC0, phase=P)
            val = self.project.get_value(key, args=args)
            if val is None and enabled:
                val = default
            get_widget(key+suffix).updateValue(key, val, args=args)
            for BC in self.bcs_current_indices:
                self.update_keyword(key, val, args=mkargs(key, bc=BC, phase=P))

        #Define volume fraction
        # Specification always available
        # Sets keyword BC_EP_S(#,#)
        #  DEFAULT 1.0 - (sum of previous tabs) for MI and CG_MI; leave [UNDEFINED] for PI
        # TODO Error Check: For MI and CG_MI, BC_EP_G(#) + BC_EP_S(#,:) = 1.0
        # TODO Error Check: For PI - either all are defined and sum to 1, or all are undefined
        enabled = True
        key = 'bc_ep_s'

        #s = sum(self.project.get_value(key, default=0, args=[BC0, p]) for p in range(1, P)) # Sum of previous tabs
        # (this is not right, we get multiple solids phases with bc_ep_s == 1 - cgw
        #default = (1.0 - s) if bc_type in ('MI', 'CG_MI') else None
        # issues/429 suggests that bc_ep_s should default to 0, note sure what this code is trying to do - jmw
        # TODO review this
        default = 0.0
        setup_key_widget(key, default, enabled)

        #Define inflow properties: Mass inflow specification changes
        #based on the BC_TYPE and Region orientation (e.g., XZ-Plane)
        #
        region_name = self.bcs[BC0].get('region')
        if not region_name:  # should not happen!
            self.error("No region defined for BC %s" % BC0)
            return
        region_info = self.bcs_region_dict.get(region_name)
        if not region_info:
            self.error("No definition for region %s" % region_name)
            return
        region_type = region_info.get('type')
        if not region_type:
            self.error("No type for region %s" % region_name)
            return
        cartesian_grid = bool(self.project.get_value('cartesian_grid', default=False))

        # For BC_TYPE='MI' and XZ-Plane region
        # For BC_TYPE='MI' and YZ-Plane region
        # For BC_TYPE='MI' and XY-Plane region
        #  Select mass inflow specification type:
        if bc_type in ('MI', 'CG_MI'):
            ui.stackedwidget_solids_inflow.setCurrentIndex(0) # subpage_solids_inflow_MI
            ui.label_solids_velocities.show()
            cb = ui.combobox_solids_inflow_type
            if bc_type == 'MI':
                if not region_type.endswith('plane'):
                    self.error("Invalid type %s for region %s" % (region_type, region_name))
                    return
                tangents = [C for C in 'XYZ' if C in region_type]
                normal = [C for C in 'XYZ' if C not in tangents]
                if len(normal) != 1 or len(tangents) != 2:
                    self.error("Invalid type %s for region %s" % (region_type, region_name))
                    return
                normal = normal[0]

                item = get_combobox_item(cb, AXIAL)
                item.setText('%s-axial velocity' % normal)
                axial_enabled = True
                inflow_type = self.bcs[BC0].get('solids_inflow_type',{}).get(P)

            elif bc_type == 'CG_MI':
                # CG_MI does not have axial velocity
                item = get_combobox_item(cb, AXIAL)
                axial_enabled = False
                inflow_type = MASSFLOW
                # Hack. The normals and tangents don't mean anything for CG_MI
                #  but are used in the (greyed-out) labels
                normal, tangents = 'X', ['Y', 'Z']

            for i in (AXIAL, VOLFLOW):
                item = get_combobox_item(cb, i)
                item.setEnabled(axial_enabled)

            # Don't stay on disabled item
            if inflow_type == AXIAL and not axial_enabled: # TODO also trap volflow
                self.error("Axial velocity not available for STL boundaries")
                for BC in self.bcs_current_indices:
                    for c in 'uvw':
                        self.unset_keyword('bc_%s_s'%c, args=[BC, P])
                    try:
                        del self.bcs[BC]['solids_inflow_type'][P]
                    except KeyError:
                        pass
                    inflow_type = None

            if bc_type == 'CG_MI' and self.project.get_value('bc_volflow_s', args=[BC0,P]) is not None:
                self.error("bc_volflow_s not allowed for STL boundaries")
                for BC in self.bcs_current_indices:
                    self.unset_keyword('bc_volflow_s', args=[BC,P])

            keys = ['bc_%s_s'%xmap[normal], 'bc_volflow_s', 'bc_massflow_s']
            vals = [self.project.get_value(k, args=[BC0, P]) for k in keys]
            if inflow_type is None:
                key_is_set = [v is not None for v in vals]
                count = sum(key_is_set)
                if count == 0:
                    inflow_type = AXIAL if axial_enabled else MASSFLOW
                else:
                    inflow_type = key_is_set.index(True)

                if count > 1:
                    self.error("Only one of %s may be set" % (' '.join(keys)))
                    first = True
                    for (k,v) in zip(keys, vals):
                        if v is not None:
                            if first:
                                first = False # Keep the first set key
                            else: # Unset the rest
                                for BC in self.bcs_current_indices:
                                    self.unset_keyword(k, args=[BC, P])

                for BC in self.bcs_current_indices:
                    if 'solids_inflow_type'  not in self.bcs[BC]:
                        self.bcs[BC]['solids_inflow_type'] = {}
                    self.bcs[BC]['solids_inflow_type'][P] = inflow_type

            cb.setCurrentIndex(inflow_type)
            key = keys[inflow_type]
            val = vals[inflow_type]
            le = ui.lineedit_solids_inflow
            le.key = key
            le.args = ['BC', 'P']
            le.dtype = float
            le.allow_parameters = True
            self.add_tooltip(le, key)
            le.updateValue(key, 0.0 if val is None else val)
            units = ['m/s', 'm³/s', 'kg/s', 'm/s'][inflow_type]
            ui.label_solids_inflow_units.setText(units)

            if bc_type != 'CG_MI': # show_tangential:
                #  Define Tangential Velocities:
                # Tangential velocity 1
                # Sets keyword BC_[UVW]_S(#, #)
                # DEFAULT 0.0
                ui.label_solids_velocities.setText('Tangential velocities:')
                key = 'bc_%s_s'%xmap[tangents[0]]
                default = 0.0
                label = ui.label_solids_velocity_1
                label.setText('  %s-axial velocity' % tangents[0]) # indent in GUI
                self.add_tooltip(label, key)
                val = self.project.get_value(key, args=[BC0, P])
                if val is None:
                    val = default
                    for BC in self.bcs_current_indices:
                        self.update_keyword(key, val, args=[BC, P])
                widget = ui.lineedit_solids_velocity_1
                widget.key = key
                widget.args = ['BC', 'P']
                widget.dtype = float
                widget.allow_parameters = True
                widget.updateValue(key, val)
                self.add_tooltip(widget, key)

                # Tangential velocity 2
                # Sets keyword BC_[UVW]_S(#, #)
                # DEFAULT 0.0
                key = 'bc_%s_s'%xmap[tangents[1]]
                default = 0.0
                label = ui.label_solids_velocity_2
                label.setText('  %s-axial velocity' % tangents[1])# UIndetn in GUI
                self.add_tooltip(label, key)
                val = self.project.get_value(key, args=[BC0])
                if val is None:
                    val = default
                for BC in self.bcs_current_indices:
                    self.update_keyword(key, val, args=[BC, P])
                widget = ui.lineedit_solids_velocity_2
                widget.key = key
                widget.args = ['BC', 'P']
                widget.dtype = float
                widget.allow_parameters = True
                widget.updateValue(key, val)
                self.add_tooltip(widget, key)

            else: # CG_MI
                for widget in (ui.lineedit_solids_velocity_1,
                               ui.lineedit_solids_velocity_2,
                               ui.lineedit_keyword_bc_w_s_args_BC_P):
                    val = self.project.get_value(widget.key, args=[BC0, P])
                    widget.updateValue(widget.key, val)


        # For BC_TYPE='PI'
        elif bc_type ==  'PI':
            ui.stackedwidget_solids_inflow.setCurrentIndex(1) # subpage_solids_inflow_PI
            ui.label_solids_velocities.hide()
            #  Specify all velocity components:

            #    Define X-Axial Velocity
            # Sets keyword BC_U_S(#,#)
            # DEFAULT 0.0
            key = 'bc_u_s'
            default = 0.0
            widget = ui.lineedit_solids_inflow
            widget.key = key
            widget.args = ['BC', 'P']
            widget.dtype = float
            widget.allow_parameters = True
            val = self.project.get_value(key, args=[BC0,P])
            if val is None:
                val = default
                for BC in self.bcs_current_indices:
                    self.update_keyword(key, val, args=[BC, P])
            widget.updateValue(key, val)
            self.add_tooltip(widget, key)

            #    Define Y-Axial Velocity
            # Sets keyword BC_V_S(#,#)
            # DEFAULT 0.0
            key = 'bc_v_s'
            label =  ui.label_solids_velocity_1
            label.setText('Y-axial velocity')
            self.add_tooltip(label, key)
            default = 0.0
            widget = ui.lineedit_solids_velocity_1
            val = self.project.get_value(key, args=[BC0, P])
            if val is None:
                val = default
                for BC in self.bcs_current_indices:
                    self.update_keyword(key, val, args=[BC, P])
            widget.key = key
            widget.args = ['BC', 'P']
            widget.dtype = float
            widget.allow_parameters = True
            self.add_tooltip(widget, key)

            #    Define Z-Axial Velocity
            # Sets keyword BC_W_S(#,#)
            # DEFAULT 0.0
            key = 'bc_w_s'
            label =  ui.label_solids_velocity_2
            label.setText('Z-axial velocity')
            self.add_tooltip(label, key)
            default = 0.0
            widget = ui.lineedit_solids_velocity_2
            val = self.project.get_value(key, args=[BC0, P])
            if val is None:
                val = default
                for BC in self.bcs_current_indices:
                    self.update_keyword(key, val, args=[BC, P])
            widget.key = key
            widget.args = ['BC', 'P']
            widget.dtype = float
            widget.allow_parameters = True
            self.add_tooltip(widget, key)

        #Define temperature
        # Specification always available
        # TODO Input required when energy equations are solved
        # Sets keyword BC_T_S(#,#)
        # DEFAULT 293.15
        key = 'bc_t_s'
        enabled = True
        default = 293.15
        setup_key_widget(key, default, enabled)

        # SRS revision June 2017
        #Define solids phase granular temperature at the BC plane.
        #Sets keyword BC_THETA_M to user specified value
        #DEFAULT value ZERO
        #Available only for KT_TYPE /= “ALGEBRAIC"
        key = 'bc_theta_m'
        default = 0.0
        kt_type = self.project.get_value('kt_type', default=DEFAULT_KT_TYPE)
        enabled = (kt_type != 'ALGEBRAIC')
        setup_key_widget(key, default, enabled)

        #Select species and set mass fractions (table format)
        # Specification always available
        # Input required for species equations
        # Drop down menu of solids species
        # Sets keyword BC_X_S(#,#,#)
        # DEFAULT - last defined species has mass fraction of 1.0
        # TODO Error check: mass fractions must sum to 1.0
        # Move back the table, in case it got stolen
        comp = ui.groupbox_solids_composition
        parent = comp.parent()
        if parent != ui.page_solids_inflow:
            comp.hide()
            parent.layout().removeWidget(comp)
            layout = ui.page_solids_inflow.layout()
            layout.insertWidget(layout.count()-1, comp)
            comp.show()

        self.update_bcs_solids_mass_fraction_table()


    def setup_bcs_scalar_inflow_tab(self):
        #Subtask Pane Tab for INFLOW type (MI, PI, CG_MI) Boundary Condition Regions
        #Scalar (tab) - Tab only available if scalar equations are solved
        #    Define initial scalar value
        # Sets keyword BC_SCALAR(#,#)
        # DEFAULT 0.0
        # (almost same as setup_bcs_scalar_po_tab)
        ui = self.ui.boundary_conditions
        nscalar = self.project.get_value('nscalar', default=0)
        old_nscalar = getattr(ui, 'nscalar_inflow', None)
        old_scalar_bc_type = getattr(ui, 'scalar_bc_type', None)
        ui.nscalar_inflow = nscalar
        ui.scalar_bc_type = 'INFLOW'

        page = ui.page_scalar
        page_layout = page.layout()
        spacer = None
        key = 'bc_scalar'
        if nscalar != old_nscalar or old_scalar_bc_type != 'INFLOW':
            spacer = self.clear_bcs_scalar_tab()
            for i in range(1, nscalar+1):
                label = QLabel('Scalar %s' % i)
                page_layout.addWidget(label, i, 0)
                le = LineEdit()
                le.key = key
                le.dtype = float
                le.allow_parameters = True
                le.args = ['BC', i]
                self.add_tooltip(le, key)
                self.project.register_widget(le, [key], ['BC', i])
                setattr(ui, 'lineedit_keyword_%s_args_BC_%s' % (key, i), le)
                page_layout.addWidget(le, i, 1)

            if spacer:
                page_layout.addItem(spacer)

        if not self.bcs_current_indices:
            return
        BC0 = self.bcs_current_indices[0]
        for i in range(1, nscalar+1):
            le = getattr(ui, 'lineedit_keyword_%s_args_BC_%s' % (key, i), None)
            if not le:
                self.error("No widget for %s %s" % (key, i))
                continue
            args = [BC0, i]
            val = self.project.get_value(key, args=args)
            if val is None:
                val = 0.0 # Default
                for BC in self.bcs_current_indices:
                    self.update_keyword(key, val, args=[BC,i])
            le.updateValue(key, val, args=args)



    def setup_bcs_fluid_po_tab(self):
        #Subtask Pane Tab for PRESSURE OUTFLOW type (PO) Boundary Condition Regions
        #Fluid (tab)
        ui = self.ui.boundary_conditions
        ui.page_fluid.setCurrentIndex(PAGE_PO)

        if not self.bcs_current_indices:
            return
        BC0 = self.bcs_current_indices[0]

        def get_widget(key):
            for pat in ('lineedit_keyword_%s_args_BC',
                        'lineedit_%s_args_BC',
                        'lineedit_keyword_%s',
                        'lineedit_%s'):
                widget = getattr(ui, pat % key, None)
                if widget:
                    return widget
            self.error('no widget for key %s' % key)

        def setup_key_widget(key, default=None, enabled=True, suffix=''):
            for pat in ('label_%s', 'label_%s_units',
                        'lineedit_keyword_%s_args_BC',
                        'lineedit_%s_args_BC',
                        'lineedit_keyword_%s',
                        'lineedit_%s'):
                name = pat % (key+suffix)
                item = getattr(ui, name, None)
                if item:
                    item.setEnabled(enabled)
            args = mkargs(key, bc=BC0)
            val = self.project.get_value(key, args=args)
            if val is None and enabled:
                val = default
            get_widget(key+suffix).updateValue(key, val, args=args)
            for BC in self.bcs_current_indices:
                self.update_keyword(key, val, args=mkargs(key, bc=BC))

        #    Define pressure
        # Specification always available
        # TODO Input required
        # Sets keyword BC_P_G(#)
        # DEFAULT 101.325d3
        enabled = True
        key = 'bc_p_g'
        default = FloatExp('101.325e3')
        setup_key_widget(key, default, enabled, suffix='_2')

        #The remaining inputs are "optional." They do not have default values, because MFIX will calculate
        #appropriate values if they are unspecified and 'backflow' occurs at the outlet.
        #
        #    Define volume fraction
        # Specification always available
        # Sets keyword BC_EP_G(#)
        # No DEFAULT value
        # Error Check: If any volume fraction for the BC region is specified, then all volume fractions
        # for the BC region must be specified and must sum to 1.0.
        # TODO implement the above error check.  Note that not all fractions will be set during the
        #  setup phase.
        enabled = True
        key = 'bc_ep_g'
        default = None
        setup_key_widget(key, default, enabled, suffix='_2')
        get_widget(key+'_2').setReadOnly(True)
        #get_widget(key+'_2').setEnabled(False) # better way to indicate read-only?

        #    Define temperature
        # Specification always available
        # NO DEFAULT value
        # Sets keyword BC_T_G(#)
        enabled = True
        default = None
        key = 'bc_t_g'
        setup_key_widget(key, default, enabled, suffix='_2')

        #    Select species and set mass fractions (table format)
        # Specification always available
        # NO DEFAULT value
        # Sets keyword BC_X_G(#,#)
        # Error check: if specified, mass fractions must sum to 1.0

        # We already have a mass fraction table - just steal it and move it here
        comp = ui.groupbox_fluid_composition
        parent = comp.parent()
        if parent != ui.page_fluid_po:
            comp.hide()
            parent.layout().removeWidget(comp)
            # (Should we put it inside the 'optional' box?  or underneath?)
            #ui.groupbox_fluid_po_optional.layout().addWidget(comp, 2, 0, 1, 3)
            layout = ui.page_fluid_po.layout() #Underneath - nested groupbox looks weird
            layout.insertWidget(layout.count()-1, comp)
            comp.show()
        self.update_bcs_fluid_mass_fraction_table()


    def setup_bcs_solids_po_tab(self, P):
        #Subtask Pane Tab for PRESSURE OUTFLOW type (PO) Boundary Condition Regions
        #    Solids-# (tab)
        ui = self.ui.boundary_conditions
        ui.page_solids.setCurrentIndex(PAGE_PO)

        self.bcs_current_solid = self.P = P
        if P is None:
            return
        if not self.bcs_current_indices:
            return # Nothing selected.  (Clear out all lineedits?)

        BC0 = self.bcs_current_indices[0]

        self.bcs_set_volume_fraction_limit()

        def get_widget(key):
            for pat in ('lineedit_keyword_%s_args_BC_P',
                        'lineedit_%s_args_BC_P',
                        'lineedit_keyword_%s',
                        'lineedit_%s'):
                widget = getattr(ui, pat % key, None)
                if widget:
                    return widget
            self.error('no widget for key %s' % key)

        def setup_key_widget(key, default=None, enabled=True, suffix=''):
            for pat in ('label_%s', 'label_%s_units',
                         'lineedit_keyword_%s_args_BC_P',
                         'lineedit_%s_args_BC_P',
                         'lineedit_keyword_%s',
                         'lineedit_%s'):
                name = pat % (key+suffix)
                item = getattr(ui, name, None)
                if item:
                    item.setEnabled(enabled)
            args = mkargs(key, bc=BC0, phase=P)
            val = self.project.get_value(key, args=args)
            if val is None and enabled:
                val = default
            get_widget(key+suffix).updateValue(key, val, args=args)
            for BC in self.bcs_current_indices:
                self.update_keyword(key, val, args=mkargs(key, bc=BC, phase=P))

        #All inputs are optional. They do not have default values, because MFIX will calculate
        #appropriate values if they are unspecified and 'backflow' occurs at the outlet.
        #  (label in UI indicates this)

        #    Define volume fraction
        # Specification always available
        # Sets keyword BC_EP_S(#,#)
        # No DEFAULT value
        # Error Check: If any volume fraction for the BC region is specified, then all
        # volume fractions for the BC region must be specified and must sum to 1.0.  TODO
        key = 'bc_ep_s'
        default = None
        enabled= True
        setup_key_widget(key, default, enabled, suffix='_2')

        #    Define temperature
        # Specification always available
        # NO DEFAULT value
        # Sets keyword BC_T_S(#,#)
        key = 'bc_t_s'
        default = None
        enabled= True
        setup_key_widget(key, default, enabled, suffix='_2')

        #    Select species and set mass fractions (table format)
        # Specification always available
        # NO DEFAULT value
        # Sets keyword BC_X_S(#,#,#)
        # Error check: if specified, mass fractions must sum to 1.0
        #
        # We already have a mass fraction table - just steal it and move it here
        comp = ui.groupbox_solids_composition
        parent = comp.parent()
        if parent != ui.page_solids_po:
            comp.hide()
            parent.layout().removeWidget(comp)
            # (Should we put it inside the 'optional' box?  or underneath?)
            layout = ui.page_solids_po.layout() #Underneath - nested groupbox looks weird
            layout.insertWidget(layout.count()-1, comp)
            comp.show()
        self.update_bcs_solids_mass_fraction_table()


    def setup_bcs_scalar_po_tab(self):
        #Subtask Pane Tab for PRESSURE OUTFLOW type (PO) Boundary Condition Regions
        #Scalar (tab) - Tab only available if scalar equations are solved
        #All inputs are optional. They do not have default values, because MFIX will calculate appropriate
        # values if they are unspecified and 'backflow' occurs at the outlet.
        #Define scalar value
        # Sets keyword BC_SCALAR(#,#)
        # NO DEFAULT value
        ui = self.ui.boundary_conditions
        nscalar = self.project.get_value('nscalar', default=0)
        old_nscalar = getattr(ui, 'nscalar_po', None)
        old_scalar_bc_type = getattr(ui, 'scalar_bc_type', None)
        ui.nscalar_wall = nscalar
        ui.scalar_bc_type = 'PO'

        page = ui.page_scalar
        page_layout = page.layout()
        spacer = None
        key = 'bc_scalar'
        if nscalar != old_nscalar or old_scalar_bc_type != 'PO':
            spacer = self.clear_bcs_scalar_tab()
            label = QLabel('<i>MFiX will calculate appropriate values if inputs are unspecified and backflow occurs at the outlet.</i>')
            label.setWordWrap(True)
            page_layout.addWidget(label, 0, 0, 1, -1)
            for i in range(1, nscalar+1):
                label = QLabel('Scalar %s' % i)
                page_layout.addWidget(label, i, 0)
                le = LineEdit()
                le.key = key
                le.dtype = float
                le.allow_parameters = True
                le.args = ['BC', i]
                self.add_tooltip(le, key)
                self.project.register_widget(le, [key], ['BC', i])
                setattr(ui, 'lineedit_keyword_%s_args_BC_%s' % (key, i), le)
                page_layout.addWidget(le, i, 1)

            if spacer:
                page_layout.addItem(spacer)

        if not self.bcs_current_indices:
            return
        BC0 = self.bcs_current_indices[0]
        for i in range(1, nscalar+1):
            le = getattr(ui, 'lineedit_keyword_%s_args_BC_%s' % (key, i), None)
            if not le:
                self.error("No widget for %s %s" % (key, i))
                continue
            args = [BC0, i]
            val = self.project.get_value(key, args=args)
            if val is not None:
                le.updateValue(key, val, args=args)


    def setup_bcs_fluid_mo_tab(self):
        #Subtask Pane Tab for MASS OUTFLOW type (MO) Boundary Condition Regions
        #Fluid (tab)
        #  NB, we use the '_3' suffix in this section for consistency, even though it is not
        #  needed everywhere
        ui = self.ui.boundary_conditions
        ui.page_fluid.setCurrentIndex(PAGE_MO)

        if not self.bcs_current_indices:
            return
        BC0 = self.bcs_current_indices[0]

        bc_type = self.project.get_value('bc_type', args=[BC0])
        if bc_type is None:
            self.error("bc_type not set for region %s" % BC0)
            return

        def get_widget(key):
            for pat in ('lineedit_keyword_%s_args_BC',
                        'lineedit_%s_args_BC',
                        'lineedit_keyword_%s',
                        'lineedit_%s'):
                widget = getattr(ui, pat % key, None)
                if widget:
                    return widget
            self.error('no widget for key %s' % key)

        def setup_key_widget(key, default=None, enabled=True, suffix=''):
            for pat in ('label_%s', 'label_%s_units',
                        'lineedit_keyword_%s_args_BC',
                        'lineedit_%s_args_BC',
                        'lineedit_keyword_%s',
                        'lineedit_%s'):
                name = pat % (key+suffix)
                item = getattr(ui, name, None)
                if item:
                    item.setEnabled(enabled)
            args = [BC0]
            val = self.project.get_value(key, args=args)
            if val is None and enabled:
                val = default
            get_widget(key+suffix).updateValue(key, val, args=args)
            for BC in self.bcs_current_indices:
                self.update_keyword(key, val, args=[BC])


        #    Define outflow properties: Mass outflow specification changes
        # based on the BC_TYPE and Region orientation (e.g., XZ-Plane)
        #
        region_name = self.bcs[BC0].get('region')
        if not region_name:  # should not happen!
            self.error("No region defined for BC %s" % BC0)
            return
        region_info = self.bcs_region_dict.get(region_name)
        if not region_info:
            self.error("No definition for region %s" % region_name)
            return
        region_type = region_info.get('type')
        if not region_type:
            self.error("No type for region %s" % region_name)
            return

        # For BC_TYPE='MO' and XZ-Plane region
        # For BC_TYPE='MO' and YZ-Plane region
        # For BC_TYPE='MO' and XY-Plane region
        #  Select mass outflow specification type:
        if bc_type == 'MO':
            ui.stackedwidget_fluid_outflow.setCurrentIndex(0) # subpage_fluid_outflow_MO
            ui.label_fluid_velocities_3.show()
            if not region_type.endswith('plane'):
                self.error("Invalid type %s for region %s" % (region_type, region_name))
                return
            tangents = [C for C in 'XYZ' if C in region_type]
            normal = [C for C in 'XYZ' if C not in tangents]
            if len(normal) != 1 or len(tangents) != 2:
                self.error("Invalid type %s for region %s" % (region_type, region_name))
                return
            normal = normal[0]

            cb = ui.combobox_fluid_outflow_type
            item = get_combobox_item(cb, 0)
            item.setText('%s-axial velocity' % normal)
            outflow_type = self.bcs[BC0].get('fluid_outflow_type')
            keys = ['bc_%s_g'%xmap[normal], 'bc_volflow_g', 'bc_massflow_g']
            vals = [self.project.get_value(k, args=[BC0]) for k in keys]
            if outflow_type is None:
                vel_axial, vol_flow, mass_flow = vals
                if (vel_axial is not None) and (vol_flow is None) and (mass_flow is None):
                    outflow_type = 0
                elif (vel_axial is None) and (vol_flow is not None) and (mass_flow is None):
                    outflow_type = 1
                elif (vel_axial is None) and (vol_flow is None) and (mass_flow is not None):
                    outflow_type = 2
                else: # warn?
                    outflow_type = 0 # default
                for BC in self.bcs_current_indices:
                    self.bcs[BC]['fluid_outflow_type'] = outflow_type

            cb.setCurrentIndex(outflow_type)
            key = keys[outflow_type]
            val = vals[outflow_type]
            le = ui.lineedit_fluid_outflow
            le.key = key
            le.args = ['BC']
            le.dtype = float
            le.allow_parameters = True
            self.add_tooltip(le, key)
            le.updateValue(key, 0.0 if val is None else val)
            units = ['m/s', 'm³/s', 'kg/s'][outflow_type]
            ui.label_fluid_outflow_units.setText(units)

            #  Define Tangential Velocities:
            # Tangential velocity 1
            # Sets keyword BC_[UVW]_G(#)
            # DEFAULT 0.0
            key = 'bc_%s_g'%xmap[tangents[0]]
            default = 0.0
            label = ui.label_fluid_velocity_1_3
            label.setText('  %s-axial velocity' % tangents[0]) # Indent in GUI
            self.add_tooltip(label, key)
            val = self.project.get_value(key, args=[BC0])
            if val is None:
                val = default
                for BC in self.bcs_current_indices:
                    self.update_keyword(key, val, args=[BC])
            widget = ui.lineedit_fluid_velocity_1_3
            widget.key = key
            widget.args = ['BC']
            widget.dtype = float
            widget.allow_parameters = True
            widget.updateValue(key, val)
            self.add_tooltip(widget, key)

            # Tangential velocity 2
            # Sets keyword BC_[UVW]_G(#)
            # DEFAULT 0.0
            key = 'bc_%s_g'%xmap[tangents[1]]
            label = ui.label_fluid_velocity_2_3
            label.setText('  %s-axial velocity' % tangents[1]) # Indent in GUI
            self.add_tooltip(label, key)
            default = 0.0
            val = self.project.get_value(key, args=[BC0])
            if val is None:
                val = default
                for BC in self.bcs_current_indices:
                    self.update_keyword(key, val, args=[BC])
            widget = ui.lineedit_fluid_velocity_2_3
            widget.key = key
            widget.args = ['BC']
            widget.dtype = float
            widget.allow_parameters = True
            widget.updateValue(key, val)
            self.add_tooltip(widget, key)

        # For BC_TYPE='CG_MO'
        elif bc_type == 'CG_MO':
            #  Specify all velocity components:
            ui.stackedwidget_fluid_outflow.setCurrentIndex(1) # subpage_fluid_outflow_CG_MO
            ui.label_fluid_velocities_3.hide()

            #    Define X-Axial Velocity
            # Sets keyword BC_U_G(#)
            # DEFAULT 0.0
            key = 'bc_u_g'
            default = 0.0
            widget = ui.lineedit_fluid_outflow
            widget.key = key
            widget.args = ['BC']
            widget.dtype = float
            widget.allow_parameters = True
            val = self.project.get_value(key, args=[BC0])
            if val is None:
                val = default
                for BC in self.bcs_current_indices:
                    self.update_keyword(key, val, args=[BC])
            widget.updateValue(key, val)
            self.add_tooltip(widget, key)

            #    Define Y-Axial Velocity
            # Sets keyword BC_V_G(#)
            # DEFAULT 0.0
            key = 'bc_v_g'
            label =  ui.label_fluid_velocity_1_3
            label.setText('Y-axial velocity')
            self.add_tooltip(label, key)
            default = 0.0
            widget = ui.lineedit_fluid_velocity_1_3
            val = self.project.get_value(key, args=[BC0])
            if val is None:
                val = default
                for BC in self.bcs_current_indices:
                    self.update_keyword(key, val, args=[BC])
            widget.key = key
            widget.args = ['BC']
            widget.dtype = float
            widget.allow_parameters = True
            self.add_tooltip(widget, key)

            #    Define Z-Axial Velocity
            # Sets keyword BC_V_G(#) # fixed, BC_W_G
            # DEFAULT 0.0
            key = 'bc_w_g'
            label =  ui.label_fluid_velocity_2_3
            label.setText('Z-axial velocity')
            self.add_tooltip(label, key)
            default = 0.0
            widget = ui.lineedit_fluid_velocity_2
            val = self.project.get_value(key, args=[BC0])
            if val is None:
                val = default
                for BC in self.bcs_current_indices:
                    self.update_keyword(key, val, args=[BC])
            widget.key = key
            widget.args = ['BC']
            widget.dtype = float
            widget.allow_parameters = True
            self.add_tooltip(widget, key)

        #Define duration to average outflow rate.
        # Specification always available
        # Input required
        # Sets keyword BC_DT_0(#)
        # DEFAULT 0.1
        # TODO: See comments in mfix_user_guide re: BC_DT_0 and restarting
        enabled = True
        key = 'bc_dt_0'
        default = 0.1
        setup_key_widget(key, default, enabled)

        #The remaining inputs are only required when either the mass or the volumetric flow rates are
        #specified. They are not required if the velocities are given for the outlet.

        #Define volume fraction
        # Specification only available with mass or volumetric flow rates.
        # Input required  # TODO decide if we allow specifiying fluid vol. fractions,
        #                   or set them from 1-bc_ep_s
        # Sets keyword BC_EP_G(#)
        # DEFAULT value 1.0
        # Error Check: If any volume fraction for the BC region is specified, then all volume
        #fractions for the BC region must be specified and must sum to 1.0.
        enabled = True
        key = 'bc_ep_g'
        default = None
        setup_key_widget(key, default, enabled, suffix='_3')
        get_widget(key+'_3').setReadOnly(True)
        #get_widget(key+'_3').setEnabled(False) # better way to indicate read-only?

        #Define temperature
        # Specification only available with mass or volumetric flow rates and R_G0 is UNDEFINED (RO_G0)
        # DEFAULT value 293.15
        # Sets keyword BC_T_G(#)
        ro_g0 = self.project.get_value('ro_g0')
        mass_flow = self.project.get_value('bc_massflow_g', args=[BC0])
        vol_flow = self.project.get_value('bc_volflow_g', args=[BC0])
        enabled = (ro_g0 is None) and (mass_flow is not None or vol_flow is not None)
        default = 293.15 if enabled else None
        key = 'bc_t_g'
        setup_key_widget(key, default, enabled, suffix='_3')

        #Select species and set mass fractions (table format)
        # Specification only available with mass or volumetric flow rates and R_G0 is UNDEFINED (RO_G0)
        # DEFAULT value 1.0 of last defined species
        # Sets keyword BC_X_G(#,#)
        # Error check: if specified, mass fractions must sum to 1.0
        #
        enabled = (ro_g0 is None) and (mass_flow is not None or vol_flow is not None)
        comp = ui.groupbox_fluid_composition
        parent = comp.parent()
        if enabled:
            if parent != ui.page_fluid_mo:
                # Steal the table widget from wherever it is
                comp.hide()
                parent.layout().removeWidget(comp)
                layout = ui.page_fluid_mo.layout()
                layout.insertWidget(layout.count()-1, comp)
            comp.show()
            self.update_bcs_fluid_mass_fraction_table()
        elif parent == ui.page_fluid_mo:
            # Table widget is here, but we don't want it
            #comp.hide()
            comp.setEnabled(False)


    def setup_bcs_solids_mo_tab(self, P):
        #Subtask Pane Tab for MASS OUTFLOW type (MO) Boundary Condition Regions
        #Solids-# (tab)
        #  NB, we use the '_4' suffix in this section for consistency, even though it is not
        #  needed everywhere
        ui = self.ui.boundary_conditions
        ui.page_solids.setCurrentIndex(PAGE_MO)

        self.bcs_current_solid = self.P = P
        if P is None:
            return
        if not self.bcs_current_indices:
            return

        BC0 = self.bcs_current_indices[0]

        def get_widget(key):
            for pat in ('lineedit_keyword_%s_args_BC_P',
                        'lineedit_%s_args_BC_P',
                        'lineedit_keyword_%s',
                        'lineedit_%s'):
                widget = getattr(ui, pat % key, None)
                if widget:
                    return widget
            self.error('no widget for key %s' % key)

        def setup_key_widget(key, default=None, enabled=True, suffix=''):
            for pat in ('label_%s', 'label_%s_units',
                         'lineedit_keyword_%s_args_BC_P',
                         'lineedit_%s_args_BC_P',
                         'lineedit_keyword_%s',
                         'lineedit_%s'):
                name = pat % (key+suffix)
                item = getattr(ui, name, None)
                if item:
                    item.setEnabled(enabled)

            args = mkargs(key, bc=BC0, phase=P)
            val = self.project.get_value(key, args=args)
            if val is None and enabled:
                val = default
            get_widget(key+suffix).updateValue(key, val, args=args)
            for BC in self.bcs_current_indices:
                self.update_keyword(key, val, args=mkargs(key, bc=BC, phase=P))


        bc_type = self.project.get_value('bc_type', args=[BC0])
        if bc_type is None:
            self.error("bc_type not set for region %s" % BC0)
            return

        #Define outflow properties: Mass outflow specification changes
        #based on the BC_TYPE and Region orientation (e.g., XZ-Plane)
        #
        region_name = self.bcs[BC0].get('region')
        if not region_name:  # should not happen!
            self.error("No region defined for BC %s" % BC0)
            return
        region_info = self.bcs_region_dict.get(region_name)
        if not region_info:
            self.error("No definition for region %s" % region_name)
            return
        region_type = region_info.get('type')
        if not region_type:
            self.error("No type for region %s" % region_name)
            return

        # For BC_TYPE='MO' and XZ-Plane region
        # For BC_TYPE='MO' and YZ-Plane region
        # For BC_TYPE='MO' and XY-Plane region
        if bc_type == 'MO':
            ui.stackedwidget_solids_outflow.setCurrentIndex(0) # subpage_solids_outflow_MO
            ui.label_solids_velocities_4.show()
            if not region_type.endswith('plane'):
                self.error("Invalid type %s for region %s" % (region_type, region_name))
                return
            tangents = [C for C in 'XYZ' if C in region_type]
            normal = [C for C in 'XYZ' if C not in tangents]
            if len(normal) != 1 or len(tangents) != 2:
                self.error("Invalid type %s for region %s" % (region_type, region_name))
                return
            normal = normal[0]

            cb = ui.combobox_solids_outflow_type
            item = get_combobox_item(cb, 0)
            item.setText('%s-axial velocity' % normal)
            outflow_type = self.bcs[BC0].get('solids_outflow_type', {}).get(P)
            keys = ['bc_%s_s'%xmap[normal], 'bc_volflow_s', 'bc_massflow_s']
            vals = [self.project.get_value(k, args=[BC0,P]) for k in keys]
            if outflow_type is None:
                vel_axial, vol_flow, mass_flow = vals
                if (vel_axial is not None) and (vol_flow is None) and (mass_flow is None):
                    outflow_type = 0
                elif (vel_axial is None) and (vol_flow is not None) and (mass_flow is None):
                    outflow_type = 1
                elif (vel_axial is None) and (vol_flow is None) and (mass_flow is not None):
                    outflow_type = 2
                else: # warn?
                    outflow_type = 0 # default
                for BC in self.bcs_current_indices:
                    if 'solids_outflow_type' not in self.bcs[BC]:
                        self.bcs[BC]['solids_outflow_type'] = {}
                    self.bcs[BC]['solids_outflow_type'][P] = outflow_type

            cb.setCurrentIndex(outflow_type)
            key = keys[outflow_type]
            val = vals[outflow_type]
            le = ui.lineedit_solids_outflow
            le.key = key
            le.args = ['BC', 'P']
            le.dtype = float
            le.allow_parameters = True
            self.add_tooltip(le, key)
            le.updateValue(key, 0.0 if val is None else val)
            units = ['m/s', 'm³/s', 'kg/s'][outflow_type]
            ui.label_solids_outflow_units.setText(units)

            #  Define Tangential Velocities:
            # Tangential velocity 1
            # Sets keyword BC_[UVW]_S(#, #)
            # DEFAULT 0.0
            key = 'bc_%s_s'%xmap[tangents[0]]
            default = 0.0
            label = ui.label_solids_velocity_1_4
            label.setText('  %s-axial velocity' % tangents[0]) # Indent in GUI
            self.add_tooltip(label, key)
            val = self.project.get_value(key, args=[BC0,P])
            if val is None:
                val = default
                for BC in self.bcs_current_indices:
                    self.update_keyword(key, val, args=[BC, P])
            widget = ui.lineedit_solids_velocity_1_4
            widget.key = key
            widget.args = ['BC','P']
            widget.dtype = float
            widget.allow_parameters = True
            widget.updateValue(key, val)
            self.add_tooltip(widget, key)

            # Tangential velocity 2
            # Sets keyword BC_[UVW]_S(#, #)
            # DEFAULT 0.0
            key = 'bc_%s_s'%xmap[tangents[1]]
            label = ui.label_solids_velocity_2_4
            label.setText('  %s-axial velocity' % tangents[1]) # Indent in GUI
            self.add_tooltip(label, key)
            default = 0.0
            val = self.project.get_value(key, args=[BC0,P])
            if val is None:
                val = default
                for BC in self.bcs_current_indices:
                    self.update_keyword(key, val, args=[BC, P])
            widget = ui.lineedit_solids_velocity_2_4
            widget.key = key
            widget.args = ['BC','P']
            widget.dtype = float
            widget.allow_parameters = True
            widget.updateValue(key, val)
            self.add_tooltip(widget, key)

        # For BC_TYPE='CG_MO'
        elif bc_type == 'CG_MO':
            #  Specify all velocity components:
            ui.stackedwidget_solids_outflow.setCurrentIndex(1) # subpage_solids_outflow_CG_MO
            ui.label_solids_velocities_4.hide()

            #    Define X-Axial Velocity
            # Sets keyword BC_U_S(#,#)
            # DEFAULT 0.0
            key = 'bc_u_s'
            default = 0.0
            widget = ui.lineedit_solids_outflow
            val = self.project.get_value(key, args=[BC0,P])
            if val is None:
                val = default
                for BC in self.bcs_current_indices:
                    self.update_keyword(key, val, args=[BC, P])
            widget.key = key
            widget.args = ['BC','P']
            widget.dtype = float
            widget.allow_parameters = True
            widget.updateValue(key, val)
            self.add_tooltip(widget, key)

            #    Define Y-Axial Velocity
            # Sets keyword BC_V_S(#,#)
            # DEFAULT 0.0
            key = 'bc_v_s'
            label =  ui.label_solids_velocity_1_4
            label.setText('Y-axial velocity')
            self.add_tooltip(label, key)
            default = 0.0
            widget = ui.lineedit_solids_velocity_1_4
            val = self.project.get_value(key, args=[BC0,P])
            if val is None:
                val = default
                for BC in self.bcs_current_indices:
                    self.update_keyword(key, val, args=[BC, P])
            widget.key = key
            widget.args = ['BC','P']
            widget.dtype = float
            widget.allow_parameters = True
            self.add_tooltip(widget, key)

            #    Define Z-Axial Velocity
            # Sets keyword BC_W_S(#,#)
            # DEFAULT 0.0
            key = 'bc_w_s'
            label =  ui.label_solids_velocity_2_4
            label.setText('Z-axial velocity')
            self.add_tooltip(label, key)
            default = 0.0
            widget = ui.lineedit_solids_velocity_2_4
            val = self.project.get_value(key, args=[BC0,P])
            if val is None:
                val = default
                for BC in self.bcs_current_indices:
                    self.update_keyword(key, val, args=[BC, P])
            widget.key = key
            widget.args = ['BC','P']
            widget.dtype = float
            widget.allow_parameters = True
            self.add_tooltip(widget, key)

        #Define duration to average outflow rate.
        # Specification always available
        # Input required
        # Sets keyword BC_DT_0(#)
        # DEFAULT 0.1
        # TODO: See comments in mfix_user_guide re: BC_DT_0 and restarting
        enabled = True
        key = 'bc_dt_0'
        default = 0.1
        setup_key_widget(key, default, enabled, suffix='_4')
        # TODO: No mass fraction table?  No volume fraction?


    def setup_bcs_scalar_mo_tab(self):
        # ? not mentioned in SRS JORDAN?
        ui = self.ui.boundary_conditions
        ui.scalar_bc_type = 'MO'
        page = ui.page_scalar
        page_layout = page.layout()
        spacer = self.clear_bcs_scalar_tab()
        if spacer:
            page_layout.addItem(spacer)


    def set_cyclic_pd(self, val):
        if not self.bcs_current_indices:
            return
        BC0 = self.bcs_current_indices[0]
        if not self.bc_is_cyclic(BC0):
            return
        ui = self.ui.boundary_conditions
        axis = BC0[0].lower()
        key = 'cyclic_%s_pd' % axis
        delp_key = 'delp_%s' % axis
        if val:
            self.update_keyword(key, True)
            delp_val = ui.lineedit_delp.value
            if delp_val is None:
                delp_val = 0.0 # default
            self.update_keyword(delp_key, delp_val)
        else:
            self.unset_keyword(key)
            self.unset_keyword('delp_%s' % axis)
            self.unset_keyword('flux_g')
        self.setup_bcs_cyclic_tab()


    def enable_flux_g(self, enabled):
        ui = self.ui.boundary_conditions
        for item in (ui.lineedit_keyword_flux_g, ui.label_flux_g_units):
            item.setEnabled(enabled)
        key = 'flux_g'
        if enabled:
            value = ui.lineedit_keyword_flux_g.value
            if value in (None, ''):
                value = 0.0 # default
                ui.lineedit_keyword_flux_g.updateValue(key, value)
            self.update_keyword(key, value)
        else:
            self.unset_keyword(key)


    def setup_bcs_cyclic_tab(self):
        # Subtask Pane Tab for Cyclic type Boundary Conditions
        if not self.bcs_current_indices:
            return
        BC0 = self.bcs_current_indices[0]
        if not self.bc_is_cyclic(BC0):
            return
        ui = self.ui.boundary_conditions

        # Make sure the rest of the tabs are disabled (why is this needed?)
        ncols = ui.tab_layout.columnCount()
        for i in range(ncols-1):
            item = ui.tab_layout.itemAtPosition(0, i)
            if not item:
                continue
            widget = item.widget()
            if not widget:
                continue
            widget.setEnabled(False)
        ui.pushbutton_cyclic.setEnabled(True)

        #Enable specified pressure drop
        # DEFAULT .FALSE.
        # Sets keyword based on axis:
        #  Q-Axis;
        #    Sets keyword CYCLIC_Q_PD to .TRUE.
        #    Required input for DELP_Q
        # DEFAULT 0.0

        # Error check: There should not be any BCs defined on walls that are cyclic.
        # (I'm not sure if this check can be easily implemented.)
        axis = BC0[0].lower()
        key = 'cyclic_%s_pd' % axis
        default = 0.0
        cb = ui.checkbox_cyclic_pd
        self.add_tooltip(cb, key)
        enabled = True
        # Error check: Only one axis can have a specified pressure drop
        for q in 'xyz':
            if q == axis:
                continue
            if self.project.get_value('cyclic_%s_pd'%q) is not None:
                enabled = False
                break
        cb.setEnabled(enabled)
        checked = self.project.get_value(key) is not None
        cb.setChecked(checked)
        if not enabled:
            for item in ui.lineedit_delp, ui.lineedit_keyword_flux_g:
                item.setText('')

        key = 'delp_%s' % axis
        le = ui.lineedit_delp
        le.key = key
        le.args = None
        self.add_tooltip(le, key)
        for item in (le, ui.label_delp_units, ui.checkbox_flux_g):
            item.setEnabled(checked and enabled)
        if not (checked and enabled): # Don't enable, leave that to checkbox_flux_g
            for item in (ui.lineedit_keyword_flux_g, ui.label_flux_g_units):
                item.setEnabled(False)
            ui.checkbox_flux_g.setChecked(False)
        if checked and enabled:
            val = self.project.get_value(key)
            if val is None:
                val = default
                self.update_keyword(key, val)
            le.updateValue(key, val)

        #Enable specified gas mass flux
        # Requires specified pressure drop
        # Sets keyword FLUX_G
        # DEFAULT 0.0
        # Error check: Only one axis can have a specified mass flux. (This should not be an
        #issue as it requires a specified pressure drop which can only be applied to one axis.)
        key = 'flux_g'
        default = 0.0
        val = self.project.get_value(key)
        cb = ui.checkbox_flux_g
        cb.setChecked(checked and enabled and (val is not None))
        le = ui.lineedit_keyword_flux_g
        for item in (le, ui.label_flux_g_units):
            item.setEnabled(enabled and cb.isChecked())
        if enabled and cb.isChecked():
            if val is None:
                val = default
                self.update_keyword(key, val)
            le.updateValue(key, val)
