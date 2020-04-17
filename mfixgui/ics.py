# -*- coding: utf-8 -*-
""" Initial Conditions pane"""

from qtpy.QtCore import Qt
from qtpy.QtGui import QPicture
from qtpy.QtWidgets import (QLabel, QPushButton, QWidget,
                            QTableWidgetItem, QHeaderView)
UserRole = Qt.UserRole

from mfixgui.widgets.base import LineEdit

from mfixgui.tools import safe_float
from mfixgui.tools.qt import (set_item_noedit, get_selected_row,
                              widget_iter, sub_icon_height)
from mfixgui.tools.keyword_args import mkargs, keys_by_type
from mfixgui.constants import *

# We don't need extended JSON here
from json import JSONDecoder, JSONEncoder

FLUID_TAB = 0
SOLIDS_TAB_DUMMY_L = 1
SOLIDS_TAB = 2
SOLIDS_TAB_DUMMY_R = 3
SCALAR_TAB = 4

# Columns in tablewidget_regions
COLUMN_REGION, COLUMN_ID = range(2)

class ICS(object):
    # Initial Conditions Task Pane Window: This section allows a user to define the initial conditions
    # for the described model. This section relies on regions named in the Regions section.

    def init_ics(self):
        ui = self.ui.initial_conditions

        self.ics = {} # key: index.  value: data dictionary for initial cond
        self.ics_current_indices = [] # List of IC indices
        self.ics_current_regions = [] # And the names of the regions which define them
        self.ics_region_dict = None
        self.ics_saved_solids_names = []

        # The top of the task pane is where users define/select IC regions
        # (see handle_ics_region_selection)
        #
        #Icons to add/remove/duplicate regions are given at the top
        #Clicking the 'add' and 'duplicate' buttons triggers a popup window where the user must select
        #the region to apply the initial condition.
        #Implementation Idea: Allow the user to select multiple regions in the popup window so that they can
        #define one IC region over multiple regions. Managing the MFIX IC indices on the back end could
        #get messy, and if so, scrap this idea.
        # (Note- Suggestion implemented)
        # TODO: implement 'duplicate' (what does this do?)
        ui.toolbutton_add.clicked.connect(self.ics_show_regions_popup)
        ui.toolbutton_delete.clicked.connect(self.ics_delete_regions)
        # Up/down buttons
        ui.toolbutton_up.clicked.connect(self.ics_row_up)
        ui.toolbutton_down.clicked.connect(self.ics_row_down)

        for tb in (ui.toolbutton_delete, ui.toolbutton_up, ui.toolbutton_down):
            tb.setEnabled(False) # Need a selection

        ui.tablewidget_regions.itemSelectionChanged.connect(self.handle_ics_region_selection)

        self.ics_current_tab = FLUID_TAB # If fluid is disabled, we will switch
        self.ics_current_solid = self.P = None
        ui.pushbutton_fluid.pressed.connect(lambda: self.ics_change_tab(FLUID_TAB,None))
        ui.pushbutton_scalar.pressed.connect(lambda: self.ics_change_tab(SCALAR_TAB,None))

        # Trim width of "Fluid" and "Scalar" buttons, like we do for
        # dynamically-created "Solid #" buttons
        for b in (ui.pushbutton_fluid, ui.pushbutton_scalar):
            w = b.fontMetrics().boundingRect(b.text()).width() + 20
            b.setMaximumWidth(w)

        # Not auto-registered with project manager
        widget = ui.lineedit_ic_ep_s_args_IC_P
        key = 'ic_ep_s'
        widget.key = key
        widget.args = ['IC', 'P']
        self.add_tooltip(widget, key)
        widget.dtype = float
        widget.value_updated.connect(self.handle_ics_volume_fraction)

        widget = ui.lineedit_ic_p_star_args_IC
        widget.key = 'ic_p_star'
        widget.args = ['IC']
        widget.dtype = float
        widget.value_updated.connect(self.handle_ic_p_star)
        self.add_tooltip(widget, key)


    def handle_ic_p_star(self, widget, data, args):
        if not self.ics_current_indices:
            return
        IC0 = self.ics_current_indices[0]
        key = 'ic_p_star'
        prev_val = self.project.get_value(key, args=[IC0])
        ignore, new_val = data.popitem()

        if len(self.solids) > 1:
            resp=self.message(text="Pressure setting applies to all solids phases\nAre you sure?",
                              buttons=['yes','no'],
                              default = 'no')
            if resp != 'yes': # Reject update, set lineedit back to previous value
                widget.updateValue(key, self.project.get_value(key, prev_val))
                return
        for IC in self.ics_current_indices:
            self.update_keyword(key, new_val, args=[IC])


    def ics_set_volume_fraction_limit(self):
        if not self.ics_current_indices:
            return
        if not self.ics_current_solid:
            return
        IC0 = self.ics_current_indices[0]
        P = self.ics_current_solid
        ui = self.ui.initial_conditions
        key = 'ic_ep_s'

        total = sum(safe_float(self.project.get_value(key, default=0, args=[IC0, s]))
                for s in range(1, len(self.solids)+1) if s != P)

        lim = max(0, 1.0 - total)
        lim = round(lim, 10) # avoid problem with 1 - 0.9 != 0.1

        widget = ui.lineedit_ic_ep_s_args_IC_P
        widget.min = 0.0
        widget.max = lim


    def handle_ics_volume_fraction(self, widget, val, args):
        if not self.ics_current_indices:
            return
        IC0 = self.ics_current_indices[0]
        if not self.ics_current_solid:
            return
        key = 'ic_ep_s'
        self.project.submit_change(widget, val, args)

        total = sum(safe_float(self.project.get_value(key, default=0, args=[IC0, i]))
                for i in range(1, len(self.solids)+1))
        if total > 1.0:
            self.warning("Volume fractions sum to %s, must be <= 1.0" % total,
                         popup=True)
            return # ?
        # Set ic_ep_g from ic_ep_s (issues/121)
        val = round(1.0 - total, 10)
        for IC in self.ics_current_indices:
            self.update_keyword('ic_ep_g', val, args=[IC])

        # usr warning if particle generation not checked (issues/441)
        if self.project.solver == DEM and total>0 and not self.project.get_value('gener_part_config', default=False):
            rsp = self.message(text="Automatic particle generation is not enabled.\nWould you like to enable it?",
                                 buttons=['yes', 'no'])
            if rsp == 'yes':
                self.update_keyword('gener_part_config', True)
                self.unset_keyword('particles')


    def ics_show_regions_popup(self):
        #Users cannot select inapplicable regions.
        # IC regions must be volumes or planes (not points or STLs)
        #  Volumes are always valid IC regions
        #  XY-Planes are valid IC regions for 2D simulations (NO_K=.TRUE.)
        #  XZ- and YZ Planes are never valid IC regions
        #No region can define more than one initial condition.
        ui = self.ui.initial_conditions
        rp = self.regions_popup
        rp.clear()
        no_k = self.project.get_value('no_k', default=False)

        for (name,data) in self.ics_region_dict.items():
            shape = data.get('type', '---')
            # Assume available if unmarked
            available = (data.get('available', True)
                         and (shape == 'box'
                              or (no_k and shape=='XY-plane')))
            row = (name, shape, available)
            rp.add_row(row)
        rp.reset_signals()
        rp.save.connect(self.ics_add_regions)
        rp.cancel.connect(self.ics_cancel_add)
        for item in (ui.tablewidget_regions,
                     ui.bottom_frame,
                     ui.toolbutton_add,
                     ui.toolbutton_delete,
                     ui.toolbutton_up,
                     ui.toolbutton_down):
            item.setEnabled(False)
        rp.popup('Select region(s) for initial conditions')


    def ics_cancel_add(self):
        ui = self.ui.initial_conditions

        for item in (ui.toolbutton_add,
                     ui.tablewidget_regions):
            item.setEnabled(True)
        row = get_selected_row(ui.tablewidget_regions)
        nrows = ui.tablewidget_regions.rowCount()
        if row is not None:
            for item in (ui.bottom_frame,
                         ui.toolbutton_delete):
                item.setEnabled(True)
            if nrows > 1:
                ui.toolbutton_up.setEnabled(row != 0)
                ui.toolbutton_down.setEnabled(row != nrows-1)
        else:
            ui.toolbutton_up.setEnabled(False)
            ui.toolbutton_down.setEnabled(False)

    def ics_add_regions(self):
        # Interactively add regions to define ICs
        ui = self.ui.initial_conditions
        rp = self.regions_popup
        self.ics_cancel_add() # Reenable input widgets
        selections = rp.get_selection_list()
        if not selections:
            return
        self.ics_add_regions_1(selections, autoselect=True)
        self.ics_setup_current_tab() # Update the widgets

    def ics_add_regions_1(self, selections, indices=None, autoselect=False):
        # Used by both interactive and load-time add-region handlers
        ui = self.ui.initial_conditions

        if self.ics_region_dict is None:
            self.ics_region_dict = self.ui.regions.get_region_dict()

        tw = ui.tablewidget_regions
        nrows = tw.rowCount()
        tw.setRowCount(nrows+1)
        def make_item(val):
            item = QTableWidgetItem('' if val is None else str(val))
            set_item_noedit(item)
            return item
        item = make_item('+'.join(selections))

        if indices is None: # interactive
            indices = [None] * len(selections)
        else: # loading file
            assert len(selections) == len(indices)

        for (i, region_name) in enumerate(selections):
            idx = indices[i]
            if idx is None:
                idx = self.ics_find_index()
                indices[i] = idx
            self.ics[idx] = {'region': region_name}
            region_data = self.ics_region_dict.get(region_name)
            if region_data is None: # ?
                self.warn("no data for region %s" % region_name)
                continue
            self.ics_set_region_keys(region_name, idx, region_data)
            self.ics_region_dict[region_name]['available'] = False # Mark as in-use
        item.setData(UserRole, (tuple(indices), tuple(selections)))
        tw.setItem(nrows, COLUMN_REGION, item)

        item = make_item(','.join(map(str, indices)))
        tw.setItem(nrows, COLUMN_ID, item)

        self.fixup_ics_table(tw)

        if autoselect:
            tw.setCurrentCell(nrows, COLUMN_REGION)

        # Issues/513
        for i in indices:
            self.ics_set_default_keys(i)


    def ics_set_default_keys(self, IC):
        #Issues/513
        energy_eq = self.project.get_value('energy_eq', default=True)
        turbulence_model = self.project.get_value('turbulence_model', default=DEFAULT_TURBULENCE_MODEL)
        nmax_g = self.project.get_value('nmax_g', default=0)
        mmax = self.project.get_value('mmax', default=0)
        nscalar = self.project.get_value('nscalar', default=0)
        kt_type = self.project.get_value('kt_type', default=DEFAULT_KT_TYPE)
        solids_model = [None] + [self.project.get_value('solids_model', args=[i])
                                 for i in range(1, 1+mmax)]
        nmax_s = [None] + [self.project.get_value('nmax_s', default=0, args=[i])
                           for i in range(1,1+mmax)]

        #Fluid Tab
        #  Define volume fraction
        #  Specification never available
        #  Sets keyword IC_EP_G(#)
        #  CALCULATED from 1.0 - sum(IC_EP_s(#,#)
        #  GUI should show the result of the calculation.
        key = 'ic_ep_g'
        default = 1.0
        self.set_keyword_default(key, default, args=[IC])
        #  Define temperature
        #  Specification always available
        #  Input required for any of the following
        #  Fluid density model: Ideal Gas Law
        #  Fluid viscosity model: Sutherland's Law
        #  Energy equations are solved
        #  Sets keyword IC_T_G(#)
        #  DEFAULT 293.15
        key = 'ic_t_g'
        default = 293.15
        self.set_keyword_default(key, default, args=[IC])

        #  Define pressure (optional)
        #  Specification always available
        #  Sets keyword IC_P_g(#)
        #  DEFAULT - no input-

        #  Define velocity components (required)
        #  Specification always available
        #  Sets keywords IC_{U,V,W}_G(#), IC_V_G(#), IC_W_G(#)
        #  DEFAULT 0.0
        default = 0.0
        for c in 'uvw':
            key = 'ic_%s_g' % c
            self.set_keyword_default(key, default, args=[IC])

        #  Select species and set mass fractions (table format)
        #  Specification always available
        #  Sets keyword IC_X_G
        #  Input required for species equations
        #  Drop down menu of fluid species
        #  DEFAULT - last defined species has mass fraction of 1.0
        #  Error check: mass fractions must sum to 1.0
        key = 'ic_x_g'
        for i in range(1, 1+nmax_g):
            default = 0.0
            self.set_keyword_default(key, default, args=[IC,i])
        total = sum(safe_float(self.project.get_value(key, default=0, args=[IC,i]))
                    for i in range(1, nmax_g)) # All but last
        if total == 0 and nmax_g > 0:  # Set last species to 1, only if all other are 0
            self.update_keyword(key, 1.0, args=[IC, nmax_g])


        #  Turbulence: Define mixing length model length scale
        #  Specification only available with Mixing Length turbulence model
        #  Sets keyword IC_L_SCALE(#)
        #  DEFAULT 1.0
        if turbulence_model == 'MIXING_LENGTH':
            key = 'ic_l_scale'
            default = 1.0
            self.set_keyword_default(key, default, args=[IC])

        #  Turbulence: Define k-ε turbulent kinetic energy
        #  Specification only available with K-Epsilon turbulence model
        #  Sets keyword IC_K_TURB_G(#)
        #  DEFAULT 0.0
        if turbulence_model == 'K_EPSILON':
            key =  'ic_k_turb_g'
            default = 0.0
            self.set_keyword_default(key, default, args=[IC])

        #  Turbulence: Define k-ε turbulent dissipation
        #  Specification only available with K-Epsilon turbulence model
        #  Sets keywords IC_E_TURB_G(#)
        #  DEFAULT 0.0
        if turbulence_model == 'K_EPSILON':
            key = 'ic_e_turb_g'
            default = 0.0
            self.set_keyword_default(key, default, args=[IC])

        #  Advanced: Define radiation coefficient
        #  Specification only available when solving energy equations
        #  Sets keyword IC_GAMA_RG(#)
        #  DEFAULT 0.0
        if energy_eq:
            key = 'ic_gama_rg'
            default = 0.0
            self.set_keyword_default(key, default, args=[IC])

        #  Advanced: Define radiation temperature
        #  Specification only available when solving energy equations
        #  Sets keyword IC_T_RG(#)
        #  DEFAULT 293.15
        if energy_eq:
            key = 'ic_t_rg'
            default = 293.15
            self.set_keyword_default(key, default, args=[IC])

        #Solids
        #  Define volume fraction (required)
        #  Sets keyword IC_EP_S(#,#)
        #  DEFAULT 0.0
        key = 'ic_ep_s'
        default = 0.0
        for P in range(1, 1+mmax):
            self.set_keyword_default(key, default, args=[IC,P])

        #  Define temperature
        #  Specification always available
        #  Sets keyword IC_T_S(#,#)
        #  DEFAULT 293.15
        key = 'ic_t_s'
        default = 293.15
        for P in range(1, 1+mmax):
            self.set_keyword_default(key, default, args=[IC,P])

        #  Define velocity components (required)
        #  Sets keywords IC_{U,V,W}_S(#,#)
        #  DEFAULT 0.0
        default = 0.0
        for P in range(1, 1+mmax):
            for c in 'uvw':
                key = 'ic_%s_s' % c
                self.set_keyword_default(key, default, args=[IC,P])

        #  Define pressure (optional)

        #  Define granular temperature
        #  Specification only available for SOLIDS_MODEL(#)='TFM' and
        #   non-algebraic formulation viscous stress model (see continuous solids
        #   model section) or for SOLIDS_MODEL(#)=DEM' or SOLIDS_MODEL(#)='PIC'
        #   Sets keyword IC_THETA_M(#,#)
        #   DEFAULT 0.0
        key = 'ic_theta_m'
        default = 0.0
        for P in range(1, 1+mmax):
            if (solids_model[P]=='TFM' and kt_type != 'ALGEBRAIC'
                or solids_model[P] in ('PIC', 'DEM')):
                self.set_keyword_default(key, default, args=[IC,P])

        # Define particles per parcel
        #  Specification only available for SOLIDS_MODEL(#)='PIC'
        #  Sets keyword IC_PIC_CONST_STATWT(#,#)
        #  DEFAULT 10.0
        key = 'ic_pic_const_statwt'
        default = 10.0
        for P in range(1, 1+mmax):
            if solids_model[P] == 'PIC':
                self.set_keyword_default(key, default, args=[IC,P])

        # Select species and set mass fractions (table format)
        #   Sets keyword IC_X_S(#,#,#)
        #   DEFAULT - last defined species has mass fraction of 1.0
        key = 'ic_x_s'
        for P in range(1, 1+mmax):
            for i in range(1, 1+nmax_s[P]):
                default = 0.0
                self.set_keyword_default(key, default, args=[IC, P, i])
            total = sum(safe_float(self.project.get_value(key, default=0, args=[IC, P, i]))
                        for i in range(1, nmax_s[P])) # All but last
            if total == 0 and nmax_s[P] > 0:  # Set last species to 1, only if all other are 0
                self.update_keyword(key, 1.0, args=[IC, P, nmax_s[P]])

        # Advanced: Option to enable fitting DES particles to region
        #  Sets keyword: IC_DES_FIT_TO_REGION
        #  Disabled [DEFAULT]
        key = 'ic_des_fit_to_region'
        default = False
        self.set_keyword_default(key, default, args=[IC])

        # Advanced: Define radiation coefficient
        #  Specification only available when solving energy equations
        #  Sets keyword IC_GAMA_RS(#,#)
        #  DEFAULT 0.0
        if energy_eq:
            key = 'ic_gama_rs'
            default = 0.0
            for P in range(1, 1+mmax):
                self.set_keyword_default(key, default, args=[IC, P])

        # Advanced: Define radiation temperature
        #  Specification only available when solving energy equations
        #  Sets keyword IC_T_RS(#,#)
        #  DEFAULT 293.15
        if energy_eq:
            key = 'ic_t_rs'
            default = 293.15
            for P in range(1, 1+mmax):
                self.set_keyword_default(key, default, args=[IC, P])

        # Scalar Tab
        #  Sets keyword IC_SCALAR(#,#)
        #  DEFAULT 0.0
        key = 'ic_scalar'
        default = 0.0
        for i in range(1, 1+nscalar):
            self.set_keyword_default(key, default, args=[IC,i])


    def ics_find_index(self):
        # Always add new IC at end
        return 1 if not self.ics else 1 + max(self.ics)


    def ics_row_up(self):
        ui = self.ui.initial_conditions
        tw = ui.tablewidget_regions
        row = get_selected_row(tw)
        if row is None: # No selection
            return
        if row == 0: # Cannot move up
            return
        row_1 = row-1 # Previous row
        (indices, regions) = tw.item(row,COLUMN_REGION).data(UserRole)
        (indices_1, regions_1) = tw.item(row_1,COLUMN_REGION).data(UserRole)
        self.ics_permute(indices_1+indices, indices+indices_1)
        col = COLUMN_REGION # Just swap the table items
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


    def ics_row_down(self):
        ui = self.ui.initial_conditions
        tw = ui.tablewidget_regions
        row = get_selected_row(tw)
        if row is None: # No selection
            return
        if row >= tw.rowCount() -1: # Cannot move down
            return
        row_1 = row+1 # Next row
        (indices, regions) = tw.item(row,COLUMN_REGION).data(UserRole)
        (indices_1, regions_1) = tw.item(row_1,COLUMN_REGION).data(UserRole)
        self.ics_permute(indices+indices_1, indices_1+indices)
        col = COLUMN_REGION # Just swap the table items
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


    def ics_permute(self, old_order, new_order):
        data = {x: {} for x in old_order}
        for key in keys_by_type['ic']:
            for args in self.project.get_key_indices(key):
                if args and args[0] in old_order:
                    data[args[0]][(key, args[1:])]=self.project.get_value(key, args=args)
        ics_copy = self.ics.copy()
        for (old, new) in zip(old_order, new_order):
            # Update keys
            for ((key, extra_args), val) in data[new].items():
                self.update_keyword(key, val, args=(old,)+extra_args)
            # Clear any remaining keys
            for ((key, extra_args), val) in data[old].items():
                if (key, extra_args) not in data[new]:
                    self.unset_keyword(key, args=(old,)+extra_args)

            # Update internal memoized values
            tmp = ics_copy.pop(new, None)
            if tmp:
                self.ics[old] = tmp
            else:
                self.ics.pop(old, None)


    def ics_delete_regions(self):
        ui = self.ui.initial_conditions
        tw = ui.tablewidget_regions
        row = get_selected_row(tw)
        if row is None: # No selection
            return

        # Unset keywords
        kwlist = list(self.project.keywordItems())
        for kw in kwlist:
            key, args = kw.key, kw.args
            if key.startswith('ic_') and args and args[0] in self.ics_current_indices:
                self.unset_keyword(key, args=args)

        for r in self.ics_current_regions:
            if r in self.ics_region_dict:
                self.ics_region_dict[r]['available'] = True

        for i in self.ics_current_indices:
            del self.ics[i]

        self.ics_current_regions = []
        self.ics_current_indices = []

        tw.removeRow(row)
        self.fixup_ics_table(tw)
        self.ics_setup_current_tab()
        self.update_nav_tree()
        nrows = tw.rowCount()
        row = get_selected_row(tw)
        ui.toolbutton_up.setEnabled(row is not None and nrows > 1 and row != 0)
        ui.toolbutton_down.setEnabled(row is not None and nrows > 1 and row != nrows-1)


    def ics_delete_solids_phase(self, phase_index):
        """adjust ics_current_solid when solids phase deleted"""
        if (self.ics_current_solid is not None and
            self.ics_current_solid >= phase_index):
            self.ics_current_solid -= 1
            if self.ics_current_solid == 0:
                self.ics_current_solid = None


    def handle_ics_region_selection(self):
        ui = self.ui.initial_conditions
        table = ui.tablewidget_regions
        row = get_selected_row(table)
        nrows = table.rowCount()
        if row is None:
            indices = []
            regions = []
        else:
            (indices, regions) = table.item(row,COLUMN_REGION).data(UserRole)
        self.ics_current_indices, self.ics_current_regions = indices, regions
        enabled = (row is not None)
        for item in (ui.toolbutton_delete,
                     ui.bottom_frame):
            item.setEnabled(enabled)
        ui.toolbutton_up.setEnabled(row is not None and nrows > 1 and row != 0)
        ui.toolbutton_down.setEnabled(row is not None and nrows > 1 and row != nrows-1)

        if not enabled:
            # Clear
            for widget in widget_iter(ui.bottom_frame):
                if isinstance(widget, LineEdit):
                    widget.setText('')
            return
        self.ics_setup_current_tab() # reinitialize all widgets in current tab
        ui.scrollarea_detail.ensureVisible(0, 0) # scroll to top



    def fixup_ics_table(self, tw, stretch_column=0):
        ui = self.ui.initial_conditions
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

        if tw == ui.tablewidget_regions: # main table, adjust top splitter
            icon_height = sub_icon_height() + 8
            ui.top_frame.setMaximumHeight(height+icon_height)
            ui.top_frame.setMinimumHeight(header_height+icon_height+row_height*min(nrows,5))
            ui.top_frame.updateGeometry()
            tw.setMaximumHeight(height)
            tw.setMinimumHeight(header_height)
        else: # mass fraction tables
            if tw == ui.tablewidget_solids_mass_fraction:
                ui.groupbox_solids_composition.setMaximumHeight(height+30) # FIXME hardcoded size
            elif tw == ui.tablewidget_fluid_mass_fraction:
                ui.groupbox_fluid_composition.setMaximumHeight(height+30)
            tw.setMaximumHeight(height) # Works for tablewidget inside groupbox
            tw.setMinimumHeight(height) #? needed? should we allow scrollbar?
        tw.updateGeometry() #? needed?


    def ics_update_enabled(self):
        if self.ics:
            # Never disable if there are ICs defined
            disabled = False
        else:
            # If there are no solids, no scalar equations, and the fluid solver is disabled,
            # then we have no input tabs on the ICs pane, so disable it completely
            regions = self.ui.regions.get_region_dict()
            nregions = sum(1 for (name, r) in regions.items()
                           if r.get('type')=='box')
            disabled = (nregions==0
                        or (self.fluid_solver_disabled
                            and self.project.get_value('nscalar',default=0)==0
                            and len(self.solids)==0))
        self.find_navigation_tree_item("Initial Conditions").setDisabled(disabled)


    def ics_change_tab(self, tab, solid):
        ui = self.ui.initial_conditions
        index = (0 if tab==FLUID_TAB
                 else len(self.solids)+1 if tab==SCALAR_TAB
                 else solid)

        for i in range(ui.tab_layout.columnCount()):
            item = ui.tab_layout.itemAtPosition(0, i)
            if not item:
                continue
            widget = item.widget()
            if not widget:
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
            if solid == self.ics_current_solid:
                return # Really nothing to do

            if solid > (self.ics_current_solid or 0):
                dummy_label = ui.label_dummy_solids_L
                dummy_tab = SOLIDS_TAB_DUMMY_L
            else:
                dummy_label = ui.label_dummy_solids_R
                dummy_tab = SOLIDS_TAB_DUMMY_R

            p = QPicture()
            ui.page_solids.render(p, flags=QWidget.DrawChildren)  #avoid rendering bg
            dummy_label.setPicture(p)
            ui.stackedwidget.setCurrentIndex(dummy_tab)

        self.ics_current_tab = tab
        self.ics_current_solid = self.P = solid if tab==SOLIDS_TAB else None
        self.ics_setup_current_tab()

        # change stackedwidget contents
        self.animate_stacked_widget(
            ui.stackedwidget,
            ui.stackedwidget.currentIndex(),
            tab,
            direction='horizontal',
            line = ui.tab_underline,
            to_btn = ui.tab_layout.itemAtPosition(0, index),
            btn_layout = ui.tab_layout)
        # Scroll to top
        ui.scrollarea_detail.ensureVisible(0, 0)


    def ics_check_region_in_use(self, name):
        return any(data.get('region')==name for data in self.ics.values())


    def ics_update_region(self, name, data):
        for (i,ic) in self.ics.items():
            if ic.get('region') == name:
                self.ics_set_region_keys(name, i, data)


    def ics_set_region_keys(self, name, idx, data):
        # Update the keys which define the region the IC applies to
        no_k = self.project.get_value('no_k')
        for (key, val) in zip(('x_w', 'y_s', 'z_b',
                               'x_e', 'y_n', 'z_t'),
                              data['from']+data['to']):
            # ic_z_t and ic_z_b keywords should not be added when no_k=True
            if no_k and key in ('z_t', 'z_b'):
                continue
            self.update_keyword('ic_'+key, val, args=[idx])


    def ics_change_region_name(self, old_name, new_name):
        ui = self.ui.initial_conditions
        for (key, val) in self.ics.items():
            if val.get('region') == old_name:
                self.ics[key]['region'] = new_name
                tw = ui.tablewidget_regions
                for i in range(tw.rowCount()):
                    data = tw.item(i,COLUMN_REGION).data(UserRole)
                    indices, names = data
                    if key in indices:
                        item = tw.item(i,COLUMN_REGION)
                        names = [new_name if n==old_name else n for n in names]
                        item.setData(UserRole, (tuple(indices), tuple(names)))
                        item.setText('+'.join(names))
                        break
                break


    def reset_ics(self):
        self.ics.clear()
        self.ics_current_indices = []
        self.ics_current_regions = []
        self.ics_region_dict = None
        self.ics_current_solid = self.P = None
        ui = self.ui.initial_conditions
        ui.tablewidget_regions.clearContents()
        ui.tablewidget_regions.setRowCount(0)
        # anything else to do here?
        # TODO remove dynamically created input widgets, although this should
        #  get handled next time we call 'setup'

    def ic_regions_to_str(self):
        ui = self.ui.initial_conditions
        tw = ui.tablewidget_regions
        data = [tw.item(i,COLUMN_REGION).data(UserRole)
                for i in range(tw.rowCount())]
        return JSONEncoder().encode(data)


    def ics_regions_from_str(self, s):
        if not s:
            return
        data = JSONDecoder().decode(s)
        for (indices, regions) in data:
            self.ics_add_regions_1(regions, indices, autoselect=False)


    def setup_ics(self):
        ui = self.ui.initial_conditions
        # Grab a fresh copy, may have been updated
        self.ics_region_dict = self.ui.regions.get_region_dict()

        # Mark regions which are in use (this gets reset each time we get here)
        for (i, data) in self.ics.items():
            region = data['region']
            if region in self.ics_region_dict:
                self.ics_region_dict[region]['available'] = False

        self.fixup_ics_table(ui.tablewidget_regions)
        row = get_selected_row(ui.tablewidget_regions)
        # Autoselect if only 1 row
        if row is None and ui.tablewidget_regions.rowCount() == 1:
            row = 0
            ui.tablewidget_regions.setCurrentCell(row, COLUMN_REGION)
        enabled = (row is not None)
        for item in (ui.toolbutton_delete,
                     ui.bottom_frame):
            item.setEnabled(enabled)

        #Tabs group initial condition parameters for phases and additional equations.
        # Tabs are unavailable if no input is required from the user.

        #Fluid tab - Unavailable if the fluid phase was disabled.
        b = ui.pushbutton_fluid
        b.setText(self.fluid_phase_name)
        b.setEnabled(not self.fluid_solver_disabled)
        font = b.font()
        font.setBold(self.ics_current_tab == FLUID_TAB)
        b.setFont(font)

        #Each solid phase will have its own tab. The tab name should be the name of the solid
        solids_names = list(self.solids.keys())
        if self.ics_saved_solids_names != solids_names:
            # Clear out the old ones
            n_cols = ui.tab_layout.columnCount()
            for i in range(n_cols-1, 0, -1):
                item = ui.tab_layout.itemAtPosition(0, i)
                if not item:
                    continue
                widget = item.widget()
                if not widget:
                    continue
                if widget in (ui.pushbutton_fluid, ui.pushbutton_scalar):
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
                font = b.font()
                font.setBold(self.ics_current_tab==SOLIDS_TAB and i==self.ics_current_solid)
                b.setFont(font)
                b.pressed.connect(lambda i=i: self.ics_change_tab(SOLIDS_TAB, i))
                ui.tab_layout.addWidget(b, 0, i)

        #Scalar (tab) - Tab only available if scalar equations are solved
        # Move the 'Scalar' button to the right of all solids, if needed
        b = ui.pushbutton_scalar
        font = b.font()
        font.setBold(self.ics_current_tab==SCALAR_TAB)
        b.setFont(font)
        nscalar = self.project.get_value('nscalar', default=0)
        enabled = (nscalar > 0)
        b.setEnabled(enabled)
        if len(self.solids) != len(self.ics_saved_solids_names):
            ui.tab_layout.removeWidget(b)
            ui.tab_layout.addWidget(b, 0, 1+len(self.solids))

        self.ics_saved_solids_names = solids_names
        self.P = self.ics_current_solid

        # Don't stay on a disabled tab
        index = self.ics_tab_to_index(self.ics_current_tab, self.ics_current_solid)
        item = None if index is None else ui.tab_layout.itemAtPosition(0, index)
        b = item.widget() if item else None
        if ui.isEnabled() and not (b and b.isEnabled()):
            self.ics_change_tab(*self.ics_find_valid_tab())
        else:
            self.ics_setup_current_tab()

        # make sure underline is in the right place, as # of solids may
        # have changed (lifted from animate_stacked_widget, which we
        # don't want to call here)
        tab = self.ics_current_tab
        line_to = self.ics_tab_to_index(tab, self.ics_current_solid)
        line = ui.tab_underline
        btn_layout = ui.tab_layout
        if line_to is not None:
            btn_layout.addItem(btn_layout.takeAt(
                btn_layout.indexOf(line)), 1, line_to)

    def ics_tab_to_index(self, tab, solid):
        return (0 if tab==FLUID_TAB
                else len(self.solids)+1 if tab==SCALAR_TAB
                else solid)

    def ics_find_valid_tab(self):
        if not self.fluid_solver_disabled:
            return (FLUID_TAB, None)
        elif self.solids:
            return (SOLIDS_TAB, 1)
        elif self.project.get_value('nscalar', default=0) != 0:
            return (SCALAR_TAB, None)
        else:
            self.error("Initial condition:  all tabs disabled!")
            return (FLUID_TAB, None) # What else to do?


    def ics_setup_current_tab(self):
        if self.ics_current_tab == FLUID_TAB:
            self.setup_ics_fluid_tab()
        elif self.ics_current_tab == SOLIDS_TAB:
            self.setup_ics_solids_tab(self.ics_current_solid)
        elif self.ics_current_tab == SCALAR_TAB:
            self.setup_ics_scalar_tab()


    def update_ics_fluid_mass_fraction_table(self):
        ui = self.ui.initial_conditions
        table = ui.tablewidget_fluid_mass_fraction
        table.clearContents()
        table.setRowCount(0)
        if not (self.fluid_species and self.ics_current_indices):
            self.fixup_ics_table(table)
            ui.groupbox_fluid_composition.setEnabled(False)
            return
        ui.groupbox_fluid_composition.setEnabled(True)
        IC0 = self.ics_current_indices[0]
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
            key = 'ic_x_g'
            le.key = key
            le.args = [self.ics_current_indices, row+1]
            self.add_tooltip(le, key)
            val = self.project.get_value(key, args=[IC0, row+1], default=None)
            if val is not None:
                le.updateValue(key, val)
            le.value_updated.connect(self.handle_ics_fluid_mass_fraction)
            table.setCellWidget(row, 1, le)
        if species:
            table.setItem(nrows-1, 0, make_item("Total"))
            table.setItem(nrows-1, 1, make_item(''))
            item = table.item(nrows-1, 0)
            font = item.font()
            font.setBold(True)
            item.setFont(font)
            self.update_ics_fluid_mass_fraction_total()
        self.fixup_ics_table(table)

    def handle_ics_fluid_mass_fraction(self, widget, value_dict, args):
        key = 'ic_x_g'
        val = value_dict[key]
        widget.updateValue(key, val)
        if val == '':
            self.unset_keyword(key, args=args)
        else:
            self.update_keyword(key, val, args=args)

        # DEFAULT - last defined species has mass fraction of 1.0
        # see also ics_set_default_keys
        N = len(self.fluid_species)
        for IC in args[0]:
            total = sum(safe_float(self.project.get_value(key, default=0, args=[IC,i]))
                        for i in range(1, 1+N))
            for i in range(1, 1+N):
                default = float(i==N) if total==0 else 0.0
                self.set_keyword_default(key, default, args=[IC, i])
        self.update_ics_fluid_mass_fraction_table()

    def update_ics_fluid_mass_fraction_total(self):
        if not self.ics_current_indices:
            return
        if not self.fluid_species:
            return
        IC0 = self.ics_current_indices[0]
        ui = self.ui.initial_conditions
        key = 'ic_x_g'
        table = ui.tablewidget_fluid_mass_fraction
        if table.rowCount() == 0:
            return
        total = sum(safe_float(self.project.get_value(key, default=0.0, args=[IC0,i]))
                    for i in range(1,len(self.fluid_species)+1))
        total = round(total, 6)
        item =  table.item(table.rowCount()-1, 1)
        font = item.font()
        font.setBold(True)
        item.setFont(font)
        item.setText(str(total))
        if total != 1.0:
            item.setForeground(Qt.red)
            #We should warn, but this creates too many popups while IC is being set up
            #self.warning("Mass fractions sum to %s, must be 1.0" % total, popup=True)
        elif ui.isEnabled():
            item.setForeground(Qt.black) # FIXME looks wrong when greyed-out


    def update_ics_solids_mass_fraction_table(self):
        ui = self.ui.initial_conditions
        table = ui.tablewidget_solids_mass_fraction
        table.clearContents()
        table.setRowCount(0)
        P = self.ics_current_solid
        if not (P and self.solids_species.get(P) and self.ics_current_indices):
            self.fixup_ics_table(table)
            table.setEnabled(False)
            ui.groupbox_solids_composition.setEnabled(False)
            return
        ui.groupbox_solids_composition.setEnabled(True)
        table.setEnabled(True)
        IC0 = self.ics_current_indices[0]
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
            le = LineEdit()
            le.setdtype('dp')
            le.setValInfo(min=0.0, max=1.0)
            key = 'ic_x_s'
            le.key = key
            le.args = [self.ics_current_indices, P, row+1]
            self.add_tooltip(le, key)
            val = self.project.get_value(key, args=[IC0, P, row+1], default=None)
            if val is not None:
                le.updateValue(key, val)
            le.value_updated.connect(self.handle_ics_solids_mass_fraction)
            table.setCellWidget(row, 1, le)
        if species:
            table.setItem(nrows-1, 0, make_item("Total"))
            table.setItem(nrows-1, 1, make_item(''))
            item = table.item(nrows-1, 0)
            font = item.font()
            font.setBold(True)
            item.setFont(font)
            self.update_ics_solids_mass_fraction_total()
        self.fixup_ics_table(table)

    def handle_ics_solids_mass_fraction(self, widget, value_dict, args):
        key = 'ic_x_s'
        val = value_dict[key]
        widget.updateValue(key, val)
        if val == '':
            self.unset_keyword(key, args=args)
        else:
            self.update_keyword(key, val, args=args)
        P = args[1]
        # DEFAULT - last defined species has mass fraction of 1.0
        # see also ics_set_default_keys
        species = self.solids_species[P]
        if species:
            N = len(species)
            for IC in args[0]:
                total = sum(safe_float(self.project.get_value(key, default=0, args=[IC,P,i]))
                            for i in range(1, 1+N))
                for i in range(1, 1+N):
                    default = float(i==N) if total==0 else 0.0
                    self.set_keyword_default(key, default, args=[IC,P,i])
        self.update_ics_solids_mass_fraction_table()

    def update_ics_solids_mass_fraction_total(self):
        if not self.ics_current_indices:
            return
        IC0 = self.ics_current_indices[0]
        P = self.ics_current_solid
        if P is None:
            return
        species = self.solids_species.get(P)
        if not P:
            return
        ui = self.ui.initial_conditions
        key = 'ic_x_s'
        table = ui.tablewidget_solids_mass_fraction
        if table.rowCount() == 0:
            return
        total = sum(safe_float(self.project.get_value(key, default=0.0, args=[IC0,P,i]))
                    for i in range(1,len(species)+1))
        total = round(total, 6)
        item = table.item(table.rowCount()-1, 1)
        font = item.font()
        font.setBold(True)
        item.setFont(font)
        item.setText(str(total))
        if total != 1.0:
            item.setForeground(Qt.red)
            #We should warn, but this creates too many popups while IC is being set up
            #self.warning("Mass fractions sum to %s, must be 1.0" % total, popup=True)
        elif ui.isEnabled():
            item.setForeground(Qt.black) # FIXME looks wrong when greyed-out


    def ics_extract_regions(self):
        if self.ics:
            # We assume that IC regions have been initialized correctly
            # from mfix_gui_comments.
            # TODO: verify that there is an IC region for each IC
            return

        if self.ics_region_dict is None:
            self.ics_region_dict = self.ui.regions.get_region_dict()

        # TODO: if we wanted to be fancy, we could find regions where
        # IC values matched, and merge into a new IC region.  That
        # is only needed for projects created outside the GUI (otherwise
        # we have already stored the IC regions).  Also would be nice
        # to offer a way to split compound regions.
        for ic in self.project.ics:

            d = ic.keyword_dict
            extent = [d.get('ic_'+k,None) for k in ('x_w', 'y_s', 'z_b',
                                                    'x_e', 'y_n', 'z_t')]
            extent = [0 if x is None else x.value for x in extent]
            #if any (x is None for x in extent):
            #    self.warn("initial condition %s: invalid extents %s" %
            #               (ic.ind, extent))
            #    continue
            for (region_name, data) in self.ics_region_dict.items():
                ext2 = [0 if x is None else x for x in
                        (data.get('from',[]) + data.get('to',[]))]
                if ext2 == extent:
                    if data.get('available', True):
                        self.ics_add_regions_1([region_name], indices=[ic.ind], autoselect=False)
                        break
            else:
                self.warn("initial condition %s: could not match defined region %s" %
                          (ic.ind, extent))
                kwlist = list(self.project.keywordItems())
                for kw in kwlist:
                    key, args = kw.key, kw.args
                    if key.startswith('ic_') and args and args[0]==ic.ind:
                        self.unset_keyword(key, args=args)


    def setup_ics_fluid_tab(self):
        #Fluid (tab)
        if self.fluid_solver_disabled:
            # we shouldn't be on this tab!
            return
        ui = self.ui.initial_conditions
        tw = ui.tablewidget_fluid_mass_fraction
        enabled = bool(self.fluid_species)
        if not enabled:
            tw.clearContents()
            tw.setRowCount(0)
        self.fixup_ics_table(tw)
        tw.setEnabled(enabled)

        if not self.ics_current_indices:
            # Nothing selected.  What can we do? (Clear out all lineedits?)
            return

        IC0 = self.ics_current_indices[0]
        # Note - value may not be consistent across grouped regions
        #  For now we're going to assume that it is, and just check
        #  first subregion of IC group

        #  If we can make this code generic enough perhaps someday it can
        # be autogenerated from SRS doc

        def get_widget(key):
            for pat in ('lineedit_keyword_%s_args_IC',
                        'lineedit_%s_args_IC'):
                widget = getattr(ui, pat % key)
                if widget:
                    return widget
            self.error('no widget for key %s' % key)

        def setup_key_widget(key, default=None, enabled=True):
            for pat in ('label_%s', 'label_%s_units',
                        'lineedit_keyword_%s_args_IC'):
                name = pat%key
                item = getattr(ui, name, None)
                if item:
                    item.setEnabled(enabled)
            if not enabled:
                get_widget(key).setText('') #?
                return
            args = mkargs(key, ic=IC0)
            val = self.project.get_value(key, args=args)
            if val is None:
                val = default
                for IC in self.ics_current_indices:
                    self.update_keyword(key, val, args=mkargs(key, ic=IC))
            get_widget(key).updateValue(key, val, args=args)

        #Define volume fraction
        # Specification always available (No, it's computed from IC_EP_S)
        # Sets keyword IC_EP_G(#)
        # DEFAULT 1.0
        # (terminology:  is ic_ep_g volume fraction or void fraction?)
        key = 'ic_ep_g'
        default = 1.0
        setup_key_widget(key, default)
        get_widget(key).setReadOnly(True)
        self.add_tooltip(get_widget(key), key, description=self.keyword_doc[key]['description'] + '\nThis is computed from solids volume fraction and cannot be specified directly.')
        #get_widget(key).setEnabled(False) # better way to indicate read-only?
        # Issues/121 ; make non-editable

        #Define temperature
        # Specification always available
        # Input required for any of the following
        # Fluid density model: Ideal Gas Law
        # Fluid viscosity model: Sutherland's Law
        # Energy equations are solved
        # Sets keyword IC_T_G(#)
        # DEFAULT 293.15
        #TODO: how do we enforce "required" inputs?
        key = 'ic_t_g'
        default = 293.15
        setup_key_widget(key, default)

        #Define pressure (optional)  TODO "optional" in label?
        # Specification always available
        # Sets keyword IC_P_g(#)
        # DEFAULT - no input
        key = 'ic_p_g'
        default = None
        setup_key_widget(key, default)

        #Define velocity components (required)
        # Specification always available
        # Sets keywords IC_U_G(#), IC_V_G(#), IC_W_G(#)
        # DEFAULT 0.0
        default = 0.0
        for key in ('ic_u_g', 'ic_v_g', 'ic_w_g'):
            setup_key_widget(key, default)

        #Select species and set mass fractions (table format)
        # Specification always available
        # Input required for species equations TODO: implement 'required'
        # Drop down menu of fluid species
        # DEFAULT - last defined species has mass fraction of 1.0  # implemented in update_ics_fluid_mass_fraction_table
        # Error check: mass fractions must sum to one   # we show total but don't warn if != 1.0
        self.update_ics_fluid_mass_fraction_table()

        key = 'turbulence_model'
        turbulence_model = self.project.get_value(key, default=DEFAULT_TURBULENCE_MODEL)
        enabled = (turbulence_model in ('MIXING_LENGTH', 'K_EPSILON'))
        ui.groupbox_turbulence.setEnabled(enabled)

        #Turbulence: Define mixing length model length scale
        # Specification only available with Mixing Length turbulence model
        # Sets keyword IC_L_SCALE(#)
        # DEFAULT 1.0
        key = 'ic_l_scale'
        default = 1.0
        enabled = (turbulence_model == 'MIXING_LENGTH')
        setup_key_widget(key, default, enabled)

        #Turbulence: Define k-ε turbulent kinetic energy
        # Specification only available with K-Epsilon turbulence model
        # Sets keyword IC_K_TURB_G(#)
        # DEFAULT 0.0
        key = 'ic_k_turb_g'
        default = 0.0
        enabled = (turbulence_model == 'K_EPSILON')
        setup_key_widget(key, default, enabled)

        #Turbulence: Define k-ε turbulent dissipation
        # Specification only available with K-Epsilon turbulence model
        # Sets keywords IC_E_TURB_G(#)
        # DEFAULT 0.0
        key = 'ic_e_turb_g'
        default = 0.0
        enabled = (turbulence_model == 'K_EPSILON')
        setup_key_widget(key, default, enabled)

        energy_eq = self.project.get_value('energy_eq', default=True)
        enabled = bool(energy_eq)
        ui.groupbox_fluid_advanced.setEnabled(enabled)

        #Advanced: Define radiation coefficient
        # Specification only available when solving energy equations
        # Sets keyword IC_GAMA_RG(#)
        # DEFAULT 0.0
        key = 'ic_gama_rg'
        default = 0.0
        enabled = bool(energy_eq)
        setup_key_widget(key, default, enabled)

        #Advanced: Define radiation temperature
        # Specification only available when solving energy equations
        # Sets keyword IC_T_RG(#)
        # DEFAULT 293.15
        key = 'ic_t_rg'
        default =  293.15
        enabled = bool(energy_eq)
        setup_key_widget(key, default, enabled)


    def setup_ics_solids_tab(self, P):
        # Solid-# (tab) - Rename tab to user provided solids name.

        # Note, solids phases are numbered 1-N
        self.ics_current_solid = self.P = P
        if P is None: # Nothing to do
            return

        if not self.ics_current_indices: # No region selected
            # TODO clear all widgets (?)
            return

        ui = self.ui.initial_conditions
        IC0 = self.ics_current_indices[0]

        # issues/121
        self.ics_set_volume_fraction_limit()

        # Generic!
        def get_widget(key):
            for pat in ('lineedit_keyword_%s_args_IC_P',
                        'lineedit_keyword_%s_args_IC',
                        'lineedit_%s_args_IC_P',
                        'lineedit_%s_args_IC'):
                widget = getattr(ui, pat % key, None)
                if widget:
                    return widget
            self.error('no widget for key %s' % key)

        def setup_key_widget(key, default=None, enabled=True):
            for pat in ('label_%s', 'label_%s_units',
                         'lineedit_keyword_%s_args_IC_P',
                         'lineedit_keyword_%s_args_IC',
                         'lineedit_%s_args_IC_P',
                         'lineedit_%s_args_IC'):
                name = pat%key
                item = getattr(ui, name, None)
                if item:
                    item.setEnabled(enabled)
                    if isinstance(item, LineEdit) and item.dtype in (int, float):
                        item.allow_parameters = True
            if not enabled:
                get_widget(key).setText('') #?
                return
            args = mkargs(key, ic=IC0, phase=P)
            val = self.project.get_value(key, args=args)
            if val is None:
                val = default
                for IC in self.ics_current_indices:
                    self.update_keyword(key, val, args=mkargs(key, ic=IC, phase=P))
            get_widget(key).updateValue(key, val, args=args)

        #Group tab inputs by equation type (e.g., momentum, energy, species).
        # Making the grouped inputs a 'collapsible list' may make navigation easier.
        #  (Note - collaspsing not implemented)

        #Define volume fraction (required)
        # Specification always available
        # Sets keyword IC_EP_S(#,#)
        # DEFAULT 0.0
        key = 'ic_ep_s'
        default = 0.0
        # Some input decks may or may not contain IC_EP_S keyword:
        #  Volume fraction is specified using the solids bulk density
        #    IC_EP_S(#,#) == IC_ROP_S(#,#) / IC_ROs(#)

        #   Solids density IC_ROs is determined by the solids density model. For
        #constant solids density, use RO_S0. For variable solids density, see
        #“Calculating Variable Solids Density” section in the appendix.

        #  Volume fraction may be inferred from IC_EP_G
        #    IC_EP_S(#,#) = 1.0 - IC_EP_G(#)
        #    Only valid for one solids phase (MMAX=1)
        # (note, this is handled in project_manager.load_project_file, see issues/142)
        #ic_ep_s = self.project.get_value(key, args=[IC0, P])
        #ic_ep_g = self.project.get_value('ic_ep_g', args=[IC0])
        #if ic_ep_s is None and ic_ep_g is not None and len(self.solids)==1:
        #    for IC in self.ics_current_indices:
        #        self.update_keyword(key, 1.0-ic_ep_g, args=[IC, P])

        setup_key_widget(key, default)

        #Define temperature
        # Specification always available
        # Input required when solving energy equations
        # Sets keyword IC_T_S(#,#)
        # DEFAULT 293.15
        energy_eq = self.project.get_value('energy_eq', default=True)
        key = 'ic_t_s'
        default = 293.15
        enabled = bool(energy_eq)
        setup_key_widget(key, default, enabled)

        #Define velocity components (required)
        # Specification always available
        # Sets keywords IC_U_S(#,#), IC_V_S(#,#), IC_W_S(#,#)
        # DEFAULT 0.0
        for key in 'ic_u_s', 'ic_v_s', 'ic_w_s':
            setup_key_widget(key, default=0.0)

        #Define pressure (optional)
        # Specification only available for SOLIDS_MODEL(#)='TFM'
        # Sets keyword IC_P_STAR(#)
        # DEFAULT of 0.0  (but optional?)
        # Common to all phases - Warn user if changed.
        solids_model = self.project.get_value('solids_model', args=[P])
        enabled = (solids_model=='TFM')
        key = 'ic_p_star'
        default = 0.0
        setup_key_widget(key, default, enabled)
        le = ui.lineedit_ic_p_star_args_IC
        self.add_tooltip(le, key)
        if not enabled:
            tooltip = le.toolTip()
            tooltip += '<br>Only enabled for TFM solids'
            le.setToolTip(tooltip)

        #Define granular temperature
        # Specification only available for SOLIDS_MODEL(#)='TFM' and non-algebraic
        # formulation viscous stress model (see continuous solids model section) or for
        # SOLIDS_MODEL(#)=DEM' or SOLIDS_MODEL(#)='PIC'
        # Sets keyword IC_THETA_M(#,#)
        # DEFAULT 0.0
        # Use (m^2/sec^2) for solids granular energy units.
        # # Note:
        # # Some of the KT_TYPES also include a mass unit (kg) -
        # # but the default model (Lun) will have units of m^s/sec^2
        solids_model = self.project.get_value('solids_model', args=[P])
        kt_type = self.project.get_value('kt_type', default=DEFAULT_KT_TYPE)
        enabled = ( (solids_model=='TFM' and kt_type != 'ALGEBRAIC')
                    or solids_model=='DEM'
                    or solids_model=='PIC')
        key = 'ic_theta_m'
        default = 0.0
        setup_key_widget(key, default, enabled)

        #Define particles per parcel
        # Specification only available for SOLIDS_MODEL(#)='PIC'
        # Sets keyword IC_PIC_CONST_STATWT(#,#)
        # DEFAULT 10.0
        enabled = (solids_model=='PIC')
        key = 'ic_pic_const_statwt'
        default = 10.0
        setup_key_widget(key, default, enabled)
        le = ui.lineedit_keyword_ic_pic_const_statwt_args_IC_P
        self.add_tooltip(le, key)
        if not enabled:
            tooltip = le.toolTip()
            tooltip += '<br>Only enabled for PIC solids'
            le.setToolTip(tooltip)

        #Select species and set mass fractions (table format)
        # Specification always available
        # Input required for species equations
        # Drop down menu of solids species
        # Sets keyword IC_X_S
        # DEFAULT - last defined species has mass fraction of 1.0
        # Error check: mass fractions must sum to one
        self.update_ics_solids_mass_fraction_table()

        enabled = (solids_model=='DEM' or bool(energy_eq))
        ui.groupbox_solids_advanced.setEnabled(enabled)

        #Advanced: Option to enable fitting DES particles to region
        # Option only available for DEM solids
        # Sets keyword: IC_DES_FIT_TO_REGION
        # Disabled [DEFAULT]
        enabled = (solids_model=='DEM')
        item = ui.checkbox_keyword_ic_des_fit_to_region_args_IC
        item.setEnabled(enabled)
        key = 'ic_des_fit_to_region'
        default = False
        val = self.project.get_value(key, args=[IC0])
        if val is None:
            val = default
            if enabled:
                for IC in self.ics_current_indices:
                    self.update_keyword(key, val, args=[IC])
        item.setChecked(val)
        # TODO: popup dialog to warn that this apples to all phases

        #Advanced: Define radiation coefficient
        # Specification only available when solving energy equations
        # Sets keyword IC_GAMA_RS(#,#)
        # DEFAULT 0.0
        enabled = bool(energy_eq)
        key = 'ic_gama_rs'
        default = 0.0
        setup_key_widget(key, default, enabled)

        #Advanced: Define radiation temperature
        # Specification only available when solving energy equations
        # Sets keyword IC_T_RS(#,#)
        # DEFAULT 293.15
        enabled = bool(energy_eq)
        key = 'ic_t_rs'
        default = 293.15
        setup_key_widget(key, default, enabled)


    def setup_ics_scalar_tab(self):
        #Note that this tab should only be available if scalar equations are being solved.
        #Furthermore, the number of scalars requiring input comes from the number of
        #scalar equations specified by the user.

        if not self.ics_current_indices:
            return # No selection
        IC0 = self.ics_current_indices[0]

        ui = self.ui.initial_conditions
        nscalar = self.project.get_value('nscalar', default=0)
        old_nscalar = getattr(ui, 'nscalar', None)
        ui.nscalar = nscalar

        key = 'ic_scalar'
        if nscalar == old_nscalar:
            # What do we have to do?  Just update the lineedits
            for i in range(1, nscalar+1):
                le = getattr(ui, "lineedit_ic_scalar_%s" % i, None)
                val = self.project.get_value(key, args=[IC0, i])
                if le:
                    le.updateValue(key, val)
            return

        page =  ui.page_scalar
        layout = page.layout()

        spacer = None
        for i in range(layout.rowCount()-1, -1, -1):
            for j in (1,0):
                item = layout.itemAtPosition(i,j)
                if not item:
                    continue
                widget = item.widget()
                if not widget:
                    spacer = item
                    continue
                if isinstance(widget, LineEdit):
                    self.project.unregister_widget(widget)
                widget.setParent(None)
                widget.deleteLater()

        if spacer:
            layout.removeItem(spacer)

        #Define initial scalar value
        #Sets keyword IC_SCALAR(#,#)
        #DEFAULT 0.0
        key = 'ic_scalar'
        row = 0
        for i in range(1, nscalar+1):
            label = QLabel('Scalar %s' % i)
            self.add_tooltip(label, key)
            layout.addWidget(label, row, 0)
            le = LineEdit()
            le.key = key
            le.args = ['IC', i]
            self.add_tooltip(le, key)
            le.dtype = float
            le.default_value = 0.0
            self.project.register_widget(le, [key], ['IC', i])
            setattr(ui, 'lineedit_ic_scalar_%s'%i, le)

            self.add_tooltip(le, key)
            val = self.project.get_value(key, args=[IC0, i])
            if val is None:
                val = 0.0
                for IC in self.ics_current_indices:
                    self.update_keyword(key, val, args=[IC, i])
            le.updateValue(key, val)
            layout.addWidget(le, row, 1)
            row += 1

        if spacer:
            layout.addItem(spacer, row, 0)
