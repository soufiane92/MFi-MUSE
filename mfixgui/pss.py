# -*- coding: utf-8 -*-
""" Point Sources pane"""

from qtpy.QtCore import Qt
from qtpy.QtWidgets import (QPushButton, QWidget,
                            QHeaderView, QTableWidgetItem)

from qtpy.QtGui import QPicture
UserRole = Qt.UserRole

from mfixgui.constants import *
from mfixgui.widgets.base import LineEdit

from mfixgui.tools import safe_float
from mfixgui.tools.qt import (set_item_noedit, get_selected_row,
                              sub_icon_height, widget_iter)

from mfixgui.tools.keyword_args import mkargs

# We don't need extended JSON here
from json import JSONDecoder, JSONEncoder

FLUID_TAB = 0
SOLIDS_TAB_DUMMY_L = 1
SOLIDS_TAB = 2
SOLIDS_TAB_DUMMY_R = 3

class PSS(object):
    #Point Source Task Pane Window: This section allows a user to define point sources for the
    #described model. This section relies on regions named in the Regions section.

    def init_pss(self):
        ui = self.ui.point_sources

        self.pss = {} # key: index.  value: data dictionary for point source
        self.pss_current_indices = [] # List of PS indices
        self.pss_current_regions = [] # And the names of the regions which define them
        self.pss_region_dict = None
        self.pss_saved_solids_names = []
        #The top of the task pane is where users define/select PS regions
        #Icons to add/remove/duplicate point sources conditions are given at the top
        #Clicking the 'add' and 'duplicate' buttons triggers a popup window where the user must select
        #a region to apply the point source.
        ui.toolbutton_add.clicked.connect(self.pss_show_regions_popup)
        ui.toolbutton_delete.clicked.connect(self.pss_delete_regions)
        ui.toolbutton_delete.setEnabled(False) # Need a selection

        ui.tablewidget_regions.itemSelectionChanged.connect(self.handle_pss_region_selection)

        self.pss_current_tab = FLUID_TAB # If fluid is disabled, we will switch
        self.pss_current_solid = self.P = None
        ui.pushbutton_fluid.pressed.connect(lambda: self.pss_change_tab(FLUID_TAB,None))

        # Trim width of "Fluid" button, like we do for
        # dynamically-created "Solid #" buttons
        b =  ui.pushbutton_fluid
        w = b.fontMetrics().boundingRect(b.text()).width() + 20
        b.setMaximumWidth(w)


    def pss_show_regions_popup(self):
        # Users cannot select inapplicable regions.
        # PS regions can be points, planes, or volumes (not STLs)
        # No region can define more than one point source.
        ui = self.ui.point_sources
        rp = self.regions_popup
        rp.clear()
        for (name,data) in self.pss_region_dict.items():
            shape = data.get('type', '---')
            # Assume available if unmarked
            available = (data.get('available', True)
                         and (shape in ('point', 'box')
                              or 'plane' in shape))
            row = (name, shape, available)
            rp.add_row(row)
        rp.reset_signals()
        rp.save.connect(self.pss_add_regions)
        rp.cancel.connect(self.pss_cancel_add)
        for item in (ui.tablewidget_regions,
                     ui.bottom_frame,
                     ui.toolbutton_add,
                     ui.toolbutton_delete):
            item.setEnabled(False)
        rp.popup('Select region(s) for point source')


    def pss_cancel_add(self):
        ui = self.ui.point_sources

        for item in (ui.toolbutton_add,
                     ui.tablewidget_regions):
            item.setEnabled(True)

        if get_selected_row(ui.tablewidget_regions) is not None:
            for item in (ui.bottom_frame,
                         ui.toolbutton_delete):
                item.setEnabled(True)


    def pss_add_regions(self):
        # Interactively add regions to define PSs
        ui = self.ui.point_sources
        rp = self.regions_popup
        self.pss_cancel_add() # Reenable input widgets
        selections = rp.get_selection_list()
        if not selections:
            return
        self.pss_add_regions_1(selections, indices=None, autoselect=True)
        self.pss_setup_current_tab() # Update the widgets


    def pss_add_regions_1(self, selections, indices=None, autoselect=False):
        # Used by both interactive and load-time add-region handlers
        ui = self.ui.point_sources

        if self.pss_region_dict is None:
            self.pss_region_dict = self.ui.regions.get_region_dict()

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
                idx = self.pss_find_index()
                indices[i] = idx
            self.pss[idx] = {'region': region_name}
            region_data = self.pss_region_dict.get(region_name)
            if region_data is None: # ?
                self.warn("no data for region %s" % region_name)
                continue
            self.pss_set_region_keys(region_name, idx, region_data)
            self.pss_region_dict[region_name]['available'] = False # Mark as in-use
        item.setData(UserRole, (tuple(indices), tuple(selections)))
        tw.setItem(nrows, 0, item)

        self.fixup_pss_table(tw)

        if autoselect:
            tw.setCurrentCell(nrows, 0)

        # Issues/513
        for i in indices:
            self.pss_set_default_keys(i)


    def pss_set_default_keys(self, PS):
        #Fluid
        #-  Define mass flowrate:
        #-  Sets keyword PS_MASSFLOW_G(#)
        #-  DEFAULT 0.0
        key = 'ps_massflow_g'
        default = 0.0
        self.set_keyword_default(key, default, args=[PS])
        mmax = self.project.get_value('mmax', default=0)
        nmax_g = self.project.get_value('nmax_g', default=0)
        nmax_s = [None] + [self.project.get_value('nmax_s', default=0, args=[i])
                           for i in range(1,1+mmax)]
        energy_eq = self.project.get_value('energy_eq', default=True)
        species_eq = [self.project.get_value('species_eq', default=True, args=[i])
                      for i in range(1+mmax)] # including index 0 = fluid
        solids_model = [None] + [self.project.get_value('solids_model', args=[i])
                                 for i in range(1,1+mmax)]

        #-  Define temperature
        #-  Specification only available when solving energy equations
        #-  DEFAULT value 293.15
        #-  Sets keyword PS_T_G(#)
        if energy_eq:
            key = 'ps_t_g'
            default = 293.15
            self.set_keyword_default(key, default, args=[PS])

        #-  Select species and set mass fractions (table format)
        #-  Specification only available when solving species equations
        #-  DEFAULT value 1.0 of last defined species
        #-  Sets keyword PS_X_G(#,#)
        #-  Error check: if specified, mass fractions must sum to 1.0
        if species_eq[0]:
            key = 'ps_x_g'
            for i in range(1, 1+nmax_g):
                default = 0.0
                self.set_keyword_default(key, default, args=[PS,i])
            total = sum(safe_float(self.project.get_value(key, default=0, args=[PS,i]))
                        for i in range(1, nmax_g)) # All but last
            if total == 0 and nmax_g > 0:  # Set last species to 1, only if all other are 0
                self.update_keyword(key, 1.0, args=[PS, nmax_g])

        #-  Define X/Y/Z-axial velocity:
        #-  Specification always available
        #-  DEFAULT value 0.0
        #-  Sets keyword PS_U/V/W_G(#)
        default = 0.0
        for c in 'uvw':
            key = 'ps_%s_g' % c
            self.set_keyword_default(key, default, args=[PS])

        #Solids-#
        #*At this time, only TFM solids can be defined with point sources. At some point in the future, this could be extended to PIC solids, but never DEM.*

        for P in range(1, 1+mmax):
            if solids_model[P] != 'TFM':
                continue
            #-  Define mass flowrate:
            #-  Select mass inflow specification type:
            #-  Sets keyword PS_MASSFLOW_S(#,#)
            #-  DEFAULT 0.0
            key = 'ps_massflow_s'
            default = 0.0
            self.set_keyword_default(key, default, args=[PS,P])

            #-  Define temperature
            #-  Specification only available when solving energy equations
            #-  DEFAULT value 293.15
            #-  Sets keyword PS_T_S(#,#)
            if energy_eq:
                key = 'ps_t_s'
                default = 293.15
                self.set_keyword_default(key, default, args=[PS,P])

            #-  Select species and set mass fractions (table format)
            #-  Specification only available when solving species equations
            #-  DEFAULT value 1.0 of last defined species
            #-  Sets keyword PS_X_S(#,#,#)
            #-  Error check: if specified, mass fractions must sum to 1.0
            if species_eq[P]:
                key = 'ps_x_s'
                N = nmax_s[P]
                for i in range(1, 1+N):
                    default = 0.0
                    self.set_keyword_default(key, default, args=[PS,P,i])
                total = sum(safe_float(self.project.get_value(key, default=0, args=[PS,i]))
                            for i in range(1, N)) # All but last
                if total == 0 and N > 0:  # Set last species to 1, only if all other are 0
                    self.update_keyword(key, 1.0, args=[PS,P,N])

            #-  Define X/Y/Z-axial velocity:
            #-  Specification always available
            #-  DEFAULT value 0.0
            #-  Sets keyword PS_{UVW}_S(#,#)
            default = 0.0
            for c in 'uvw':
                key = 'ps_%s_s' % c
                self.set_keyword_default(key, default, args=[PS,P])

        #Scalar Tab
        #*Point sources have not been implemented for general scalars.*


    def pss_find_index(self):
        # Always add new PS at end
        return 1 if not self.pss else 1 + max(self.pss)


    def pss_delete_regions(self):
        ui = self.ui.point_sources
        tw = ui.tablewidget_regions
        row = get_selected_row(tw)
        if row is None: # No selection
            return

        # Unset keywords
        kwlist = list(self.project.keywordItems())
        for kw in kwlist:
            key, args = kw.key, kw.args
            if key.startswith('ps_') and args and args[0] in self.pss_current_indices:
                self.unset_keyword(key, args=args)

        for r in self.pss_current_regions:
            if r in self.pss_region_dict:
                self.pss_region_dict[r]['available'] = True

        for i in self.pss_current_indices:
            del self.pss[i]

        self.pss_current_regions = []
        self.pss_current_indices = []

        tw.removeRow(row)
        self.fixup_pss_table(tw)
        self.pss_setup_current_tab()
        self.update_nav_tree()


    def pss_delete_solids_phase(self, phase_index):
        """adjust pss_current_solid when solids phase deleted"""
        if (self.pss_current_solid is not None and
            self.pss_current_solid >= phase_index):
            self.pss_current_solid -= 1
            if self.pss_current_solid == 0:
                self.pss_current_solid = None


    def handle_pss_region_selection(self):
        ui = self.ui.point_sources
        table = ui.tablewidget_regions
        row = get_selected_row(table)
        if row is None:
            indices = []
            regions = []
        else:
            (indices, regions) = table.item(row,0).data(UserRole)
        self.pss_current_indices, self.pss_current_regions = indices, regions
        enabled = (row is not None)
        for item in (ui.toolbutton_delete,
                     ui.bottom_frame):
            item.setEnabled(enabled)
        if not enabled:
            # Clear
            for widget in widget_iter(ui.bottom_frame):
                if isinstance(widget, LineEdit):
                    widget.setText('')
            return
        self.setup_pss() # reinitialize all widgets
        ui.scrollarea_detail.ensureVisible(0, 0)  # scroll to top


    def fixup_pss_table(self, tw, stretch_column=0):
        ui = self.ui.point_sources
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
            tw.setMaximumHeight(height) # Works for tablewidget inside groupbox
            tw.setMinimumHeight(height) #? needed? should we allow scrollbar?
        tw.updateGeometry() #? needed?


    def pss_update_enabled(self):
        if self.pss:
            # Never disable if there are PSs defined
            disabled = False
        else:
            # If there are no solids, (no scalar equations), and the fluid solver is disabled,
            # then we have no input tabs on the PSs pane, so disable it completely
            # PS regions can be points, planes, or volumes (not STLs)
            regions = self.ui.regions.get_region_dict()
            nregions = sum(1 for (name, r) in regions.items()
                           if (r.get('type')  in ('point', 'box')
                                or 'plane' in r.get('type','---')))
            #At this time, only TFM solids can be defined with point sources.
            tfm_solids = [s for (i,s) in enumerate(self.solids,1)
                          if self.project.get_value('solids_model', args=[i])=='TFM']
            disabled = (nregions==0
                        or (self.fluid_solver_disabled
                            and len(tfm_solids)==0))
        self.find_navigation_tree_item("Point Sources").setDisabled(disabled)


    def pss_change_tab(self, tab, solid):
        ui = self.ui.point_sources
        index = (0 if tab==FLUID_TAB
                 #else len(self.solids)+1 if tab==SCALAR_TAB
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
            if solid == self.pss_current_solid:
                return # Really nothing to do

            if solid > (self.pss_current_solid or 0):
                dummy_label = ui.label_dummy_solids_L
                dummy_tab = SOLIDS_TAB_DUMMY_L
            else:
                dummy_label = ui.label_dummy_solids_R
                dummy_tab = SOLIDS_TAB_DUMMY_R

            p = QPicture()
            ui.page_solids.render(p, flags=QWidget.DrawChildren)  #avoid rendering bg
            dummy_label.setPicture(p)
            ui.stackedwidget.setCurrentIndex(dummy_tab)

        self.pss_current_tab = tab
        self.pss_current_solid = self.P = solid if tab==SOLIDS_TAB else None
        self.pss_setup_current_tab()

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


    def pss_check_region_in_use(self, name):
        return any(data.get('region')==name for data in self.pss.values())


    def pss_update_region(self, name, data):
        for (i,ps) in self.pss.items():
            if ps.get('region') == name:
                self.pss_set_region_keys(name, i, data)


    def pss_set_region_keys(self, name, idx, data):
        # Update the keys which define the region the PS applies to
        no_k = self.project.get_value('no_k')
        for (key, val) in zip(('x_w', 'y_s', 'z_b',
                               'x_e', 'y_n', 'z_t'),
                              data['from']+data['to']):
            # ps_z_t and ps_z_b keywords should not be added when no_k=True
            if no_k and key in ('z_t', 'z_b'):
                continue
            self.update_keyword('ps_'+key, val, args=[idx])


    def pss_change_region_name(self, old_name, new_name):
        ui = self.ui.point_sources
        for (key, val) in self.pss.items():
            if val.get('region') == old_name:
                self.pss[key]['region'] = new_name
                tw = ui.tablewidget_regions
                for i in range(tw.rowCount()):
                    data = tw.item(i,0).data(UserRole)
                    indices, names = data
                    if key in indices:
                        item = tw.item(i,0)
                        names = [new_name if n==old_name else n for n in names]
                        item.setData(UserRole, (tuple(indices), tuple(names)))
                        item.setText('+'.join(names))
                        break
                break


    def reset_pss(self):
        self.pss.clear()
        self.pss_current_indices = []
        self.pss_current_regions = []
        self.pss_region_dict = None
        self.pss_current_solid = self.P = None
        ui = self.ui.point_sources
        ui.tablewidget_regions.clearContents()
        ui.tablewidget_regions.setRowCount(0)
        # anything else to do here?
        # TODO remove dynamically created input widgets, although this should
        #  get handled next time we call 'setup'

    def ps_regions_to_str(self):
        ui = self.ui.point_sources
        tw = ui.tablewidget_regions
        data = [tw.item(i,0).data(UserRole)
                for i in range(tw.rowCount())]
        return JSONEncoder().encode(data)


    def pss_regions_from_str(self, s):
        if not s:
            return
        data = JSONDecoder().decode(s)
        for (indices, regions) in data:
            self.pss_add_regions_1(regions, indices, autoselect=False)


    def setup_pss(self):
        ui = self.ui.point_sources

        # Grab a fresh copy, may have been updated
        self.pss_region_dict = self.ui.regions.get_region_dict()

        # Mark regions which are in use (this gets reset each time we get here)
        for (i, data) in self.pss.items():
            region = data['region']
            if region in self.pss_region_dict:
                self.pss_region_dict[region]['available'] = False

        self.fixup_pss_table(ui.tablewidget_regions)
        row = get_selected_row(ui.tablewidget_regions)
        # Autoselect if only 1 row
        if row is None and ui.tablewidget_regions.rowCount() == 1:
            row = 0
            ui.tablewidget_regions.setCurrentCell(row, 0)
        enabled = (row is not None)
        for item in (ui.toolbutton_delete,
                     ui.bottom_frame):
            item.setEnabled(enabled)

        #Tabs group point source parameters for phases. Tabs are unavailable if no input
        #is required from the user.
        #    Fluid tab - Unavailable if the fluid phase was disabled.
        b = ui.pushbutton_fluid
        b.setText(self.fluid_phase_name)
        b.setEnabled(not self.fluid_solver_disabled)
        font = b.font()
        font.setBold(self.pss_current_tab == 0)
        b.setFont(font)

        # Each solid phase will have its own tab. The tab name should be the name of the solid
        solids_names = list(self.solids.keys())
        if self.pss_saved_solids_names != solids_names:
            # Clear out the old ones
            n_cols = ui.tab_layout.columnCount()
            for i in range(n_cols-1, 0, -1):
                item = ui.tab_layout.itemAtPosition(0, i)
                if not item:
                    continue
                widget = item.widget()
                if not widget:
                    continue
                if widget == ui.pushbutton_fluid:
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
                font.setBold(self.pss_current_tab==SOLIDS_TAB and i==self.pss_current_solid)
                b.setFont(font)
                ui.tab_layout.addWidget(b, 0, i)
                b.pressed.connect(lambda i=i: self.pss_change_tab(SOLIDS_TAB, i))

        for (i, solid_name) in enumerate(self.solids.keys(),1):
            model = self.project.get_value('solids_model', args=[i])
            #At this time, only TFM solids can be defined with point sources.
            #At some point in the future, this could be extended to PIC solids, but never DEM.
            b = ui.tab_layout.itemAtPosition(0, i).widget()
            if model == 'TFM':
                b.setEnabled(True)
                b.setToolTip(None)
            else:
                b.setEnabled(False)
                b.setToolTip("Only TFM solids can be defined as point sources""")

        self.pss_saved_solids_names = solids_names
        self.P = self.pss_current_solid

        # Don't stay on a disabled tab
        index = self.pss_tab_to_index(self.pss_current_tab, self.pss_current_solid)
        item = None if index is None else ui.tab_layout.itemAtPosition(0, index)
        b = item.widget() if item else None
        if ui.isEnabled() and not (b and b.isEnabled()):
            self.pss_change_tab(*self.pss_find_valid_tab())
        else:
            self.pss_setup_current_tab()

        # make sure underline is in the right place, as # of solids may
        # have changed (lifted from animate_stacked_widget, which we
        # don't want to call here)
        tab = self.pss_current_tab
        line_to = self.pss_tab_to_index(tab, self.pss_current_solid)
        line = ui.tab_underline
        btn_layout = ui.tab_layout
        if line_to is not None:
            btn_layout.addItem(btn_layout.takeAt(
                btn_layout.indexOf(line)), 1, line_to)


    def pss_tab_to_index(self, tab, solid):
        return (0 if tab==FLUID_TAB
                else solid)

    def pss_find_valid_tab(self):
        if not self.fluid_solver_disabled:
            return (FLUID_TAB, None)
        else:
            mmax = self.project.get_value('mmax', default=0)
            solids_model = [None] + [self.project.get_value('solids_model', args=[i])
                                     for i in range(1,1+mmax)]
            if 'TFM' in solids_model:
                return (SOLIDS_TAB, solids_model.index('TFM'))
            else:
                self.error("Point sources:  all tabs disabled!")
        return (FLUID_TAB, None) # What else to do?


    def pss_setup_current_tab(self):
        if self.pss_current_tab == FLUID_TAB:
            self.setup_pss_fluid_tab()
        elif self.pss_current_tab == SOLIDS_TAB:
            self.setup_pss_solids_tab(self.pss_current_solid)


    def update_pss_fluid_mass_fraction_table(self):
        ui = self.ui.point_sources
        table = ui.tablewidget_fluid_mass_fraction
        table.clearContents()
        table.setRowCount(0)
        if not (self.fluid_species and self.pss_current_indices):
            self.fixup_pss_table(table)
            ui.groupbox_fluid_composition.setEnabled(False)
            return
        ui.groupbox_fluid_composition.setEnabled(True)
        PS0 = self.pss_current_indices[0]
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
            key = 'ps_x_g'
            le.key = key
            le.args = [self.pss_current_indices, row+1]
            self.add_tooltip(le, key)
            val = self.project.get_value(key, args=[PS0, row+1], default=None)
            if val is not None:
                le.updateValue(key, val)
            le.value_updated.connect(self.handle_pss_fluid_mass_fraction)
            table.setCellWidget(row, 1, le)
        if species:
            table.setItem(nrows-1, 0, make_item("Total"))
            table.setItem(nrows-1, 1, make_item(''))
            item = table.item(nrows-1, 0)
            font = item.font()
            font.setBold(True)
            item.setFont(font)
            self.update_pss_fluid_mass_fraction_total()
        self.fixup_pss_table(table)


    def handle_pss_fluid_mass_fraction(self, widget, value_dict, args):
        ui = self.ui.point_sources
        key = 'ps_x_g'
        val = value_dict[key]
        widget.updateValue(key, val)
        if val == '':
            self.unset_keyword(key, args=args)
        else:
            self.update_keyword(key, val, args=args)

        # DEFAULT - last defined species has mass fraction of 1.0
        # (only enforce this if no mass fractions are set)
        # See also pss_set_default_keys
        if self.fluid_species:
            N = len(self.fluid_species)
            for PS in args[0]:
                total = sum(safe_float(self.project.get_value(key, default=0, args=[PS,i]))
                            for i in range(1, 1+N))
                for i in range(1, 1+N):
                    default = float(i==N) if total==0.0 else 0.0
                    self.set_keyword_default(key, default, args=[PS, i])
        self.update_pss_fluid_mass_fraction_table()


    def update_pss_fluid_mass_fraction_total(self):
        if not self.pss_current_indices:
            return
        if not self.fluid_species:
            return
        PS0 = self.pss_current_indices[0]
        ui = self.ui.point_sources
        key = 'ps_x_g'
        table = ui.tablewidget_fluid_mass_fraction
        if table.rowCount() == 0:
            return
        N = len(self.fluid_species)
        total = sum(safe_float(self.project.get_value(key, default=0.0, args=[PS0,i]))
                    for i in range(1, 1+N))
        total = round(total, 6)
        item = table.item(table.rowCount()-1, 1)
        font = item.font()
        font.setBold(True)
        item.setFont(font)
        item.setText(str(total))
        if total != 1.0:
            item.setForeground(Qt.red)
            #We should warn, but this creates too many popups while PS is being set up
            #self.warning("Mass fractions sum to %s, must be 1.0" % total, popup=True)
        elif ui.isEnabled():
            item.setForeground(Qt.black) # FIXME looks wrong when greyed-out

    def update_pss_solids_mass_fraction_table(self):
        ui = self.ui.point_sources
        table = ui.tablewidget_solids_mass_fraction
        table.clearContents()
        table.setRowCount(0)
        P = self.pss_current_solid
        if not (P and self.solids_species.get(P) and self.pss_current_indices):
            self.fixup_pss_table(table)
            table.setEnabled(False)
            ui.groupbox_solids_composition.setEnabled(False)
            return
        ui.groupbox_solids_composition.setEnabled(True)
        table.setEnabled(True)
        PS0 = self.pss_current_indices[0]
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
            key = 'ps_x_s'
            le.key = key
            le.args = [self.pss_current_indices, P, row+1]
            self.add_tooltip(le, key)
            val = self.project.get_value(key, args=[PS0, P, row+1], default=None)
            if val is not None:
                le.updateValue(key, val)
            le.value_updated.connect(self.handle_pss_solids_mass_fraction)
            table.setCellWidget(row, 1, le)
        if species:
            table.setItem(nrows-1, 0, make_item("Total"))
            table.setItem(nrows-1, 1, make_item(''))
            item = table.item(nrows-1, 0)
            font = item.font()
            font.setBold(True)
            item.setFont(font)
            self.update_pss_solids_mass_fraction_total()
        self.fixup_pss_table(table)


    def handle_pss_solids_mass_fraction(self, widget, value_dict, args):
        ui = self.ui.point_sources
        key = 'ps_x_s'
        val = value_dict[key]
        widget.updateValue(key, val)
        if val == '':
            self.unset_keyword(key, args=args)
        else:
            self.update_keyword(key, val, args=args)

        # DEFAULT - last defined species has mass fraction of 1.0
        # See also pss_set_default_keys
        PS = args[0]
        P = args[1]
        species = self.solids_species[P]
        if species:
            N = len(species)
            for PS in self.pss_current_indices:
                total = sum(safe_float(self.project.get_value(key, default=0, args=[PS,P,i]))
                            for i in range(1, 1+N))
                for i in range(1, 1+N):
                    default = float(i==N) if total==0 else 0.0
                    self.set_keyword_default(key, default, args=[PS, P, i])

        self.update_pss_solids_mass_fraction_table()


    def update_pss_solids_mass_fraction_total(self):
        if not self.pss_current_indices:
            return
        PS0 = self.pss_current_indices[0]
        P = self.pss_current_solid
        if P is None:
            return
        species = self.solids_species.get(P)
        if not P:
            return
        ui = self.ui.point_sources
        key = 'ps_x_s'
        table = ui.tablewidget_solids_mass_fraction
        if table.rowCount() == 0:
            return
        total = sum(safe_float(self.project.get_value(key, default=0.0, args=[PS0,P,i]))
                    for i in range(1,len(species)+1))
        total = round(total, 6)
        item = table.item(table.rowCount()-1, 1)
        font = item.font()
        font.setBold(True)
        item.setFont(font)
        item.setText(str(total))
        if total != 1.0:
            item.setForeground(Qt.red)
            #We should warn, but this creates too many popups while PS is being set up
            #self.warning("Mass fractions sum to %s, must be 1.0" % total, popup=True)
        elif ui.isEnabled():
            item.setForeground(Qt.black) # FIXME looks wrong when greyed-out

    def pss_extract_regions(self):
        if self.pss:
            # We assume that PS regions have been initialized correctly
            # from mfix_gui_comments.
            # TODO: verify that there is an PS region for each PS
            return

        if self.pss_region_dict is None:
            self.pss_region_dict = self.ui.regions.get_region_dict()

        # TODO: if we wanted to be fancy, we could find regions where
        # PS values matched, and merge into a new PS region.  That
        # is only needed for projects created outside the GUI (otherwise
        # we have already stored the PS regions).  Also would be nice
        # to offer a way to split compound regions.
        for ps in self.project.pss:

            d = ps.keyword_dict
            extent = [d.get('ps_'+k,None) for k in ('x_w', 'y_s', 'z_b',
                                                    'x_e', 'y_n', 'z_t')]
            extent = [0 if x is None else x.value for x in extent]
            #if any (x is None for x in extent):
            #    self.warn("point source %s: invalid extents %s" %
            #               (ps.ind, extent))
            #    continue
            for (region_name, data) in self.pss_region_dict.items():
                ext2 = [0 if x is None else x for x in
                        (data.get('from',[]) + data.get('to',[]))]
                if ext2 == extent:
                    if data.get('available', True):
                        self.pss_add_regions_1([region_name], indices=[ps.ind], autoselect=False)
                        break
            else:
                self.warn("point source %s: could not match defined region %s" %
                          (ps.ind, extent))
                kwlist = list(self.project.keywordItems())
                for kw in kwlist:
                    key, args = kw.key, kw.args
                    if key.startswith('ps_') and args and args[0]==ps.ind:
                        self.unset_keyword(key, args=args)


    def setup_pss_fluid_tab(self):
        #Fluid (tab)
        if self.fluid_solver_disabled:
            # we shouldn't be on this tab!
            return
        ui = self.ui.point_sources
        tw = ui.tablewidget_fluid_mass_fraction
        enabled = bool(self.fluid_species)
        if not enabled:
            tw.clearContents()
            tw.setRowCount(0)
        self.fixup_pss_table(tw)
        tw.setEnabled(enabled)

        if not self.pss_current_indices:
            # Nothing selected.  What can we do? (Clear out all lineedits?)
            return

        PS0 = self.pss_current_indices[0]

        def get_widget(key):
            for pat in ('lineedit_keyword_%s_args_PS',
                        'lineedit_%s_args_PS'):
                widget = getattr(ui, pat % key)
                if widget:
                    return widget
            self.error('no widget for key %s' % key)

        def setup_key_widget(key, default=None, enabled=True):
            for pat in ('label_%s', 'label_%s_units',
                        'lineedit_keyword_%s_args_PS'):
                name = pat%key
                item = getattr(ui, name, None)
                if item:
                    item.setEnabled(enabled)
                    if isinstance(item, LineEdit) and item.dtype in (int, float):
                        item.allow_parameters = True
            if not enabled:
                get_widget(key).setText('') #?
                return
            args = mkargs(key, ps=PS0)
            val = self.project.get_value(key, args=args)
            if val is None:
                val = default
            for PS in self.pss_current_indices:
                self.update_keyword(key, val, args=mkargs(key, ps=PS))
            get_widget(key).updateValue(key, val, args=args)


        #    Define mass flowrate:
        # Sets keyword PS_MASSFLOW_G(#)
        # DEFAULT 0.0
        enabled = True
        key = 'ps_massflow_g'
        default = 0.0
        setup_key_widget(key, default, enabled)

        #    Define temperature
        # Specification only available when solving energy equations
        # DEFAULT value 293.15
        # Sets keyword PS_T_G(#)
        energy_eq = self.project.get_value('energy_eq', default=True)
        enabled = bool(energy_eq)
        default = 293.15 if enabled else None
        key = 'ps_t_g'
        setup_key_widget(key, default, enabled)

        # Define X-axial velocity:
        # Specification always availavle
        # DEFAULT value 0.0
        # Sets keyword PS_U_G(#)
        # Define Y-axial velocity:
        # Specification always availavle
        # DEFAULT value 0.0
        # Sets keyword PS_V_G(#)
        # Define Z-axial velocity:
        # Specification always availavle
        # DEFAULT value 0.0
        # Sets keyword PS_W_G(#)
        enabled = True
        default = 0.0
        for c in 'uvw':
            key = 'ps_%s_g' % c
            setup_key_widget(key, default, enabled)

        #    Select species and set mass fractions (table format)
        # Specification only available when solving species equations
        # DEFAULT value 1.0 of last defined species
        # Sets keyword PS_X_G(#,#)
        # Error check: if specified, mass fractions must sum to 1.0
        species_eq = self.project.get_value('species_eq', default=True, args=[0])
        enabled = bool(species_eq)
        comp = ui.groupbox_fluid_composition
        if enabled:
            self.update_pss_fluid_mass_fraction_table()
        else:
            #comp.hide() #?
            comp.setEnabled(False)


    def setup_pss_solids_tab(self, P):
        #Solids-# (tab)
        # Note, solids phases are numbered 1-N
        ui = self.ui.point_sources
        tw = ui.tablewidget_solids_mass_fraction
        self.pss_current_solid = self.P = P
        if P is None: # Shouldn't be here
            return
        enabled = bool(self.solids_species.get(P))
        if not enabled:
            tw.clearContents()
            tw.setRowCount(0)
        self.fixup_pss_table(tw)
        tw.setEnabled(enabled)

        if not self.pss_current_indices: # No region selected
            # TODO clear all widgets (?)
            return


        PS0 = self.pss_current_indices[0]

        # Generic!
        def get_widget(key):
            for pat in ('lineedit_keyword_%s_args_PS_P',
                        'lineedit_keyword_%s_args_PS',
                        'lineedit_%s_args_PS_P',
                        'lineedit_%s_args_PS'):
                widget = getattr(ui, pat % key, None)
                if widget:
                    return widget
            self.error('no widget for key %s' % key)

        def setup_key_widget(key, default=None, enabled=True):
            for pat in ('label_%s', 'label_%s_units',
                         'lineedit_keyword_%s_args_PS_P',
                         'lineedit_keyword_%s_args_PS',
                         'lineedit_%s_args_PS',
                         'lineedit_%s_args_PS'):
                name = pat%key
                item = getattr(ui, name, None)
                if item:
                    item.setEnabled(enabled)
            if not enabled:
                get_widget(key).setText('') #?
                return
            args = mkargs(key, ps=PS0, phase=P)
            val = self.project.get_value(key, args=args)
            if val is None:
                val = default
            for PS in self.pss_current_indices:
                self.update_keyword(key, val, args=mkargs(key, ps=PS, phase=P))
            get_widget(key).updateValue(key, val, args=args)

        #Define mass flowrate:
        # Sets keyword PS_MASSFLOW_S(#,#)
        # DEFAULT 0.0
        enabled = True
        key = 'ps_massflow_s'
        default = 0.0
        setup_key_widget(key, default, enabled)

        #Define temperature
        # Specification only available when solving energy equations
        # DEFAULT value 293.15
        # Sets keyword PS_T_S(#,#)
        energy_eq = self.project.get_value('energy_eq', default=True)
        enabled = bool(energy_eq)
        default = 293.15 if enabled else None
        key = 'ps_t_s'
        setup_key_widget(key, default, enabled)

        #Define X-axial velocity:
        # Specification always available
        # DEFAULT value 0.0
        # Sets keyword PS_U_S(#,#)

        #Define Y-axial velocity:
        # Specification always available
        # DEFAULT value 0.0
        # Sets keyword PS_V_S(#,#)

        #Define Z-axial velocity:
        # Specification always available
        # DEFAULT value 0.0
        # Sets keyword PS_W_S(#,#)
        enabled = True
        default = 0.0
        for c in 'uvw':
            key = 'ps_%s_s' % c
            setup_key_widget(key, default, enabled)

        #Select species and set mass fractions (table format)
        # Specification only available when solving species equations
        # DEFAULT value 1.0 of last defined species
        # Sets keyword PS_X_S(#,#,#)
        # Error check: if specified, mass fractions must sum to 1.0

        species_eq = self.project.get_value('species_eq', default=True, args=[P])
        enabled = bool(species_eq)
        comp = ui.groupbox_solids_composition
        if enabled:
            self.update_pss_solids_mass_fraction_table()
        else:
            comp.setEnabled(False)
