# -*- coding: utf-8 -*-

"""Output Task Pane Window"""

# We don't need extended JSON here
from json import JSONDecoder, JSONEncoder

from qtpy.QtCore import Qt

from qtpy.QtWidgets import (QTableWidgetItem, QHeaderView, QLabel,
                            QPushButton, QWidget)

from qtpy.QtGui import QPicture

UserRole = Qt.UserRole

from mfixgui.constants import *

from mfixgui.tools.qt import (get_combobox_item, get_selected_row,
                              set_item_enabled, set_item_noedit,
                              sub_icon_height, widget_iter)

from mfixgui.widgets.base import (BaseWidget, LineEdit, CheckBox)

#Top tabset
BASIC_TAB, VTK_TAB, SPX_TAB, NETCDF_TAB = range(4)
PAGE_CELL, PAGE_PARTICLE = (0, 1)

#Columns in VTK output region table
COLUMN_REGION, COLUMN_FILENAME, COLUMN_TYPE, COLUMN_ID = range(4)

#Bottom subpage tabset, for VTK cell data
(FLUID_TAB, SOLIDS_TAB_DUMMY_L, SOLIDS_TAB, SOLIDS_TAB_DUMMY_R,
 SCALAR_TAB, REACTIONS_TAB, OTHER_TAB) = range(7) # bottom tabset

MAX_SP = 11

#model/init_namelist.f:      bWrite_netCDF(:20) = .FALSE.
MAX_BWRITE_NETCDF = 20

VTK_DATA_TYPES = ['C', 'P']

class Output(object):
    #Output Task Pane Window:
    #The output input is split into tabs.

    def init_output(self):
        ui = self.ui.output

        self.output_current_tab = BASIC_TAB
        self.outputs = {} # key: index.  value: data dictionary for VTK output region
        # Note, SingleSelection enabled so 'indices' and 'regions' are really just 'index' and 'region'
        self.vtk_current_indices = [] # List of VTK output indices
        self.vtk_current_regions = [] # And the names of the regions which define them
        self.vtk_current_solid = self.P = None
        self.output_region_dict = None
        # connect tab buttons
        self.output_pushbuttons = (ui.pushbutton_basic,
                                   ui.pushbutton_vtk,
                                   ui.pushbutton_spx,
                                   ui.pushbutton_netcdf)
        for (i, btn) in enumerate(self.output_pushbuttons):
            btn.pressed.connect(lambda i=i, btn=btn: self.output_change_tab(i, btn))

        self.init_output_basic_tab()
        self.init_output_vtk_tab()
        self.init_output_spx_tab()
        self.init_output_netcdf_tab()

        cb = ui.combobox_vtk_select_mode
        key = 'vtk_select_mode'
        for (i, value) in enumerate('CPI'):
            item = get_combobox_item(cb, i)
            self.add_tooltip(item, key, value=value)
        cb.activated.connect(self.handle_combobox_vtk_select_mode)
        # Make the table update when user changed VTK_FILEBASE
        le = ui.lineedit_keyword_vtk_filebase_args_VTK
        le.post_update = self.setup_output_vtk_tab

    def handle_combobox_vtk_select_mode(self, index):
        ui = self.ui.output
        key = 'vtk_select_mode'
        val = 'CPI'[index]
        for v in self.vtk_current_indices:
            self.update_keyword(key, val, args=[v])
        cb = ui.combobox_vtk_select_mode
        cb.setToolTip(get_combobox_item(cb,index).toolTip())

    # Output sub-pane navigation
    def output_change_tab(self, tabnum, to_btn):
        ui = self.ui.output
        self.output_current_tab = tabnum
        self.animate_stacked_widget(
            ui.stackedwidget_output,
            ui.stackedwidget_output.currentIndex(),
            tabnum,
            direction='horizontal',
            line=ui.line_output,
            to_btn=to_btn,
            btn_layout=ui.gridlayout_tab_btns)
        self.setup_output_tab(tabnum)
        for btn in self.output_pushbuttons:
            btn.setChecked(btn == to_btn)
            font = btn.font()
            font.setBold(btn == to_btn)
            btn.setFont(font)


    def init_output_basic_tab(self):
        ui = self.ui.output

        for item in widget_iter(ui.page_basic):
            if isinstance(item, BaseWidget):
                item.post_update = self.setup_output_basic_tab

        gb = ui.groupbox_write_vtk_files
        key = 'write_vtk_files'
        gb.clicked.connect(self.output_enable_vtk)
        self.add_tooltip(gb, key)

        cb = ui.checkbox_spx
        cb.clicked.connect(self.output_enable_spx)

        cb = ui.checkbox_netcdf
        cb.clicked.connect(self.output_enable_netcdf)


    def init_output_vtk_tab(self):
        #VTK (tab)
        ui = self.ui.output

        self.output_saved_fluid_species_names = []
        self.output_saved_solids_names = []
        self.output_saved_solids_species_names = []

        # Dynamically created items
        ui.dynamic_widgets = {}

        # Set up subtabs
        self.output_pushbuttons_bottom = (ui.pushbutton_fluid,
                                          #ui.pushbutton_solid,
                                          ui.pushbutton_scalar,
                                          ui.pushbutton_reactions,
                                          ui.pushbutton_other)

        self.output_current_subtab = FLUID_TAB
        self.vtk_current_solid = self.P = None
        ui.pushbutton_fluid.pressed.connect(lambda: self.output_change_subtab(FLUID_TAB, None))
        ui.pushbutton_scalar.pressed.connect(lambda: self.output_change_subtab(SCALAR_TAB, None))
        ui.pushbutton_reactions.pressed.connect(lambda: self.output_change_subtab(REACTIONS_TAB, None))
        ui.pushbutton_other.pressed.connect(lambda: self.output_change_subtab(OTHER_TAB, None))

        # Trim width of "Fluid" and "Scalar" buttons, like we do for
        # dynamically-created "Solid #" buttons
        for b in self.output_pushbuttons_bottom:
            w = b.fontMetrics().boundingRect(b.text()).width() + 20
            b.setMaximumWidth(w)

        # Icons and table similar to IC/BC/PS/IS for adding VTK regions. This
        # section requires WRITE_VTK_FILES = .TRUE. Icons to
        # add/remove/duplicate regions are given at the top Clicking the 'add'
        # and 'duplicate' buttons triggers a popup window where the user must
        # select a VTK region.

        ui.toolbutton_add.clicked.connect(self.output_show_regions_popup)
        ui.toolbutton_delete.clicked.connect(self.output_delete_regions)
        ui.toolbutton_delete.setEnabled(False) # Need a selection
        ui.tablewidget_regions.itemSelectionChanged.connect(self.handle_output_region_selection)

        ui.stackedwidget_cell_particle.setCurrentIndex(PAGE_CELL)
        # no per-subpage init, yet, so there's some per-page init here
        ui.checkbox_keyword_vtk_vorticity_args_VTK.post_update = self.set_vtk_lambda_2

        # Show cell data groupboxes without a title (title is separate
        # label_cell, displayed outside tab set)

        # Show groupbox without title  (title is empty string)
        height = ui.checkbox_keyword_vtk_ep_g_args_VTK.sizeHint().height()
        tweak = 3
        ui.subpage_cell.setStyleSheet(
            # Fix gap where title would be, with negative padding.
            # This is somewhat questionable (i.e. a total hack)
            'QGroupBox {margin-top: %spx; padding-top: %spx }' % (tweak-height, height))

    def output_change_subtab(self, subtab, solid):
        ui = self.ui.output
        index = (0 if subtab == FLUID_TAB
                 else len(self.solids)+1 if subtab == SCALAR_TAB
                 else len(self.solids)+2 if subtab == REACTIONS_TAB
                 else len(self.solids)+3 if subtab == OTHER_TAB
                 else solid)

        for i in range(ui.bottom_tab_layout.columnCount()):
            item = ui.bottom_tab_layout.itemAtPosition(0, i)
            if not item:
                continue
            widget = item.widget()
            if not widget:
                continue
            font = widget.font()
            font.setBold(i == index)
            widget.setFont(font)

        current_index = ui.stackedwidget_cell.currentIndex()
        # If we're switching from solid m to solid n, we need some
        # special handling, because both tabs are really the same
        # widget.  We make a picture of the current tab, display that
        # in a dummy pane, then slide back to the solids tab
        if subtab == current_index == SOLIDS_TAB:
            if solid == self.vtk_current_solid:
                return # nothing to do

            if solid > (self.vtk_current_solid or 0):
                dummy_label = ui.label_dummy_solids_L
                dummy_tab = SOLIDS_TAB_DUMMY_L
            else:
                dummy_label = ui.label_dummy_solids_R
                dummy_tab = SOLIDS_TAB_DUMMY_R

            picture = QPicture()
            ui.subpage_cell_solids.render(picture, flags=QWidget.DrawChildren) # avoid rendering bg
            dummy_label.setPicture(picture)
            ui.stackedwidget_cell.setCurrentIndex(dummy_tab)

        self.output_current_subtab = subtab
        self.vtk_current_solid = self.P = solid if subtab == SOLIDS_TAB else None
        self.setup_output_current_subtab()

        # change stackedwidget contents
        self.animate_stacked_widget(
            ui.stackedwidget_cell,
            ui.stackedwidget_cell.currentIndex(),
            subtab,
            direction='horizontal',
            line=ui.line_subtab,
            to_btn=ui.bottom_tab_layout.itemAtPosition(0, index),
            btn_layout=ui.bottom_tab_layout)
        # Scroll to top
        ui.scrollarea_cell.ensureVisible(0, 0)


    def handle_output_region_selection(self):
        ui = self.ui.output
        tw = ui.tablewidget_regions
        row = get_selected_row(tw)
        if row is None:
            indices = []
            regions = []
        else:
            (indices, regions) = tw.item(row, 0).data(UserRole)
        self.vtk_current_indices, self.vtk_current_regions = indices, regions
        enabled = (row is not None)
        ui.toolbutton_delete.setEnabled(enabled)
        ui.bottom_frame.setEnabled(enabled)
        ui.stackedwidget_cell_particle.setCurrentIndex(PAGE_CELL)
        if not enabled: # No selection, clear inputs
            for widget in widget_iter(ui.bottom_frame):
                if isinstance(widget, LineEdit):
                    widget.setText('')
                elif isinstance(widget, CheckBox):
                    widget.setChecked(False)
            return
        self.setup_output_vtk_tab() # reinitialize all widgets in current tab


    def fixup_output_table(self, tw, stretch_column=1):
        ui = self.ui.output
        hv = QHeaderView
        resize = tw.horizontalHeader().setSectionResizeMode
        ncols = tw.columnCount()
        for n in range(0, ncols):
            resize(n, hv.Stretch if n == stretch_column else hv.ResizeToContents)

        # trim excess vertical space - can't figure out how to do this in designer
        header_height = tw.horizontalHeader().height()

        # Note - scrollbar status can change outside of this function.
        # Do we need to call this everytime window geometry changes?
        scrollbar_height = tw.horizontalScrollBar().isVisible() * (4+tw.horizontalScrollBar().height())
        nrows = tw.rowCount()
        if nrows == 0:
            row_height = 0
            height = header_height+scrollbar_height
        else:
            row_height = tw.rowHeight(0)
            height = (header_height+scrollbar_height
                      + nrows*row_height + 4) # extra to avoid unneeded scrollbar (?)
        icon_height = sub_icon_height() + 8
        ui.top_frame.setMaximumHeight(height+icon_height)
        ui.top_frame.setMinimumHeight(header_height+icon_height+row_height*min(nrows, 5))
        ui.top_frame.updateGeometry()
        tw.setMaximumHeight(height)
        tw.setMinimumHeight(header_height)
        tw.updateGeometry()


    def output_show_regions_popup(self):
        # Users cannot select inapplicable regions.
        # VTK regions can be points, planes, or volumes (not STLs)
        # Regions can define multiple VTK regions.
        ui = self.ui.output
        rp = self.regions_popup
        rp.clear()
        for (name, data) in self.output_region_dict.items():
            shape = data.get('type', '---')
            # Assume available if unmarked
            available = (data.get('available', True)
                         and (shape in ('point', 'box')
                              or 'plane' in shape))
            row = (name, shape, available)
            rp.add_row(row)
        #Select Output type
        # Selection is required
        # Available selections:
        #  Cell data
        #    Selection always available
        #    Set keyword VTK_DATA(#) to 'C'
        #  Particle data
        #    Selection only available with DEM or PIC solids
        #    Sets keyword VTK_DATA(#) to 'P'
        solids_models = set(self.project.get_value('solids_model', args=[i])
                            for i in range(1, len(self.solids)+1))
        enabled = 'DEM' in solids_models or 'PIC' in solids_models

        rp.reset_signals()
        rp.save.connect(self.output_add_regions)
        rp.cancel.connect(self.output_cancel_add)
        for item in (ui.tablewidget_regions,
                     ui.bottom_frame,
                     ui.toolbutton_add,
                     ui.toolbutton_delete):
            item.setEnabled(False)
        rp.popup('Select region for VTK output')
        set_item_enabled(get_combobox_item(rp.combobox, 1), enabled)


    def output_delete_solids_phase(self, phase_index):
        """adjust vtk_current_solid when solids phase deleted"""
        if (self.vtk_current_solid is not None
                and self.vtk_current_solid >= phase_index):
            self.vtk_current_solid -= 1
            if self.vtk_current_solid == 0:
                self.vtk_current_solid = None


    def output_cancel_add(self):
        ui = self.ui.output

        for item in (ui.toolbutton_add,
                     ui.tablewidget_regions):
            item.setEnabled(True)

        if get_selected_row(ui.tablewidget_regions) is not None:
            for item in (ui.bottom_frame,
                         ui.toolbutton_delete):
                item.setEnabled(True)


    def output_add_regions(self):
        # Interactively add regions to define VTK Output
        ui = self.ui.output
        rp = self.regions_popup
        self.output_cancel_add() # Reenable input widgets
        selections = rp.get_selection_list()
        if not selections:
            return
        output_type = VTK_DATA_TYPES[rp.combobox.currentIndex()]
        self.output_add_regions_1(selections, output_type=output_type, indices=None, autoselect=True)
        self.output_setup_current_tab() # Update the widgets


    def output_add_regions_1(self, selections, output_type=None, indices=None, autoselect=False):
        # Used by both interactive and load-time add-region handlers
        ui = self.ui.output

        if output_type is None:
            output_type = 'C' #Cell data is default

        if self.output_region_dict is None:
            self.output_region_dict = self.ui.regions.get_region_dict()

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
                idx = self.output_find_index()
                indices[i] = idx
            self.outputs[idx] = {'region': region_name}
            region_data = self.output_region_dict.get(region_name)
            if region_data is None: # ?
                self.warn("no data for region %s" % region_name)
                continue
            self.output_set_region_keys(region_name, idx, region_data, output_type)
            #self.output_region_dict[region_name]['available'] = False # Mark as in-use
        item.setData(UserRole, (tuple(indices), tuple(selections)))
        tw.setItem(nrows, COLUMN_REGION, item)

        filebase = None if indices is None else self.project.get_value('vtk_filebase',
                                                                       args=[indices[0]],
                                                                       default='')
        item = make_item(filebase)
        tw.setItem(nrows, COLUMN_FILENAME, item)
        self.add_tooltip(item, key='vtk_filebase')

        name = 'Cell data' if output_type == 'C' else 'Particle data' if output_type == 'P' else '???'
        item = make_item(name)
        if output_type == 'C':
            item.setToolTip('Cell data (VTU file)')
        elif output_type == 'P':
            item.setToolTip('Particle data (VTP file)')
        tw.setItem(nrows, COLUMN_TYPE, item)

        item = make_item(','.join(map(str, indices)))
        tw.setItem(nrows, COLUMN_ID, item)
        #self.fixup_output_table(tw) # avoid dup. call

        if autoselect:
            tw.setCurrentCell(nrows, 0)


    def output_find_index(self):
        # Fill holes (?)
        n = 1
        while n in self.outputs:
            n += 1
        return n


    def output_delete_regions(self):
        ui = self.ui.output
        tw = ui.tablewidget_regions
        row = get_selected_row(tw)
        if row is None: # No selection
            return

        # Unset keywords
        kwlist = list(self.project.keywordItems())
        for kw in kwlist:
            key, args = kw.key, kw.args
            if key.startswith('vtk_') and args and args[0] in self.vtk_current_indices:
                self.unset_keyword(key, args=args)

        for r in self.vtk_current_regions:
            if r in self.output_region_dict:
                self.output_region_dict[r]['available'] = True

        for i in self.vtk_current_indices:
            del self.outputs[i]

        self.vtk_current_regions = []
        self.vtk_current_indices = []

        tw.removeRow(row)
        #self.fixup_output_table(tw)
        self.setup_output_vtk_tab()
        #self.update_nav_tree()

    def vtk_regions_to_str(self):
        """convert VTK output region definitions to savable form"""
        ui = self.ui.output
        tw = ui.tablewidget_regions
        data = [tw.item(i, 0).data(UserRole)
                for i in range(tw.rowCount())]
        return JSONEncoder().encode(data)


    def vtk_regions_from_str(self, s):
        if not s:
            return
        data = JSONDecoder().decode(s)
        for (indices, regions) in data:
            if not indices:
                continue # should not get empty tuple
            # vtk_data (output type) keyword should be set already when we call this
            output_type = self.project.get_value('vtk_data', args=[indices[0]], default='C')
            self.output_add_regions_1(regions, output_type=output_type,
                                      indices=indices, autoselect=False)


    def vtk_extract_regions(self):
        if self.outputs:
            # We assume that output regions have been initialized correctly
            # from mfix_gui_comments.
            # TODO: verify that there is an output region for each output
            return

        if self.output_region_dict is None:
            self.output_region_dict = self.ui.regions.get_region_dict()

        # TODO: if we wanted to be fancy, we could find regions where
        # output values matched, and merge into a new output region.  That
        # is only needed for projects created outside the GUI (otherwise
        # we have already stored the output regions).  Also would be nice
        # to offer a way to split compound regions.
        for vtk in self.project.vtks:

            d = vtk.keyword_dict
            extent = [d.get('vtk_'+k, None) for k in ('x_w', 'y_s', 'z_b',
                                                      'x_e', 'y_n', 'z_t')]
            extent = [0 if x is None else x.value for x in extent]
            #if any (x is None for x in extent):
            #    self.warn("vtk output %s: invalid extents %s" %
            #               (vtk.ind, extent))
            #    continue

            output_type = d.get('vtk_data') # Returns a keyword object or None, not a value
            if output_type is None:
                output_type = 'C' # Default

            else:
                output_type = output_type.value # get value from keyword object
                if output_type not in ('C', 'P'):
                    self.error("Invalid type %s for VTK output %s" % (output_type, vtk.ind))
                    continue

            for (region_name, data) in self.output_region_dict.items():
                ext2 = [0 if x is None else x for x in
                        (data.get('from', []) + data.get('to', []))]
                if ext2 == extent: # Don't need to check 'available' here since
                            # Regions can define multiple VTK regions.
                    self.output_add_regions_1([region_name], indices=[vtk.ind],
                                              output_type=output_type,
                                              autoselect=False)
                    break
            else:
                self.warn("vtk output %s: could not match defined region %s" %
                          (vtk.ind, extent))
                kwlist = list(self.project.keywordItems())
                for kw in kwlist:
                    key, args = kw.key, kw.args
                    if key.startswith('vtk_') and args and args[0]==vtk.ind:
                        self.unset_keyword(key, args=args)


    def init_output_spx_tab(self):
        ui = self.ui.output
        gb = ui.groupbox_print_des_data # aka "Write ASCII particle data"
        gb.clicked.connect(lambda val: self.update_keyword('print_des_data', val))
        self.add_tooltip(gb, key='print_des_data')

        cb = ui.combobox_des_output_type
        cb.currentIndexChanged.connect(lambda val: self.update_keyword('des_output_type', DES_OUTPUT_TYPES[val]))
        self.add_tooltip(cb, key='des_output_type')


    def init_output_netcdf_tab(self):
        ui = self.ui.output
        # Special handling for all bwrite_netcdf checkboxes
        for w in widget_iter(ui.groupbox_netcdf):
            if isinstance(w, CheckBox):
                w.post_update = self.output_handle_bwrite_netcdf

        gb = ui.groupbox_print_des_data_2 # aka "Write ASCII particle data"
        gb.clicked.connect(lambda val: self.update_keyword('print_des_data', val))
        self.add_tooltip(gb, key='print_des_data')

        cb = ui.combobox_des_output_type_2
        cb.currentIndexChanged.connect(lambda val: self.update_keyword('des_output_type', DES_OUTPUT_TYPES[val]))
        self.add_tooltip(cb, key='des_output_type')


    def output_handle_bwrite_netcdf(self):
        # Enable/disable spx_dt inputs based on bwrite_netcdf,
        #  and keep certain pairs of bwrite_netcdf flags in sync

        #Write interval for gas volume fraction
        #    Sets keyword BWRITE_NETCDF(1) = .TRUE.
        #    Sets keyword SPX_DT(1)
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT
        #Write interval for gas and solids pressure
        #    Sets keyword BWRITE_NETCDF(2) = .TRUE.
        #    Sets keyword BWRITE_NETCDF(3) = .TRUE.
        #    Sets keyword SPX_DT(2)
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT
        #Write interval for gas velocity
        #    Sets keyword BWRITE_NETCDF(4) = .TRUE.
        #    Sets keyword SPX_DT(3)
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT
        #Write interval for solids velocity
        #    Sets keyword BWRITE_NETCDF(5) = .TRUE.
        #    Sets keyword SPX_DT(4)
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT
        #Write interval for solids bulk density
        #    Sets keyword BWRITE_NETCDF(6) = .TRUE.
        #    Sets keyword SPX_DT(5)
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT
        #Write interval for gas and solids temperature
        #    Only available when solving energy equations
        #    Sets keyword BWRITE_NETCDF(7) = .TRUE.
        #    Sets keyword BWRITE_NETCDF(8) = .TRUE.
        #    Sets keyword SPX_DT(6)
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT
        #Write interval for gas and solids mass fractions
        #    Only available when solving species equations
        #    Sets keyword BWRITE_NETCDF(9) = .TRUE.
        #    Sets keyword BWRITE_NETCDF(10) = .TRUE.
        #    Sets keyword SPX_DT(7)
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT
        #Write interval for granular temperature
        #    Only available when KT_TYPE =/ 'ALGEBRAIC'
        #    Sets keyword BWRITE_NETCDF(11) = .TRUE.
        #    Sets keyword SPX_DT(8)
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT
        #Write interval for user defined scalars
        #    Only available when solving any user defined scalar equations
        #    Sets keyword BWRITE_NETCDF(12) = .TRUE.
        #    Sets keyword SPX_DT(9)
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT
        #Write interval for reaction rates
        #    Only available if NRR > 0 (see below)
        #    Sets keyword BWRITE_NETCDF(13) = .TRUE.
        #    Sets keyword SPX_DT(10)
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT
        #Write interval for turbulence quantities
        #    Only available if TURBULENCE_MODEL = “K_EPSILON”
        #    Sets keyword BWRITE_NETCDF(14) = .TRUE.
        #    Sets keyword SPX_DT(11)
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        ui = self.ui.output
        key = 'bwrite_netcdf'
        bwrite_netcdf = [self.project.get_value(key, default=False, args=[i])
                         for i in range(0, MAX_BWRITE_NETCDF+1)] # start at 0 so 1-based index works

        res_dt = self.project.get_value('res_dt', default=1.0)
        default = max(res_dt, 1.0)

        def enable_input(idx, enabled):
            enabled = bool(enabled)
            le = getattr(ui, 'lineedit_keyword_spx_dt_2_args_%s' % idx)
            la = getattr(ui, 'label_spx_dt_2_units_args_%s' % idx)
            le.setEnabled(enabled)
            la.setEnabled(enabled)
            if enabled:
                val = self.project.get_value('spx_dt', args=[idx])
                if val is None:
                    prev_val = le.value # restore value from lineedit
                    if prev_val != '' and prev_val is not None:
                        val = prev_val
                    else:
                        val = default
                if val < res_dt:
                    val = res_dt
                self.update_keyword('spx_dt', val, args=[idx])
                le.min = res_dt

            else:
                self.unset_keyword('spx_dt', args=[idx])

        # Association of bwrite flags to spx_dt flags, see comments above
        index_map = [1, 2, 4, 5, 6, 7, 9, 11, 12, 13, 14]
        for spx_idx, bwrite_idx in enumerate(index_map, 1):
            enabled = bwrite_netcdf[bwrite_idx]
            enable_input(spx_idx, enabled)
            # Why do we have to do this?  Setting keyword should update checkbox
            cb = getattr(ui, 'checkbox_keyword_bwrite_netcdf_args_%s' % bwrite_idx)
            cb.setChecked(enabled)

        # Certain BWRITE_NETCDF flags are set in pairs
        for n in (2, 7, 9):
            val = bwrite_netcdf[n+1] = bwrite_netcdf[n]
            self.update_keyword(key, val, args=[n+1])


    def output_enable_vtk(self, enabled):
        self.update_keyword('write_vtk_files', enabled)
        self.setup_output() # enable/disable gui widgets


    def output_enable_spx(self, enabled):
        ui = self.ui.output
        if enabled:
            ui.pushbutton_spx.setEnabled(True)
            ui.pushbutton_netcdf.setEnabled(False)
            ui.checkbox_netcdf.setChecked(False)
            ui.checkbox_netcdf.setEnabled(False)

            res_dt = self.project.get_value('res_dt', default=1.0)
            default = max(1.0, res_dt)
            for i in range(1, MAX_SP+1): # Only set keys not already set
                if self.project.get_value('spx_dt', args=[i]) is None:
                    # Try to restore value from disabled lineedit
                    le = getattr(ui, 'lineedit_keyword_spx_dt_args_%s' % i)
                    val = le.value
                    if val == '' or val is None or val == 0:
                        val = default
                    self.update_keyword('spx_dt', val, args=[i])
        else:
            ui.pushbutton_spx.setEnabled(False)
            ui.checkbox_netcdf.setEnabled(True) #Enable option, but do not activate
            for i in range(1, MAX_SP+1):
                self.unset_keyword('spx_dt', args=[i])



    def output_enable_netcdf(self, enabled):
        ui = self.ui.output
        if enabled:
            ui.pushbutton_netcdf.setEnabled(True)
            ui.pushbutton_spx.setEnabled(False)
            ui.checkbox_spx.setChecked(False)
            ui.checkbox_spx.setEnabled(False)
        else:
            ui.pushbutton_netcdf.setEnabled(False)
            ui.checkbox_spx.setEnabled(True) # Enable, do not activate
            for i in range(1, MAX_BWRITE_NETCDF+1):
                self.unset_keyword('bwrite_netcdf', args=[i])
            for i in range(1, MAX_SP+1):
                self.unset_keyword('spx_dt', args=[i])
        self.output_handle_bwrite_netcdf()


    def setup_output(self):
        ui = self.ui.output
        # Grab a fresh copy, may have been updated
        self.output_region_dict = self.ui.regions.get_region_dict()

        # Mark regions which are in use (this gets reset each time we get here)
        #for (i, data) in self.outputs.items():
        #    region = data['region']
        #    if region in self.output_region_dict:
        #        self.output_region_dict[region]['available'] = False


        #Write binary Single Precision files (SPx)
        #    No keyword association
        #    Enables SPx tab
        #    Backwards compatibility: Enabled if any SPx time values are specified

        spx_dt_specified = any(self.project.get_value('spx_dt', args=[i]) is not None
                               for i in range(1, MAX_SP+1)) # Note, enabled in template! XXX Jordan?
        bwrite_netcdf_specified = any(bool(self.project.get_value('bwrite_netcdf', args=[i]))
                                      for i in range(1, MAX_BWRITE_NETCDF+1))

        enable_spx = not bwrite_netcdf_specified # Enables checkbox but does not check it
        activate_spx = enable_spx and spx_dt_specified
        ui.checkbox_spx.setEnabled(enable_spx)
        ui.checkbox_spx.setChecked(activate_spx)
        ui.pushbutton_spx.setEnabled(activate_spx)

        #Enable NetCDF output files
        #    Not available when SPx output is enabled
        #    No keyword association.
        #    Enables NetCDF tab

        enable_netcdf = not activate_spx
        activate_netcdf = bwrite_netcdf_specified
        ui.checkbox_netcdf.setEnabled(enable_netcdf)
        ui.checkbox_netcdf.setChecked(activate_netcdf)
        ui.pushbutton_netcdf.setEnabled(activate_netcdf)

        vtk_enabled = self.project.get_value('write_vtk_files', default=False)
        ui.pushbutton_vtk.setEnabled(vtk_enabled)

        # TODO don't stay on disabled tab
        self.output_setup_current_tab()


    def setup_output_tab(self, tabnum):
        if tabnum == BASIC_TAB:
            self.setup_output_basic_tab()
        elif tabnum == VTK_TAB:
            self.setup_output_vtk_tab()
        elif tabnum == SPX_TAB:
            self.setup_output_spx_tab()
        elif tabnum == NETCDF_TAB:
            self.setup_output_netcdf_tab()
        else:
            raise ValueError(tabnum)


    def output_setup_current_tab(self):
        self.setup_output_tab(self.output_current_tab)


    def setup_output_basic_tab(self):
        #Basic (tab)
        ui = self.ui.output
        #
        #Specify Restart/checkpoint write interval
        #    Specification always available (required)
        #    Sets keyword RES_DT
        #    DEFAULT 1.0
        key = 'res_dt'
        res_dt = self.project.get_value(key, default=1.0)

        # 'reverse constraint', res_dt must be less than a bunch of other keys
        # (which must be greater than it)
        #m = min(safe_float(self.project.get_value('spx_dt', default=1.0, args=[i]))
        #        for i in range(1,MAX_SP+1))
        #if self.project.get_value('res_backups', default=0) > 0:
        #    m = min(m, safe_float(self.project.get_value('res_backup_dt', default=1.0)))
        #ui.lineedit_keyword_res_dt.max = m
        #  Note, this seems like a good idea but makes it hard to change values

        #if res_dt > m:
        #    res_dt = m

        #    self.update_keyword(key, res_dt)

        #Specify the number of backup copies
        #    Specification always available
        #    Sets keyword RES_BACKUPS
        #    DEFAULT 0
        #    Error check: value is greater than or equal to 0

        #Specify the backup interval
        #    Specification only available if RES_BACKUPS > 0
        enabled = self.project.get_value('res_backups', default=0) > 0
        #    Sets keyword RES_BACKUP_DT
        key = 'res_backup_dt'
        #    DEFAULT 1.0
        default = 1.0
        #    Error check: value must be greater than or equal to RES_DT
        ui.lineedit_keyword_res_backup_dt.min = res_dt
        for item in (ui.label_res_backup_dt,
                     ui.lineedit_keyword_res_backup_dt,
                     ui.label_res_backup_dt_units):
            item.setEnabled(enabled)
            if enabled:
                val = self.project.get_value(key)
                if val is None:
                    val = default
                    if val < res_dt:
                        val = res_dt
                    self.update_keyword(key, val)

        #Enable VTK output
        #    Specification always available
        #    Sets keyword WRITE_VTK_FILES
        #    DEFAULT .FALSE.
        #    Enables VTK tab
        # (handled by keyword widget and post_update)
        write_vtk_files = self.project.get_value('write_vtk_files', default=False)
        ui.groupbox_write_vtk_files.setChecked(write_vtk_files)
        ui.pushbutton_vtk.setEnabled(write_vtk_files)

        #Enable time-dependent VTK files
        #    Specification only if WRITE_VTK_FILES = .TRUE.
        enabled = bool(write_vtk_files)

        #    Sets keyword TIME_DEPENDENT_FILENAME
        key = 'time_dependent_filename'
        #    DEFAULT value .TRUE.
        default = True
        cb = ui.checkbox_keyword_time_dependent_filename
        #cb.setEnabled(enabled)        #(handled by checkable groupbox)
        value = self.project.get_value(key)
        self.add_tooltip(cb, key, value=True if value is None else value)
        if enabled:
            if value is None:
                value = default
                self.update_keyword(key, value)
        else:
            self.unset_keyword(key)
        ui.pushbutton_vtk.setEnabled(enabled)

        #Specify VTK Directory
        #    Specification only if WRITE_VTK_FILES = .TRUE.
        #    Sets keyword VTU_DIR
        #    No default (empty string)
        key = 'vtu_dir'
        enabled = bool(write_vtk_files)
        value = self.project.get_value(key, default='')
        #for item in (ui.label_vtu_dir, ui.lineedit_keyword_vtu_dir):
        #    item.setEnabled(enabled)        #(handled by checkable groupbox)
        if enabled:
            if value is None or value == '':
                value = ui.lineedit_keyword_vtu_dir.value # saved value in GUI
                if value is None:
                    value = default
                self.update_keyword(key, value)
        else:
            self.unset_keyword(key)
        ui.pushbutton_vtk.setEnabled(enabled)


    def setup_output_spx_tab(self):
        """SPx (tab) Note: Technically, MFiX will now permit a user to mix-and-match
        the SPx output files meaning that some can be written and others not.
        However, this is likely to break the ParaView reader. Therefore, if the
        “Write binary SPx” checkbox is enabled, output is required for all SPx
        files. Otherwise, all should remain unspecified to skip writing the SPx
        files.
        """

        ui = self.ui.output

        #Write interval for gas volume fraction
        #    Sets keyword SPX_DT(1)
        #    DEFAULT 1.0
        #    Required if SPx data is enabled.
        #    Error check: value must be greater than or equal to RES_DT

        #Write interval for gas and solids pressure
        #    Sets keyword SPX_DT(2)
        #    Required if SPx data is enabled.
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        #Write interval for gas velocity
        #    Sets keyword SPX_DT(3)
        #    Required if SPx data is enabled.
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        #Write interval for solids velocity
        #    Sets keyword SPX_DT(4)
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        #Write interval for solids bulk density
        #    Sets keyword SPX_DT(5)
        #    Required if SPx data is enabled.
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        #Write interval for gas and solids temperature
        #    Sets keyword SPX_DT(6)
        #    Required if SPx data is enabled and solving any energy equations.
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        #Write interval for gas and solids mass fractions
        #    Sets keyword SPX_DT(7)
        #    Required if SPx data is enabled and solving any species equations.
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        #Write interval for granular temperature
        #    Sets keyword SPX_DT(8)
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        #Write interval for user defined scalars
        #    Sets keyword SPX_DT(9)
        #    Required if SPx data is enabled and solving any user defined scalar equations
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        #Write interval for reaction rates
        #    Required if SPx data is enabled and NRR > 0 (see below)
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        #Write interval for turbulence quantities
        #    Sets keyword SPX_DT(11)
        #    Required if SPx data is enabled and TURBULENCE_MODEL = “K_EPSILON”
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        res_dt = self.project.get_value('res_dt', default=1.0)

        key = 'spx_dt'
        default = 1.0
        for sp in range(1, MAX_SP+1):
            le = getattr(ui, 'lineedit_keyword_spx_dt_args_%s' % sp)
            le.min = res_dt
            val = self.project.get_value(key, args=[sp])
            if val is None:
                val = default
                if val < res_dt:
                    val = res_dt
                self.update_keyword(key, val, args=[sp])

        #Write ASCII particle data
        #    Selection only available if DEM or PIC solids
        #    Sets keyword PRINT_DES_DATA
        #    DEFAULT .TRUE.
        key = 'print_des_data'
        solids_models = set(self.project.get_value('solids_model', args=[i])
                            for i in range(1, len(self.solids)+1))
        enabled = 'DEM' in solids_models or 'PIC' in solids_models
        gb = ui.groupbox_print_des_data
        if enabled:
            checked = self.project.get_value(key, default=True)
        else:
            checked = self.project.get_value(key, default=False)
        gb.setEnabled(enabled)
        gb.setChecked(checked)

        #Specify VTP Directory
        #    Specification only if PRINT_DES_DATA = .TRUE.
        #    Sets keyword VTP_DIR
        #    No default (empty string)
        #Select particle data format
        #
        #Selection only available if DEM or PIC solids and PRINT_DES_DATA = .TRUE.
        #  (note, containing groupbox is disabled if this condition is not met)
        #Sets keyword DES_OUTPUT_TYPE
        #Available Selections
        #  ParaView - VTK/.vtp [DEFAULT]
        #  Tecplot - .dat
        cb = ui.combobox_des_output_type
        key = 'des_output_type'
        default = DES_OUTPUT_TYPES[0] # "PARAVIEW"
        val = self.project.get_value(key, default)
        if val not in DES_OUTPUT_TYPES:
            val = default
            self.update_keyword(key, val)
        cb.setCurrentIndex(DES_OUTPUT_TYPES.index(val))


    def setup_output_netcdf_tab(self):
        """NetCDF (tab) Note: NetCDF support 'piggy-backs' off of the SPx keywords.
        The output time values are specified via SPX_DT while NetCDF output is
        triggered by a BWRITE_NETCDF flag. To make this less opaque to users,
        both SPx and netCDF output cannot be enabled at the same time.

        """

        ui = self.ui.output
        self.output_handle_bwrite_netcdf()

        #This is the same particle section as the SPx section.

    #Write ASCII particle data
        #    Selection only available if DEM or PIC solids
        #    Sets keyword PRINT_DES_DATA
        #    DEFAULT .TRUE.
        #    Selection only available if DEM or PIC solids and PRINT_DES_DATA = .TRUE.
        key = 'print_des_data'
        solids_models = set(self.project.get_value('solids_model', args=[i])
                            for i in range(1, len(self.solids)+1))
        enabled = 'DEM' in solids_models or 'PIC' in solids_models
        gb = ui.groupbox_print_des_data_2
        if enabled:
            checked = self.project.get_value(key, default=True)
        else:
            checked = self.project.get_value(key, default=False)
        gb.setEnabled(enabled)
        gb.setChecked(checked)

        #Specify VTP Directory
        #    Specification only if PRINT_DES_DATA = .TRUE.
        #    Sets keyword VTP_DIR
        #    No default (empty string)
        #Select particle data format
        #
        #Selection only available if DEM or PIC solids and PRINT_DES_DATA = .TRUE.
        #  (note, containing groupbox is disabled if this condition is not met)
        #Sets keyword DES_OUTPUT_TYPE
        #Available Selections
        #  ParaView - VTK/.vtp [DEFAULT]
        #  Tecplot - .dat
        cb = ui.combobox_des_output_type_2
        key = 'des_output_type'
        default = DES_OUTPUT_TYPES[0] # "PARAVIEW"
        val = self.project.get_value(key, default)
        if val not in DES_OUTPUT_TYPES:
            val = default
            self.update_keyword(key, val)
        cb.setCurrentIndex(DES_OUTPUT_TYPES.index(val))

    def output_default_vtk_filebase(self, region_names):
        # Construct default value for VTK_FILEBASE,
        # replacing possibly troublesome characters
        key = 'vtk_filebase'
        val = '+'.join(region_names)
        for c in ': /*?': # is this enough?
            val = val.replace(c, '_')
        indices = self.project.get_key_indices(key)
        names = set(self.project.get_value(key, args=i) for i in indices)
        val0 = val
        count = 0
        while val in names:
            count += 1
            val = '%s_%s' % (val0, count)
        return val


    def setup_output_vtk_tab(self):
        ui = self.ui.output
        self.fixup_output_table(ui.tablewidget_regions)

        row = get_selected_row(ui.tablewidget_regions)
        # Autoselect if only 1 row
        if row is None and ui.tablewidget_regions.rowCount() == 1:
            row = 0
            ui.tablewidget_regions.setCurrentCell(row, 0)
        enabled = (row is not None)
        ui.toolbutton_delete.setEnabled(enabled)
        ui.bottom_frame.setEnabled(enabled)

        indices = self.vtk_current_indices
        if not indices:
            ui.stackedwidget_cell_particle.setCurrentIndex(PAGE_CELL)
            #Construct the GUI, even though disabled (species checkboxes)
            self.setup_output_current_subtab()
            return

        V0 = indices[0]
        vtk_data = self.project.get_value('vtk_data', args=[V0], default='C')

        # Note, filebase through nzs are common to cell/particle

        #Specify filename base
        # Specification is required.
        # Sets keyword VTK_FILEBASE(#)
        # DEFAULT region name
        # TODO make sure it's a valid filename
        key = 'vtk_filebase'
        le = ui.lineedit_keyword_vtk_filebase_args_VTK
        tw = ui.tablewidget_regions
        val = self.project.get_value(key, args=[V0])
        if val is None: # Construct from region name
            val = self.output_default_vtk_filebase(self.vtk_current_regions)
            for V in self.vtk_current_indices:
                self.update_keyword(key, val, args=[V])
        le.updateValue(key, val)
        for i in range(tw.rowCount()):
            data = tw.item(i, 0).data(UserRole)
            indices, names = data
            if V0 in indices:
                tw.item(i, COLUMN_FILENAME).setText(val)

        #Specify write interval
        # Specification is required
        # Sets keyword VTK_DT(#)
        # DEFAULT 1.0 (must write)
        key = 'vtk_dt'
        default = 1.0
        le = ui.lineedit_keyword_vtk_dt_args_VTK
        val = self.project.get_value(key, args=[V0])
        if val is None:
            val = default
            for V in self.vtk_current_indices:
                self.update_keyword(key, val, args=[V])
        le.updateValue(key, val)

        for c in 'xyz':
            #Specify region x[yz]-axis slices
            # Specification always available
            # Sets keyword VTK_NXS(#)
            # DEFAULT 0
            key = 'vtk_n%ss' % c
            default = 0
            le = getattr(ui, 'lineedit_keyword_vtk_n%ss_args_VTK'%c)
            val = self.project.get_value(key, args=[V0])
            if val is None:
                val = default
                for V in self.vtk_current_indices:
                    self.update_keyword(key, val, args=[V])
            le.updateValue(key, val)

        if vtk_data == 'C':
            self.setup_output_vtk_cell()
            ui.stackedwidget_cell_particle.setCurrentIndex(PAGE_CELL)
            ui.scrollarea_cell.ensureVisible(0, 0)
        elif vtk_data == 'P':
            self.setup_output_vtk_particle()
            ui.stackedwidget_cell_particle.setCurrentIndex(PAGE_PARTICLE)
            ui.scrollarea_particle.ensureVisible(0, 0)
        else:
            self.error("Unknown vtk_data %s" % vtk_data)
            ui.stackedwidget_cell_particle.setCurrentIndex(PAGE_CELL)


    def setup_output_vtk_cell(self):
        #Cell data sub-pane

        #There is a need for some hand waving here. Many mfix.dat files may use a different specification
        #for VTK input. There will need to be a way of catching the 'old' format and converting it to this
        #input style.

        ui = self.ui.output
        b = ui.pushbutton_fluid
        b.setText(self.fluid_phase_name)
        font = b.font()
        font.setBold(self.output_current_tab == FLUID_TAB)
        b.setFont(font)
        w = b.fontMetrics().boundingRect(b.text()).width() + 20
        b.setMaximumWidth(w)


        solids_names = list(self.solids.keys())
        if self.output_saved_solids_names != solids_names:
            # Clear out the old ones
            n_cols = ui.bottom_tab_layout.columnCount()
            for i in range(n_cols-1, 0, -1):
                item = ui.bottom_tab_layout.itemAtPosition(0, i)
                if not item:
                    continue
                widget = item.widget()
                if not widget:
                    continue
                if widget in self.output_pushbuttons_bottom:
                    continue
                ui.bottom_tab_layout.removeWidget(widget)
                widget.setParent(None)
                widget.deleteLater()
            # And make new ones
            for (i, solid_name) in enumerate(solids_names, 1):
                b = QPushButton(text=solid_name)
                w = b.fontMetrics().boundingRect(solid_name).width() + 20
                b.setMaximumWidth(w)
                b.setFlat(True)
                font = b.font()
                font.setBold(self.output_current_subtab == SOLIDS_TAB and i == self.vtk_current_solid)
                b.setFont(font)
                b.pressed.connect(lambda i=i: self.output_change_subtab(SOLIDS_TAB, i))
                ui.bottom_tab_layout.addWidget(b, 0, i)

        # Move the 'Scalar' and other buttons to the right of all solids, if needed
        if len(self.solids) != len(self.output_saved_solids_names):
            for (i, b) in enumerate(self.output_pushbuttons_bottom):
                if b == ui.pushbutton_fluid:
                    continue
                ui.bottom_tab_layout.removeWidget(b)
                ui.bottom_tab_layout.addWidget(b, 0, i+len(self.solids))

        b = ui.pushbutton_scalar
        font = b.font()
        font.setBold(self.output_current_subtab == SCALAR_TAB)
        b.setFont(font)
        nscalar = self.project.get_value('nscalar', default=0)
        enabled = (nscalar > 0)
        b.setEnabled(enabled)

        b = ui.pushbutton_reactions
        font = b.font()
        font.setBold(self.output_current_subtab == REACTIONS_TAB)
        b.setFont(font)
        nrr = self.project.get_value('nrr', default=0)
        enabled = (nrr > 0)
        b.setEnabled(enabled)

        b = ui.pushbutton_other
        font = b.font()
        font.setBold(self.output_current_subtab == OTHER_TAB)
        b.setFont(font)
        enabled = True
        b.setEnabled(enabled)

        self.output_saved_solids_names = solids_names

        for (i, solid_name) in enumerate(self.solids.keys(), 1):
            model = self.project.get_value('solids_model', args=[i])
            # All vtk solids keywords require TFM solids, so disable tab completely if not applicable
            b = ui.bottom_tab_layout.itemAtPosition(0, i).widget()
            if model == 'TFM':
                b.setEnabled(True)
                b.setToolTip(None)
            else:
                b.setEnabled(False)
                b.setToolTip("VTK output only supported for TFM solids""")

        # make sure underline is in the right place, as # of solids may
        # have changed (lifted from animate_stacked_widget, which we
        # don't want to call here)
        tab = self.output_current_subtab
        line_to = self.output_tab_to_index(tab, self.vtk_current_solid)
        line = ui.line_subtab
        btn_layout = ui.bottom_tab_layout
        if line_to is not None:
            btn_layout.addItem(btn_layout.takeAt(
                btn_layout.indexOf(line)), 1, line_to)

        # Don't stay on disabled tab TODO

        indices = self.vtk_current_indices
        if not indices:
            # Clear inputs?  should have been done in handle_selection
            return

        self.setup_output_current_subtab()


    def output_tab_to_index(self, tab, solid):
        return (0 if tab == FLUID_TAB
                else len(self.solids)+1 if tab == SCALAR_TAB
                else len(self.solids)+2 if tab == REACTIONS_TAB
                else len(self.solids)+3 if tab == OTHER_TAB
                else solid)

    def setup_output_current_subtab(self):
        self.setup_output_subtab(self.output_current_subtab,
                                 self.vtk_current_solid)


    def setup_output_subtab(self, subtab, solid):
        if subtab == FLUID_TAB:
            self.setup_output_fluid_subtab()
        elif subtab == SOLIDS_TAB:
            self.setup_output_solids_subtab(solid)
        elif subtab == SCALAR_TAB:
            self.setup_output_scalar_subtab()
        elif subtab == REACTIONS_TAB:
            self.setup_output_reactions_subtab()
        elif subtab == OTHER_TAB:
            self.setup_output_other_subtab()
        else:
            raise ValueError(subtab)


    def setup_output_fluid_subtab(self):
        # Fluid Phase (tab?)
        ui = self.ui.output
        layout = ui.groupbox_cell_fluid.layout()
        indices = self.vtk_current_indices
        V0 = indices[0] if indices else None
        # NB normally we bail out if indices is None but we want to construct
        #  the fluid species checkboxes (for visual apperarance sake)

        #Enable writing gas volume fraction
        # Selection always available
        # Sets keyword VTK_EP_G(#)
        # DEFAULT value .FALSE.

        #Enable writing gas pressure
        # Requires fluid solver (RO_G0 /= 0.0)
        # Sets keyword VTK_P_G(#)
        # DEFAULT value .FALSE.

        #Enable writing gas velocity vector
        # Requires fluid solver (RO_G0 /= 0.0)
        # Sets keyword VTK_VEL_G(#)
        # DEFAULT value .FALSE.

        #Enable writing gas velocity x-component
        # Requires fluid solver (RO_G0 /= 0.0)
        # Sets keyword VTK_U_G(#)
        # DEFAULT value .FALSE.

        #Enable writing gas velocity y-component
        # Requires fluid solver (RO_G0 /= 0.0)
        # Sets keyword VTK_V_G(#)
        # DEFAULT value .FALSE.

        #Enable writing gas velocity z-component
        # Requires fluid solver (RO_G0 /= 0.0)
        # Sets keyword VTK_W_G(#)
        # DEFAULT value .FALSE.

        #Enable writing gas temperature
        # Requires fluid solver (RO_G0 /= 0.0) and ENERGY_EQ = .TRUE.
        # Sets keyword VTK_T_G(#)
        # DEFAULT value .FALSE.

        #Enable writing turbulent kinetic energy
        #    Requires fluid solver (RO_G0 /= 0.0) and TURBULENCE_MODEL='K_EPSILON'
        #    Sets keyword VTK_K_TURB_G(#)
        #    DEFAULT .FALSE.

        #Enable writing turbulent dissipation
        #    Requires fluid solver (RO_G0 /= 0.0) and TURBULENCE_MODEL='K_EPSILON'
        #    Sets keyword VTK_E_TURB_G(#)
        #    DEFAULT .FALSE.

        for key in ('vtk_ep_g',
                    'vtk_p_g',
                    'vtk_vel_g',
                    'vtk_u_g',
                    'vtk_v_g',
                    'vtk_w_g',
                    'vtk_k_turb_g',
                    'vtk_e_turb_g'):
            cb = getattr(ui, 'checkbox_keyword_%s_args_VTK' % key)
            val = False if V0 is None else self.project.get_value(key, args=[V0], default=False)
            cb.setChecked(val)
            cb.setEnabled(not self.fluid_solver_disabled  or key == 'vtk_ep_g')


        # disable temp if energy_eq disabled
        cb = ui.checkbox_keyword_vtk_t_g_args_VTK
        key = 'vtk_t_g'
        enabled = not self.fluid_solver_disabled and self.project.get_value('energy_eq', default=True)
        cb.setEnabled(enabled)
        if not enabled:
            cb.setChecked(False)
            for V in self.vtk_current_indices:
                self.unset_keyword(key, args=[V])

        # disable turbulences if model != K_EPSILON
        turbulence_model = self.project.get_value('turbulence_model', default=DEFAULT_TURBULENCE_MODEL)
        enabled = not self.fluid_solver_disabled and (turbulence_model == 'K_EPSILON')
        for key in ('vtk_k_turb_g', 'vtk_e_turb_g'):
            cb = getattr(ui, 'checkbox_keyword_%s_args_VTK' % key)
            cb.setEnabled(enabled)
            if not enabled:
                cb.setChecked(False)
                for V in self.vtk_current_indices:
                    self.unset_keyword(key, args=[V])

        #Enable writing gas species N (an entry for each defined species)
        # Requires defined gas phase species
        # Sets keyword VTK_X_G(#,N)
        # DEFAULT value .FALSE.
        key = 'vtk_x_g'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        fluid_species_names = list(self.fluid_species.keys())
        if self.output_saved_fluid_species_names != fluid_species_names:
            self.output_saved_fluid_species_names = fluid_species_names
            n_fluid_species = len(fluid_species_names)
            if len(cbs) > n_fluid_species:
                for (i, cb) in enumerate(cbs[n_fluid_species:], 1+n_fluid_species):
                    for V in self.vtk_current_indices:
                        self.unset_keyword(key, args=[V, i])
                    layout.removeWidget(cb)
                    cb.setParent(None)
                    cb.deleteLater()
                ui.dynamic_widgets[key] = cbs = cbs[:n_fluid_species]
            while len(cbs) < n_fluid_species:
                n = 1+len(cbs)
                cb = CheckBox('%s mass fraction' % fluid_species_names[n-1])
                cb.key = key
                cb.args = ['VTK', n]
                cb.value_updated.connect(self.project.submit_change)
                cbs.append(cb)
                self.add_tooltip(cb, key=cb.key)
                layout.addWidget(cb)
            for (cb, name) in zip(cbs, fluid_species_names):
                cb.setText(name)

        # Set checkboxes to correct state
        species_eq = self.project.get_value('species_eq', default=True, args=[0])
        enabled = bool(species_eq)
        for (i, cb) in enumerate(cbs, 1):
            cb.setEnabled(enabled)
            if enabled:
                val = False if V0 is None else self.project.get_value(key, args=[V0, i], default=False)
                cb.setChecked(bool(val))
            else:
                cb.setChecked(False)
                for V in self.vtk_current_indices:
                    self.unset_keyword(key, args=[V, i])


    def setup_output_reactions_subtab(self):
        ui = self.ui.output
        V0 = self.vtk_current_indices[0] if self.vtk_current_indices else None
        # Dynamically created GUI items
        layout = ui.groupbox_cell_reactions.layout()
        #Enable writing reaction rates
        # Requires nRR > 0
        # Sets keyword VTK_RRATE(#, #)
        # DEFAULT value .FALSE.

        key = 'vtk_rrate'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        nrr = self.project.get_value('nrr', default=0)
        # Remove extra widgets if number decreased
        if len(cbs) > nrr:
            for (i, cb) in enumerate(cbs[nrr:], 1+nrr):
                for V in self.vtk_current_indices:
                    self.unset_keyword(key, args=[V, i])
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:nrr]
        key = 'vtk_rrate_label'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        ws = ui.dynamic_widgets[key] # Values are 2-tuples, (label,lineedit)
        if len(ws) > nrr:
            for (i, (label,le)) in enumerate(ws[nrr:], 1+nrr):
                for V in self.vtk_current_indices:
                    self.unset_keyword(key, args=[V, i])
                for w in (label, le):
                    layout.removeWidget(w)
                    layout.removeWidget(w)
                    w.setParent(None)
                    w.deleteLater()
            ui.dynamic_widgets[key] = ws = ws[:nrr]

        while len(cbs) < nrr:
            key = 'vtk_rrate'
            n = 1+len(cbs)
            cb = CheckBox("Reaction rate %s" % n)
            cb.key = key
            cb.args = ['VTK', n]
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            layout.addWidget(cb, n-1, 0)
            key = 'vtk_rrate_label'
            label = QLabel('Label')
            label.args = ['VTK', n]
            self.add_tooltip(label, key=key)
            layout.addWidget(label, n-1, 1)
            le = LineEdit()
            le.key = key
            le.args = ['VTK', n]
            le.dtype = str
            le.value_updated.connect(self.project.submit_change)
            ws.append((label,le))
            self.add_tooltip(le, key=le.key)
            layout.addWidget(le, n-1, 2)


        # Set checkboxes to correct state
        key = 'vtk_rrate'
        for (i, cb) in enumerate(cbs, 1):
            val = self.project.get_value(key, args=[V0, i], default=False)
            cb.setChecked(bool(val))
        # Set lineedits to correct state
        key = 'vtk_rrate_label'
        for (i, (label, le)) in enumerate(ws, 1):
            val = self.project.get_value(key, args=[V0, i], default='')
            le.setText(val)


    def setup_output_solids_subtab(self, P):
        #Solids Phase (tab?)
        ui = self.ui.output
        self.vtk_current_solid = self.P = P
        if P is None: # Nothing to do (?)
            return
        if not self.vtk_current_indices: # No region selected
            # Clear inputs?
            return
        indices = self.vtk_current_indices
        V0 = indices[0]

        #Enable writing solids pressure  (moved from fluid to solids tab)
        # Requires TFM solids
        # Sets keyword VTK_P_STAR
        # DEFAULT value .FALSE.
        key = 'vtk_p_star'
        default = False
        cb = ui.checkbox_keyword_vtk_p_star_args_VTK
        val = self.project.get_value(key, default, args=[V0])
        cb.setChecked(bool(val))

        #Enable writing solids velocity vector
        # Requires TFM solids
        # Sets keyword VTK_VEL_S(#,#)
        # DEFAULT value .FALSE.

        #Enable writing solids velocity x/y/z-component
        # Requires TFM solids
        # Sets keyword VTK_U/V/W_S(#,#)
        # DEFAULT value .FALSE.

        #Enable writing solids bulk density
        # Requires TFM solids
        # Sets keyword VTK_ROP_S(#,#)
        # DEFAULT value .FALSE.

        #Enable writing solids temperature
        # Requires TFM solids and ENERGY_EQ = .TRUE.
        # Sets keyword VTK_S_G(#,#)  # VTK_T_S
        # DEFAULT value .FALSE.
        key = 'vtk_t_s'
        cb = ui.checkbox_keyword_vtk_t_s_args_VTK_P
        energy_eq = self.project.get_value('energy_eq', default=True)
        enabled = bool(energy_eq)
        cb.setEnabled(enabled)
        if not enabled:
            cb.setChecked(False)
            for V in self.vtk_current_indices:
                self.unset_keyword(key, args=[V, P])

        #Enable writing solids phase granular temperature
        # Requires TFM solids and KT_TYPE /= “ALGEBRAIC”
        # Sets keyword VTK_THETA_M(#,#)
        # DEFAULT value .FALSE.
        key = 'vtk_theta_m'
        cb = ui.checkbox_keyword_vtk_theta_m_args_VTK_P
        kt_type = self.project.get_value('kt_type', default=DEFAULT_KT_TYPE)
        enabled = (kt_type != 'ALGEBRAIC')
        cb.setEnabled(enabled)
        if not enabled:
            cb.setChecked(False)
            for V in self.vtk_current_indices:
                self.unset_keyword(key, args=[V, P])

        for key in ('vtk_vel_s', 'vtk_u_s', 'vtk_v_s', 'vtk_w_s',
                    'vtk_rop_s', 'vtk_t_s', 'vtk_theta_m'):
            cb = getattr(ui, 'checkbox_keyword_%s_args_VTK_P'%key)
            val = self.project.get_value(key, default=False, args=[V0, P])
            cb.setChecked(bool(val))

        layout = ui.groupbox_cell_solids.layout()

        #Enable writing solids phase M, species N
        # Requires TFM solids and SPECIES_EQ(#) = .TRUE.
        # Sets keyword VTK_X_S(#,M,N)
        # DEFAULT value .FALSE.
        key = 'vtk_x_s'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        solids_species_names = list(self.solids_species.get(P, {}).keys())
        if self.output_saved_solids_species_names != solids_species_names:
            self.output_saved_solids_species_names = solids_species_names
            n_solids_species = len(solids_species_names)
            if len(cbs) > n_solids_species:
                for (i, cb) in enumerate(cbs[n_solids_species:], 1+n_solids_species):
                    for V in self.vtk_current_indices:
                        self.unset_keyword(key, args=[V, P, i])
                    layout.removeWidget(cb)
                    cb.setParent(None)
                    cb.deleteLater()
                ui.dynamic_widgets[key] = cbs = cbs[:n_solids_species]

            while len(cbs) < n_solids_species:
                n = 1+len(cbs)
                cb = CheckBox('%s mass fraction' % solids_species_names[n-1])
                cb.key = key
                cb.args = ['VTK', 'P', n]
                cb.value_updated.connect(self.project.submit_change)
                cbs.append(cb)
                self.add_tooltip(cb, key=cb.key)
                layout.addWidget(cb)
            for (cb, name) in zip(cbs, solids_species_names):
                cb.setText(name)

        # Set checkboxes to correct state
        species_eq = self.project.get_value('species_eq', default=True, args=[P])
        enable = bool(species_eq)
        for (i, cb) in enumerate(cbs, 1):
            cb.setEnabled(enable)
            if enable:
                val = self.project.get_value(key, args=[V0, P, i], default=False)
                cb.setChecked(bool(val))
            else:
                cb.setChecked(False)
                for V in self.vtk_current_indices:
                    self.unset_keyword(key, args=[V, P, i])



    def setup_output_scalar_subtab(self):
        #Scalar (tab?)
        # Note, this is nearly identical to output_reactions_tab
        ui = self.ui.output
        V0 = self.vtk_current_indices[0] if self.vtk_current_indices else None
        layout = ui.groupbox_cell_scalar.layout()
        #Enable writing user defined scalar
        # Requires NSCALAR > 0
        # Sets keyword VTK_SCALAR(#, #) # requires Scalar index
        # DEFAULT value .FALSE.
        key = 'vtk_scalar'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        nscalar = self.project.get_value('nscalar', default=0)
        # Remove extra widgets if number decreased
        if len(cbs) > nscalar:
            for (i, cb) in enumerate(cbs[nscalar:], 1+nscalar):
                for V in self.vtk_current_indices:
                    self.unset_keyword(key, args=[V, i])
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:nscalar]
        while len(cbs) < nscalar:
            n = 1+len(cbs)
            cb = CheckBox("Scalar %s" % n)
            cb.key = key
            cb.args = ['VTK', n]
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            layout.addWidget(cb)

        # Set checkboxes to correct state
        for (i, cb) in enumerate(cbs, 1):
            val = False if V0 is None else self.project.get_value(key, args=[V0, i], default=False)
            cb.setChecked(bool(val))


    def setup_output_other_subtab(self):
        #Other (tab?)
        ui = self.ui.output

        if not self.vtk_current_indices:
            return
        V0 = self.vtk_current_indices[0]

        #Enable writing vorticity
        # Requires fluid solver (RO_G0 /= 0.0)
        # Sets keyword VTK_VORTICITY (#)
        # Sets keyword VTK_LAMBDA_2(#)
        # DEFAULT value .FALSE.
        keys = ['vtk_vorticity', 'vtk_lambda_2']
        cb = ui.checkbox_keyword_vtk_vorticity_args_VTK
        enabled = not self.fluid_solver_disabled
        cb.setEnabled(enabled)
        if not enabled:
            cb.setChecked(False)
            for V in self.vtk_current_indices:
                for k in keys:
                    self.unset_keyword(k, args=[V])
        else:
            val = self.project.get_value(keys[0], default=False, args=[V0])
            cb.setChecked(bool(val))

        #Enable writing partition
        # Sets keyword VTK_PARTITION(#)
        # DEFAULT value .FALSE.

        #Enable writing boundary ID
        # Sets keyword VTK_BC_ID(#)
        # DEFAULT value .FALSE.

        #Enable writing wall distance
        # Sets keyword VTK_DWALL(#)
        # DEFAULT value .FALSE.

        #Enable writing cell index
        # Sets keyword VTK_IJK(#)
        # DEFAULT value .FALSE.
        for key in ('vtk_partition', 'vtk_bc_id',
                    'vtk_dwall', 'vtk_ijk'):
            cb = getattr(ui, 'checkbox_keyword_%s_args_VTK'%key)
            val = self.project.get_value(key, default=False, args=[V0])
            cb.setChecked(bool(val))


    def set_vtk_lambda_2(self):
        #vtk_lambda_2 follows setting of vtk_vorticity
        if not self.vtk_current_indices:
            return
        V0 = self.vtk_current_indices[0]
        val = self.project.get_value('vtk_vorticity', args=[V0])
        for V in self.vtk_current_indices:
            self.update_keyword('vtk_lambda_2', val, args=[V])


    def setup_output_vtk_particle(self):
        """Particle data sub-pane There is a need for some hand waving here. Many
        mfix.dat files may use a different specification for VTK input. There
        will need to be a way of catching the 'old' format and converting it to
        this input style.
        """

        # Note 1, filebase through nzs are common to cell/particle
        # Note 2, this groupbox is disabled completely if not DEM or PIC
        ui = self.ui.output
        ui.stackedwidget_cell_particle.setCurrentIndex(PAGE_PARTICLE)

        indices = self.vtk_current_indices
        if not indices:
            # Clear inputs?  should have been done in handle_selection
            return
        V0 = indices[0]

        # SRS update June 2017 add VTK_SELECT_MODE
        key = 'vtk_select_mode'
        vtk_select_mode = self.project.get_value(key, args=[V0], default='C')
        if vtk_select_mode not in ['C', 'P', 'I']:
            self.warn("Invalid vtk_select_mode %s" % vtk_select_mode)
            vtk_select_mode = 'C'
        cb = ui.combobox_vtk_select_mode
        index = ['C', 'P', 'I'].index(vtk_select_mode)
        cb.setCurrentIndex(index)
        cb.setToolTip(get_combobox_item(cb,index).toolTip())

        # NB The following all require PIC or DEM solids and default to False
        #
        #Enable writing particle diameter
        # Sets keyword VTK_PART_DIAMETER(#)

        #Enable writing particle translational velocity
        # Sets keyword VTK_PART_VEL(#)

        #Enable writing particle rotational velocity
        # Sets keyword VTK_PART_ANGULAR_VEL

        #Enable writing particle orientation
        # Sets keyword VTK_PART_ORIENTATION

        #Enable writing particle temperature
        # Requires and ENERGY_EQ=.TRUE.
        # Sets keyword VTK_PART_TEMP(#)

        #Enable writing particle density
        # Sets keyword VTK_PART_DENSITY(#)

        #Enable writing particle cohesive force
        # Requires USE_COHESION=.TRUE.
        # Sets keyword VTK_PART_COHESION(#)

        #Enable writing particle user variable
        # Requires DES_USR_VAR > 0
        # Sets keyword VTK_PART_USR_VAR(#,#)
        # (handled below)

        #Enable writing particle species composition
        # (below other controls)

        #Enable writing particle MPI rank
        # Sets keyword VTK_PART_RANK(#)

        #Enable writing particle global ID
        # Sets keyword VTK_PART_ID(#)

        for key in ('vtk_part_diameter',
                    'vtk_part_vel',
                    'vtk_part_angular_vel',
                    'vtk_part_orientation',
                    'vtk_part_temp',
                    'vtk_part_density',
                    'vtk_part_cohesion',
                    'vtk_part_rank',
                    'vtk_part_id'):
            cb = getattr(ui, 'checkbox_keyword_%s_args_VTK' % key)
            val = self.project.get_value(key, args=[V0], default=False)
            cb.setChecked(val)

        # disable temp if energy_eq disabled
        cb = ui.checkbox_keyword_vtk_part_temp_args_VTK
        key = 'vtk_part_temp'
        enabled = self.project.get_value('energy_eq', default=True)
        cb.setEnabled(enabled)
        if not enabled:
            cb.setChecked(False)
            for V in self.vtk_current_indices:
                self.unset_keyword(key, args=[V])

        # disable cohesion if use_cohesion disabled
        cb = ui.checkbox_keyword_vtk_part_cohesion_args_VTK
        key = 'vtk_part_cohesion'
        enabled = self.project.get_value('use_cohesion', default=False)
        cb.setEnabled(enabled)
        if not enabled:
            cb.setChecked(False)
            for V in self.vtk_current_indices:
                self.unset_keyword(key, args=[V])


        layout = ui.groupbox_particle_data.layout()

        # SRS update Jul 2017
        #Enable writing per-phase particle data
        # Requires DEM solids
        # Sets keyword VTK_PART_PHASE(#,#)
        # DEFAULT value .TRUE.
        key = 'vtk_part_phase'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        # Remove extra widgets if number decreased
        n_solids = len(self.solids)
        solids_names = list(self.solids.keys())
        if len(cbs) > n_solids:
            for (i, cb) in enumerate(cbs[n_solids:], 1+n_solids):
                for V in self.vtk_current_indices:
                    self.unset_keyword(key, args=[V, i])
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:n_solids]

        while len(cbs) < n_solids:
            n = len(cbs) + 1
            cb = CheckBox(solids_names[n-1])
            cb.key = key
            cb.args = ['VTK', n]
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            layout.addWidget(cb)
        # Set checkboxes to correct state
        for (i, cb) in enumerate(cbs, 1):
            cb.setText(solids_names[i-1])
            val = self.project.get_value(key, args=[V0, i], default=True)
            cb.setChecked(bool(val))


        #Enable writing particle user variable
        # Requires DEM or PIC solids and DES_USR_VAR_SIZE > 0
        # Sets keyword VTK_PART_USR_VAR(#,#)
        # DEFAULT value .FALSE.
        key = 'vtk_part_usr_var'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        des_usr_var_size = self.project.get_value('des_usr_var_size', default=0)
        # Remove extra widgets if number decreased
        if len(cbs) > des_usr_var_size:
            for (i, cb) in enumerate(cbs[des_usr_var_size:], 1+des_usr_var_size):
                for V in self.vtk_current_indices:
                    self.unset_keyword(key, args=[V, i])
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:des_usr_var_size]

        while len(cbs) < des_usr_var_size:
            n = 1+len(cbs)
            cb = CheckBox("DES user scalar %s" % n)
            cb.key = key
            cb.args = ['VTK', n]
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            layout.addWidget(cb)
        # Set checkboxes to correct state
        for (i, cb) in enumerate(cbs, 1):
            val = self.project.get_value(key, args=[V0, i], default=False)
            cb.setChecked(bool(val))

        #Enable writing particle species composition
        # Requires DEM or PIC solids and any SPECIES_EQ=.TRUE.
        # Sets keyword VTK_PART_X_S(#,N)
        # NOTE, VTK_PART_X_S(#,N) where N ranges from 1 to max(nmax_s)
        # DEFAULT value .FALSE.
        key = 'vtk_part_x_s'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        enabled = any(self.project.get_value('species_eq', args=[P])
                      for P in range(1, len(self.solids)+1))
        max_n = 0 if not enabled else max(self.project.get_value('nmax_s', args=[N], default=0)
                                          for N in range(1, len(self.solids)+1))
        # Remove extra widgets if number decreased
        if len(cbs) > max_n:
            for (i, cb) in enumerate(cbs[max_n:], 1+max_n):
                for V in self.vtk_current_indices:
                    self.unset_keyword(key, args=[V, i])
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:max_n]

        while len(cbs) < max_n:
            n = 1+len(cbs)
            cb = CheckBox("Species %s mass fraction" % n)
            cb.key = key
            cb.args = ['VTK', n]
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            layout.addWidget(cb)
        # Set checkboxes to correct state
        for (i, cb) in enumerate(cbs, 1):
            val = self.project.get_value(key, args=[V0, i], default=False)
            cb.setChecked(bool(val))

    def output_set_region_keys(self, name, idx, data, output_type=None):
        # Update the keys which define the region the vtk output applies to
        if output_type is not None:
            self.update_keyword('vtk_data', output_type, args=[idx])

        no_k = self.project.get_value('no_k')
        for (key, val) in zip(('x_w', 'y_s', 'z_b',
                               'x_e', 'y_n', 'z_t'),
                              data['from']+data['to']):
            # vtk_z_t and vtk_z_b keywords should not be added when no_k=True
            if no_k and key in ('z_t', 'z_b'):
                continue
            self.update_keyword('vtk_'+key, val, args=[idx])


    def output_check_region_in_use(self, name):
        return any(data.get('region') == name for data in self.outputs.values())


    def output_update_region(self, name, data):
        for (i, output) in self.outputs.items():
            if output.get('region') == name:
                self.output_set_region_keys(name, i, data)


    def output_change_region_name(self, old_name, new_name):
        ui = self.ui.output
        for (key, val) in self.outputs.items():
            if val.get('region') == old_name:
                self.outputs[key]['region'] = new_name
                tw = ui.tablewidget_regions
                for i in range(tw.rowCount()):
                    data = tw.item(i, 0).data(UserRole)
                    indices, names = data
                    if key in indices:
                        item = tw.item(i, COLUMN_REGION)
                        new_names = [new_name if n == old_name else n for n in names]
                        item.setData(UserRole, (indices, new_names))
                        item.setText('+'.join(new_names))
                        # Also update vtk_filebase, if it is at the default setting
                        vtk_filebase = self.project.get_value('vtk_filebase', args=[key],
                                                              default='')
                        default_name = '+'.join(names)
                        if vtk_filebase and (vtk_filebase==default_name
                                             or vtk_filebase.startswith(default_name+'_')):
                            vtk_filebase = self.output_default_vtk_filebase(new_names)
                            for i in indices:
                                self.update_keyword('vtk_filebase', vtk_filebase, args=[i])
                            tw.item(i, COLUMN_FILENAME).setText(vtk_filebase)

    def reset_output(self):
        ui = self.ui.output
        # Set all output-related state back to default
        ui.pushbutton_vtk.setEnabled(False)
        self.output_change_tab(BASIC_TAB, ui.pushbutton_basic)
        self.outputs.clear()
        self.vtk_current_indices = []
        self.vtk_current_regions = []
        self.output_region_dict = None
        self.vtk_current_solid = self.P = None
        ui.tablewidget_regions.clearContents()
        ui.tablewidget_regions.setRowCount(0)
        self.output_current_tab = BASIC_TAB
        ui.stackedwidget_output.setCurrentIndex(BASIC_TAB)
        ui.stackedwidget_cell_particle.setCurrentIndex(PAGE_CELL)
        self.output_current_subtab = FLUID_TAB
        ui.stackedwidget_cell.setCurrentIndex(FLUID_TAB)
        # TODO remove dynamically created input widgets, although this should
        #  get handled next time we call 'setup'
