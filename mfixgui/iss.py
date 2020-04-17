# -*- coding: utf-8 -*-
""" Internal surfaces pane"""

from json import JSONDecoder, JSONEncoder

from qtpy.QtCore import Qt
UserRole = Qt.UserRole

from qtpy.QtWidgets import QHeaderView, QTableWidgetItem, QLabel

from mfixgui.constants import *

from mfixgui.widgets.base import LineEdit

from mfixgui.tools.qt import (get_combobox_item, get_selected_row,
                           set_item_noedit, sub_icon_height, widget_iter)

from mfixgui.tools.keyword_args import mkargs

class ISS(object):
    #Internal Surfaces Task Pane Window: This section allows a user to
    #define internal surfaces for the described model. This section
    #relies on regions named in the Regions section.

    def init_iss(self):
        ui = self.ui.internal_surfaces

        self.iss = {} # key: index.  value: data dictionary for internal surface
        self.iss_current_indices = [] # List of IS indices
        self.iss_current_regions = [] # And the names of the regions which define them
        self.iss_region_dict = None
        self.iss_saved_solids_names = []

        #  The top of the task pane is where users define/select IS regions.
        #  Icons to add/remove/duplicate internal surfaces are given at the
        #  top. Clicking the 'add' and 'duplicate' buttons triggers a popup
        #  window where the user must select a region to apply the internal
        #  surface.

        ui.toolbutton_add.clicked.connect(self.iss_show_regions_popup)
        ui.toolbutton_delete.clicked.connect(self.iss_delete_regions)
        ui.toolbutton_delete.setEnabled(False) # Need a selection

        ui.tablewidget_regions.itemSelectionChanged.connect(self.handle_iss_region_selection)

        ui.combobox_is_type.activated.connect(self.change_is_type)

        for widget in widget_iter(ui):
            if isinstance(widget, LineEdit) and widget.dtype in (int, float):
                widget.allow_parameters = True

    def change_is_type(self, idx):
        if not self.iss_current_indices:
            return
        ui = self.ui.internal_surfaces
        row = get_selected_row(ui.tablewidget_regions)
        if row is None:
            return
        new_name = IS_NAMES[idx]
        new_type = IS_TYPES[idx]
        item = QTableWidgetItem(new_name)
        set_item_noedit(item)
        ui.tablewidget_regions.setItem(row, 1, item)
        for IS in self.iss_current_indices:
            self.update_keyword('is_type', new_type, args=[IS])
            for kw in list(self.project.keywordItems()):
                if kw.key.startswith('is_') and kw.args and kw.args[0] == IS:
                    if kw.key not in ('is_type',
                                      'is_x_e', 'is_x_w',
                                      'is_y_s', 'is_y_n',
                                      'is_z_b', 'is_z_t'):
                        self.unset_keyword(kw.key, args=kw.args)
        self.iss_setup_current_tab()


    def iss_show_regions_popup(self):
        #Select internal surface type
        # Selection is required
        # (selection logic implemented in regions_popup.py)

        ui = self.ui.internal_surfaces
        rp = self.regions_popup
        rp.clear()
        for (name, data) in self.iss_region_dict.items():
            shape = data.get('type', '---')
            # Assume available if unmarked

            # Users cannot select inapplicable regions.
            # IS regions can be planes or volumes (not points or STLs)
            # No region can define more than one internal surface.

            available = (data.get('available', True)
                         and (shape == 'box'
                              or 'plane' in shape))
            row = (name, shape, available)
            rp.add_row(row)
        rp.reset_signals()
        rp.save.connect(self.iss_add_regions)
        rp.cancel.connect(self.iss_cancel_add)
        for item in (ui.tablewidget_regions,
                     ui.bottom_frame,
                     ui.toolbutton_add,
                     ui.toolbutton_delete):
            item.setEnabled(False)
        rp.popup('Select region(s) for internal surface')


    def iss_cancel_add(self):
        ui = self.ui.internal_surfaces

        for item in (ui.toolbutton_add,
                     ui.tablewidget_regions):
            item.setEnabled(True)

        if get_selected_row(ui.tablewidget_regions) is not None:
            for item in (ui.bottom_frame,
                         ui.toolbutton_delete):
                item.setEnabled(True)


    def iss_add_regions(self):
        # Interactively add regions to define ISs
        rp = self.regions_popup
        self.iss_cancel_add() # Reenable input widgets
        selections = rp.get_selection_list()
        if not selections:
            return
        is_type = IS_TYPES[rp.combobox.currentIndex()]
        self.iss_add_regions_1(selections, is_type=is_type, indices=None, autoselect=True)
        self.iss_setup_current_tab() # Update the widgets


    def iss_add_regions_1(self, selections,
                          is_type=None, indices=None, autoselect=False):
        # Used by both interactive and load-time add-region handlers
        if is_type is None:
            self.error('Type not defined for internal surface %s' % '+'.join(selections))
            return

        ui = self.ui.internal_surfaces

        if self.iss_region_dict is None:
            self.iss_region_dict = self.ui.regions.get_region_dict()

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
                idx = self.iss_find_index()
                indices[i] = idx
            self.iss[idx] = {'region': region_name}
            region_data = self.iss_region_dict.get(region_name)
            if region_data is None: # ?
                self.warn("no data for region %s" % region_name)
                continue
            self.iss_set_region_keys(region_name, idx, region_data, is_type)
            self.iss_region_dict[region_name]['available'] = False # Mark as in-use
        item.setData(UserRole, (tuple(indices), tuple(selections)))
        tw.setItem(nrows, 0, item)

        item = make_item(IS_NAMES[IS_TYPES.index(is_type)])
        tw.setItem(nrows, 1, item)

        self.fixup_iss_table(tw)

        if autoselect:
            tw.setCurrentCell(nrows, 0)

        # Issues/513
        for i in indices:
            self.iss_set_default_keys(i)


    def iss_set_default_keys(self, IS):
        #*Input is only needed for semi-permeable surfaces.*
        is_type = self.project.get_value('is_type', args=[IS])
        if is_type.endswith('SEMIPERMEABLE'): # matches multiple types
            #-  Gas permeability:
            #-  DEFAULT value 1.0d32
            #-  Sets keyword IS_PC(#,1)
            key = 'is_pc'
            default = 1.0e32
            self.set_keyword_default(key, default, args=[IS,1])
            #-  Internal resistance coefficient:
            #-  DEFAULT value 0.0
            #-  Sets keyword IS_PC(#,2)
            key = 'is_pc'
            default = 0.0
            self.set_keyword_default(key, default, args=[IS,2])
            #-  Solids velocity through surface:
            #-  DEFAULT value 0.0
            #-  Sets keyword IS_VEL_s(#,PHASE)
            key = 'is_vel_s'
            default = 0.0
            for P in range(1, 1+len(self.solids)):
                self.set_keyword_default(key, default, args=[IS,P])


    def iss_find_index(self):
        # Always add new IS at end
        return 1 if not self.iss else 1 + max(self.iss)


    def iss_delete_regions(self):
        ui = self.ui.internal_surfaces
        tw = ui.tablewidget_regions
        row = get_selected_row(tw)
        if row is None: # No selection
            return

        # Unset keywords
        kwlist = list(self.project.keywordItems())
        for kw in kwlist:
            key, args = kw.key, kw.args
            if key.startswith('is_') and args and args[0] in self.iss_current_indices:
                self.unset_keyword(key, args=args)

        for r in self.iss_current_regions:
            if r in self.iss_region_dict:
                self.iss_region_dict[r]['available'] = True

        for i in self.iss_current_indices:
            del self.iss[i]

        self.iss_current_regions = []
        self.iss_current_indices = []

        tw.removeRow(row)
        self.fixup_iss_table(tw)
        self.iss_setup_current_tab()
        self.update_nav_tree()


    def iss_delete_solids_phase(self, phase_index):
        """adjust iss_current_solid when solids phase deleted"""
        if (self.iss_current_solid is not None
                and self.iss_current_solid >= phase_index):
            self.iss_current_solid -= 1
            if self.iss_current_solid == 0:
                self.iss_current_solid = None


    def handle_iss_region_selection(self):
        ui = self.ui.internal_surfaces
        table = ui.tablewidget_regions
        row = get_selected_row(table)
        if row is None:
            indices = []
            regions = []
        else:
            (indices, regions) = table.item(row, 0).data(UserRole)
        self.iss_current_indices, self.iss_current_regions = indices, regions
        enabled = (row is not None)
        for item in (ui.toolbutton_delete,
                     ui.bottom_frame):
            item.setEnabled(enabled)
        ui.bottom_frame.setEnabled(enabled)
        if not enabled:
            # Clear
            for widget in widget_iter(ui.bottom_frame):
                if isinstance(widget, LineEdit):
                    widget.setText('')
            return
        IS0 = self.iss_current_indices[0]
        cb = ui.combobox_is_type
        key = 'is_type'
        is_type = self.project.get_value(key, args=[IS0])
        if is_type not in IS_TYPES:
            self.error("Unknown IS_TYPE %s" % is_type)
            is_type = 'IMPERMEABLE'
            for IS in self.iss_current_indices:
                self.update_keyword(key, is_type, args=[IS])
        cb.setCurrentIndex(IS_TYPES.index(is_type))

        # Available selections:
        #  Impermeable
        #    Selection only available for plane regions
        #    set keyword IS_TYPE(#) to 'IMPERMEABLE'
        #  X-Axis Impermeable
        #    Selection only available for volume regions
        #    set keyword IS_TYPE(#) to 'X_IMPERMEABLE'
        #  Y-Axis Impermeable
        #    Selection only available for volume regions
        #    set keyword IS_TYPE(#) to 'Y_IMPERMEABLE'
        #  z-Axis Impermeable
        #    Selection only available for volume regions
        #    set keyword IS_TYPE(#) to 'Z_IMPERMEABLE'
        #  Semi-permeable
        #    Selection only available for plane regions
        #    set keyword IS_TYPE(#) to 'SEMIPERMEABLE'
        #  X-Axis semi-permeable
        #    Selection only available for volume regions
        #    set keyword IS_TYPE(#) to 'X_SEMIPERMEABLE'
        #  Y-Axis semi-permeable
        #    Selection only available for volume regions
        #    set keyword IS_TYPE(#) to 'Y_SEMIPERMEABLE'
        #  Z-Axis semi-permeable
        #    Selection only available for volume regions
        #    set keyword IS_TYPE(#) to 'Z_SEMIPERMEABLE'
        # DEFAULT - Impermeable

        plane = is_type in ('IMPERMEABLE', 'SEMIPERMEABLE')
        vol = not plane
        for (i, enable) in enumerate((plane, vol, vol, vol, plane, vol, vol, vol)):
            get_combobox_item(cb, i).setEnabled(enable)

        self.setup_iss() # reinitialize all widgets in current tab
        ui.scrollarea_detail.ensureVisible(0, 0) # scroll to top



    def fixup_iss_table(self, tw, stretch_column=0):
        ui = self.ui.internal_surfaces
        hv = QHeaderView
        resize = tw.horizontalHeader().setSectionResizeMode
        ncols = tw.columnCount()
        for n in range(0, ncols):
            resize(n, hv.Stretch if n == stretch_column else hv.ResizeToContents)

        # trim excess vertical space - can't figure out how to do this in designer
        header_height = tw.horizontalHeader().height()

        # Note - scrollbar status can change outside of this function.
        # Do we need to call this everytime window geometry changes?
        scrollbar_height = (tw.horizontalScrollBar().isVisible()
                            * (4+tw.horizontalScrollBar().height()))
        nrows = tw.rowCount()
        if nrows == 0:
            row_height = 0
            height = header_height+scrollbar_height
        else:
            row_height = tw.rowHeight(0)
            height = (header_height+scrollbar_height
                      + nrows*row_height + 4) # extra to avoid unneeded scrollbar

        if tw == ui.tablewidget_regions: # main table, adjust top splitter
            icon_height = sub_icon_height() + 8
            ui.top_frame.setMaximumHeight(height+icon_height)
            ui.top_frame.setMinimumHeight(header_height+icon_height+row_height*min(nrows, 5))
            ui.top_frame.updateGeometry()
            tw.setMaximumHeight(height)
            tw.setMinimumHeight(header_height)
        else: # mass fraction tables (none for ISS)
            tw.setMaximumHeight(height) # Works for tablewidget inside groupbox
            tw.setMinimumHeight(height) #? needed? should we allow scrollbar?
        tw.updateGeometry() #? needed?


    def iss_update_enabled(self):
        if self.iss:
            # Never disable if there are ISs defined
            disabled = False
        else:
            # If there are no solids, (no scalar equations), and the fluid solver is disabled,
            # then we have no input tabs on the ISs pane, so disable it completely
            regions = self.ui.regions.get_region_dict()
            nregions = sum(1 for (name, r) in regions.items()
                           if (r.get('type') == 'box'
                               or 'plane' in r.get('type')))
            disabled = (nregions == 0
                        or (self.fluid_solver_disabled
                            and len(self.solids) == 0))
        self.find_navigation_tree_item("Internal Surfaces").setDisabled(disabled)


    def iss_set_region_keys(self, name, idx, data, is_type=None):
        # Update the keys which define the region the IS applies to
        if is_type is not None:
            self.update_keyword('is_type', is_type, args=[idx])

        no_k = self.project.get_value('no_k')
        for (key, val) in zip(('x_w', 'y_s', 'z_b',
                               'x_e', 'y_n', 'z_t'),
                              data['from']+data['to']):
            # is_z_t and is_z_b keywords should not be added when no_k=True
            if no_k and key in ('z_t', 'z_b'):
                continue
            self.update_keyword('is_'+key, val, args=[idx])


    def iss_change_region_name(self, old_name, new_name):
        ui = self.ui.internal_surfaces
        for (key, val) in self.iss.items():
            if val.get('region') == old_name:
                self.iss[key]['region'] = new_name
                tw = ui.tablewidget_regions
                for i in range(tw.rowCount()):
                    data = tw.item(i, 0).data(UserRole)
                    indices, names = data
                    if key in indices:
                        item = tw.item(i, 0)
                        names = [new_name if n == old_name else n for n in names]
                        item.setData(UserRole, (tuple(indices), tuple(names)))
                        item.setText('+'.join(names))
                        break
                break


    def reset_iss(self):
        self.iss.clear()
        self.iss_current_indices = []
        self.iss_current_regions = []
        self.iss_region_dict = None
        self.iss_current_solid = self.P = None
        ui = self.ui.internal_surfaces
        ui.tablewidget_regions.clearContents()
        ui.tablewidget_regions.setRowCount(0)
        # anything else to do here?
        # TODO remove dynamically created input widgets, although this should
        #  get handled next time we call 'setup'

    def is_regions_to_str(self):
        ui = self.ui.internal_surfaces
        tw = ui.tablewidget_regions
        data = [tw.item(i, 0).data(UserRole)
                for i in range(tw.rowCount())]
        return JSONEncoder().encode(data)


    def iss_regions_from_str(self, s):
        if not s:
            return
        data = JSONDecoder().decode(s)
        for (indices, regions) in data:
            if not indices:
                continue # should not get empty tuple
            # is_type should already be set
            is_type = self.project.get_value('is_type', args=[indices[0]])
            self.iss_add_regions_1(regions, is_type=is_type, indices=indices, autoselect=False)


    def iss_extract_regions(self):
        if self.iss:
            # We assume that IS regions have been initialized correctly
            # from mfix_gui_comments.
            # TODO: verify that there is an IS region for each IS
            return

        if self.iss_region_dict is None:
            self.iss_region_dict = self.ui.regions.get_region_dict()

        for is_ in self.project.iss:
            d = is_.keyword_dict
            extent = [d.get('is_'+k, None) for k in ('x_w', 'y_s', 'z_b',
                                                     'x_e', 'y_n', 'z_t')]
            extent = [0 if x is None else x.value for x in extent]
            #if any (x is None for x in extent):
            #    self.warn("internal surface %s: invalid extents %s" %
            #               (is_.ind, extent))
            #    continue

            is_type = d.get('is_type')
            if is_type is None:
                self.error("No type for internal surface %s" % is_.ind)
                continue
            is_type = is_type.value #

            for (region_name, data) in self.iss_region_dict.items():
                ext2 = [0 if x is None else x for x in
                        (data.get('from', []) + data.get('to', []))]
                if ext2 == extent:
                    if data.get('available', True):
                        self.iss_add_regions_1([region_name], is_type=is_type,
                                               indices=[is_.ind], autoselect=False)
                        break
            else:
                self.warn("internal surface %s: could not match defined region %s" %
                          (is_.ind, extent))
                kwlist = list(self.project.keywordItems())
                for kw in kwlist:
                    key, args = kw.key, kw.args
                    if key.startswith('is_') and args and args[0]==is_.ind:
                        self.unset_keyword(key, args=args)


    def iss_check_region_in_use(self, name):
        return any(data.get('region') == name for data in self.iss.values())


    def iss_update_region(self, name, data):
        for (i, is_) in self.iss.items():
            if is_.get('region') == name:
                self.iss_set_region_keys(name, i, data)


    def setup_iss(self):
        ui = self.ui.internal_surfaces

        # Grab a fresh copy, may have been updated
        self.iss_region_dict = self.ui.regions.get_region_dict()

        # Mark regions which are in use (this gets reset each time we get here)
        for (i, data) in self.iss.items():
            region = data['region']
            if region in self.iss_region_dict:
                self.iss_region_dict[region]['available'] = False

        self.fixup_iss_table(ui.tablewidget_regions)
        row = get_selected_row(ui.tablewidget_regions)
        # Autoselect if only 1 row
        if row is None and ui.tablewidget_regions.rowCount() == 1:
            row = 0
            ui.tablewidget_regions.setCurrentCell(row, 0)
        enabled = (row is not None)
        for item in (ui.toolbutton_delete,
                     ui.bottom_frame):
            item.setEnabled(enabled)

        solids_names = list(self.solids.keys())
        # There should be a line for each solids phase. Use the user provided solids name.
        if not self.solids:
            ui.groupbox_solids_velocities.hide()
        else:
            if self.iss_saved_solids_names != solids_names:
                # Clear out the old ones
                for w in widget_iter(ui.groupbox_solids_velocities):
                    if not isinstance(w, (LineEdit, QLabel)):
                        continue
                    self.project.unregister_widget(w)
                    w.setParent(None)
                    w.deleteLater()
                # make new ones
                layout = ui.groupbox_solids_velocities.layout()
                row = 0
                key = 'is_vel_s'
                for phase, solid_name in enumerate(self.solids.keys(), 1):
                    label = QLabel(solid_name)
                    self.add_tooltip(label, key)
                    layout.addWidget(label, row, 0)
                    le = LineEdit()
                    le.key = key
                    le.args = ['IS', phase]
                    le.dtype = float
                    self.project.register_widget(le, key, le.args)
                    self.add_tooltip(le, key)
                    layout.addWidget(le, row, 1)
                    units_label = QLabel('m/s')
                    layout.addWidget(units_label, row, 2)
                    row += 1
            ui.groupbox_solids_velocities.show()

        self.iss_saved_solids_names = solids_names
        self.iss_setup_current_tab()


    def iss_setup_current_tab(self):
        ui = self.ui.internal_surfaces
        if not self.iss_current_indices:
            return
        IS0 = self.iss_current_indices[0]

        #Input is only needed for semi-permeable surfaces.
        is_type = self.project.get_value('is_type', args=[IS0])
        enabled = 'SEMI' in is_type

        #Gas permeability:
        # Specification only available for semipermeable regions
        # DEFAULT value 1.0d32
        # Sets keyword IS_PC(#,1)
        key = 'is_pc'
        for widget in ui.label_is_pc_1, ui.label_is_pc_1_units, ui.lineedit_keyword_is_pc_args_IS_1:
            widget.setEnabled(enabled)
        default = 1.0e32 if enabled else None
        args = [IS0, 1]
        val = self.project.get_value(key, args=args)
        if val is None:
            val = default
        ui.lineedit_keyword_is_pc_args_IS_1.updateValue(key, val, args)

        #Internal resistance coefficient:
        # Specification only available for semipermeable regions
        # DEFAULT value 0.0
        # Sets keyword IS_PC(#,2)
        key = 'is_pc'
        for widget in ui.label_is_pc_2, ui.label_is_pc_2_units, ui.lineedit_keyword_is_pc_args_IS_2:
            widget.setEnabled(enabled)
        default = 0.0 if enabled else None
        args = [IS0, 2]
        val = self.project.get_value(key, args=args)
        if val is None:
            val = default
        ui.lineedit_keyword_is_pc_args_IS_2.updateValue(key, val, args)

        #Solids velocity through surface:
        # Specification only available for semipermeable regions

        # enable/disable entire groupbox, don't have to do widgets
        ui.groupbox_solids_velocities.setEnabled(enabled)

        # DEFAULT value 0.0
        # Sets keyword IS_VEL_s(#,PHASE)
        default = 0.0 if enabled else None
        key = 'is_vel_s'
        for le in widget_iter(ui.groupbox_solids_velocities):
            if not isinstance(le, LineEdit):
                continue
            args = mkargs(key, is_=IS0, phase=le.args[1])
            val = self.project.get_value(key, args=args)
            if val is None:
                val = default
            le.updateValue(key, val, args)
