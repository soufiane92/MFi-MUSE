# -*- coding: utf-8 -*-
"""Monitors pane"""
from qtpy import QtCore
from qtpy.QtWidgets import (QHeaderView, QPushButton,
                            QTableWidgetItem, QWidget)

from qtpy.QtGui import QPicture
UserRole = QtCore.Qt.UserRole

from mfixgui.widgets.base import CheckBox

from mfixgui.tools.qt import (set_item_noedit, get_combobox_item,
                              get_selected_row, sub_icon_height)

from mfixgui.tools.keyword_args import mkargs, keyword_args
from mfixgui.constants import *

# We don't need extended JSON here
from json import JSONDecoder, JSONEncoder

(FLUID_TAB, SOLIDS_TAB_DUMMY_L, SOLIDS_TAB, SOLIDS_TAB_DUMMY_R,
 SCALAR_TAB, REACTIONS_TAB, PHASE_TAB) = range(7) # bottom tabset

# Columns in tablewidget_regions
COLUMN_REGION, COLUMN_FILENAME, COLUMN_ID = range(3)

class Monitors(object):
    # Monitors task pane window:

    def init_monitors(self):
        ui = self.ui.monitors

        self.monitors = {} # key: index.  value: data dictionary for monitor
        self.monitors_current_index = None
        self.monitors_current_region = None # And the names of the regions which define them
        self.monitors_region_dict = None
        self.monitors_saved_solids_names = []
        ui.dynamic_widgets = {}
        ui.toolbutton_add.clicked.connect(self.monitors_show_regions_popup)
        ui.toolbutton_delete.clicked.connect(self.monitors_delete_regions)
        for tb in (ui.toolbutton_delete,):
            tb.setEnabled(False) # Need a selection

        ui.tablewidget_regions.itemSelectionChanged.connect(self.handle_monitors_region_selection)

        self.monitors_current_tab = FLUID_TAB # If fluid is disabled, we will switch
        self.monitors_current_solid = self.P = None
        ui.pushbutton_fluid.pressed.connect(lambda: self.monitors_change_tab(FLUID_TAB,None))
        ui.pushbutton_scalar.pressed.connect(lambda: self.monitors_change_tab(SCALAR_TAB,None))
        ui.pushbutton_reactions.pressed.connect(lambda: self.monitors_change_tab(REACTIONS_TAB,None))
        # Phase tab is not user-selectable

        # Trim width of "Fluid", "Scalar" "Reactions" and "Phase", like we do for
        # dynamically-created "Solid #" buttons
        for b in (ui.pushbutton_fluid, ui.pushbutton_scalar,
                  ui.pushbutton_reactions, ui.pushbutton_phase):
            w = b.fontMetrics().boundingRect(b.text()).width() + 20
            b.setMaximumWidth(w)

        # Show groupbox without title  (title is empty string)
        height = ui.checkbox_keyword_monitor_ep_g_args_MONITOR.sizeHint().height()
        tweak = 3
        ui.stackedwidget_detail.setStyleSheet(
            # Fix gap where title would be, with negative padding.
            # This is somewhat questionable (i.e. a total hack)
            'QGroupBox {margin-top: %spx; padding-top: %spx }' % (tweak-height, height))

        cb = ui.combobox_monitor_type
        key = 'monitor_type'
        for i in range(len(cb)):
            self.add_tooltip(get_combobox_item(cb, i), key, value=i)
        cb.setToolTip(get_combobox_item(cb, cb.currentIndex()).toolTip())
        cb.activated.connect(self.handle_combobox_monitor_type)
        # Make the table update when user changed MONITOR_NAME
        le = ui.lineedit_keyword_monitor_name_args_MONITOR
        le.post_update = self.setup_monitors


    def handle_combobox_monitor_type(self, index):
        ui = self.ui.monitors
        cb = ui.combobox_monitor_type
        cb.setToolTip(get_combobox_item(cb,index).toolTip())
        mon = self.monitors_current_index
        if mon is None: # No selection
            return
        key = 'monitor_type'
        val = index
        prev_phase_required = self.monitor_requires_phase(mon)
        self.update_keyword(key, val, args=[mon])
        new_phase_required = self.monitor_requires_phase(mon)
        if prev_phase_required != new_phase_required:
            # Unset keys when 'phase_required' changes..
            kwlist = list(self.project.keywordItems())
            for kw in kwlist:
                key, args = kw.key, kw.args
                if key.startswith('monitor_') and args and args[0]==mon:
                    if key in ('monitor_x_w', 'monitor_y_s', 'monitor_z_b',
                               'monitor_x_e', 'monitor_y_n', 'monitor_z_t',
                               'monitor_name', 'monitor_type', 'monitor_dt'):
                        continue
                    self.unset_keyword(key, args=args)

        self.handle_monitors_region_selection() # Will update tabset & ensure valid tab


    def handle_monitor_p_star(self, widget, data, args):
        mon = self.monitors_current_index
        if mon is None:
            return
        key = 'monitor_p_star'
        prev_val = self.project.get_value(key, args=[mon])
        ignore, new_val = data.popitem()

        if len(self.solids) > 1:
            resp=self.message(text="Pressure setting applies to all solids phases\nAre you sure?",
                              buttons=['yes','no'],
                              default = 'no')
            if resp != 'yes': # Reject update, set lineedit back to previous value
                widget.updateValue(self.project.get_value(key, prev_val))
                return

        self.update_keyword(key, new_val, args=[mon])


    def monitors_show_regions_popup(self):
        #  Monitor regions can be points, planes, or volumes
        #  STL regions can not be used to defined a monitor region
        #  Multiple regions can not be combined to define a monitor
        #  Invalid regions can not be selected in the menu

        ui = self.ui.monitors
        rp = self.regions_popup
        rp.clear()

        for (name,data) in self.monitors_region_dict.items():
            shape = data.get('type', '---')
            # Assume available if unmarked
            available = (data.get('available', True)
                         and (shape in ('box', 'point')
                              or 'plane' in shape))
            row = (name, shape, available)
            rp.add_row(row)
        rp.reset_signals()
        rp.save.connect(self.monitors_add_regions)
        rp.cancel.connect(self.monitors_cancel_add)
        for item in (ui.tablewidget_regions,
                     ui.bottom_frame,
                     ui.toolbutton_add,
                     ui.toolbutton_delete):
            item.setEnabled(False)
        rp.popup('Select region for monitor')


    def monitors_cancel_add(self):
        ui = self.ui.monitors
        for item in (ui.toolbutton_add,
                     ui.tablewidget_regions):
            item.setEnabled(True)
        tw = ui.tablewidget_regions
        row = get_selected_row(tw)
        if row is not None:
            for item in (ui.bottom_frame,
                         ui.toolbutton_delete):
                item.setEnabled(True)


    def monitors_add_regions(self):
        # Interactively add regions to define Monitors
        ui = self.ui.monitors
        rp = self.regions_popup
        self.monitors_cancel_add() # Reenable input widgets
        selections = rp.get_selection_list()
        if not selections:
            return
        monitor_type = rp.combobox.currentIndex()
        self.monitors_add_regions_1(selections, monitor_type=monitor_type, autoselect=True)
        self.setup_monitors() # Update the widgets


    def monitors_add_regions_1(self, selections, monitor_type=None, index=None, autoselect=False):
        # Used by both interactive and load-time add-region handlers
        ui = self.ui.monitors

        if not selections:
            return
        if monitor_type is None:
            self.error("No type for monitor %s" % index)
            return
        if self.monitors_region_dict is None:
            self.monitors_region_dict = self.ui.regions.get_region_dict()

        tw = ui.tablewidget_regions
        nrows = tw.rowCount()
        tw.setRowCount(nrows+1)

        def make_item(val):
            item = QTableWidgetItem('' if val is None else str(val))
            set_item_noedit(item)
            return item

        region_name = selections[0]
        item = make_item(region_name)

        if index is None: # interactive
            index = self.monitors_find_index()

        self.monitors[index] = {'region': region_name}
        region_data = self.monitors_region_dict.get(region_name)
        if region_data is None: # ?
            self.warn("no data for region %s" % region_name)
            return
        self.monitors_set_region_keys(region_name, index, region_data, monitor_type=monitor_type)
        #self.monitors_region_dict[region_name]['available'] = False # Mark as in-use
        item.setData(UserRole, (index, region_name))
        tw.setItem(nrows, COLUMN_REGION, item)

        item = make_item(self.project.get_value('monitor_name', args=[index], default=''))
        tw.setItem(nrows, COLUMN_FILENAME, item)

        item = make_item(str(index))
        tw.setItem(nrows, COLUMN_ID, item)

        self.fixup_monitors_table()
        if autoselect:
            tw.setCurrentCell(nrows, COLUMN_REGION)


    def monitors_find_index(self):
        # Always add new monitor at end
        return 1 if not self.monitors else 1 + max(self.monitors)


    def monitors_delete_regions(self):
        ui = self.ui.monitors
        tw = ui.tablewidget_regions
        row = get_selected_row(tw)
        if row is None: # No selection
            return

        # Unset keywords
        kwlist = list(self.project.keywordItems())
        for kw in kwlist:
            key, args = kw.key, kw.args
            if key.startswith('monitor_') and args and args[0]==self.monitors_current_index:
                self.unset_keyword(key, args=args)

        r = self.monitors_current_region
        if r and r in self.monitors_region_dict:
            self.monitors_region_dict[r]['available'] = True

        i = self.monitors_current_index
        if i in self.monitors:
            del self.monitors[i]

        self.monitors_current_region = None
        self.monitors_current_index = None

        tw.removeRow(row)
        self.fixup_monitors_table()
        self.monitors_setup_current_tab()
        self.update_nav_tree()


    def monitors_delete_solids_phase(self, phase_index):
        """adjust monitors_current_solid when solids phase deleted"""
        if (self.monitors_current_solid is not None and
            self.monitors_current_solid >= phase_index):
            self.monitors_current_solid -= 1
            if self.monitors_current_solid == 0:
                self.monitors_current_solid = None


    def handle_monitors_region_selection(self):
        ui = self.ui.monitors
        table = ui.tablewidget_regions
        row = get_selected_row(table)
        nrows = table.rowCount()
        if row is None:
            index = None
            region = None
        else:
            (index, region) = table.item(row,COLUMN_REGION).data(UserRole)
        self.monitors_current_index, self.monitors_current_region = index, region
        enabled = (row is not None)
        for item in (ui.toolbutton_delete,
                     ui.bottom_frame):
            item.setEnabled(enabled)
        if not enabled:
            return
        # Set combobox to only allow appropriate monitor_type
        cb = ui.combobox_monitor_type
        mon = index
        monitor_type = self.project.get_value('monitor_type', args=[mon])
        region_type = self.monitors_region_dict.get(region)
        if region_type is None:
            self.error("Invalid region %s" % region)
            return
        region_type = region_type.get('type')
        if region_type == 'point':
            valids = MONITOR_TYPES_POINT
        elif 'plane' in region_type:
            valids = MONITOR_TYPES_PLANE
        elif 'box' in region_type:
            valids = MONITOR_TYPES_VOLUME
        else:
            valids = []
        for i in range(len(cb)):
            get_combobox_item(cb, i).setEnabled(i in valids)
        if monitor_type is None or monitor_type not in valids:
            # Default not specified in SRS, but 1 (sum) is valid for planes and volumes
            monitor_type = 0 if region_type == 'point' else 1
            self.update_keyword('monitor_type', monitor_type, args=[mon])
        cb.setCurrentIndex(monitor_type)
        cb.setToolTip(get_combobox_item(cb, monitor_type).toolTip())
        enable_phase = self.monitor_requires_phase(mon)
        if enable_phase: # Disable all tabs except 'Phase'
            for i in range(ui.tab_layout.columnCount()-1): # Skip 'Phase'
                item = ui.tab_layout.itemAtPosition(0, i)
                if not item:
                    continue
                widget = item.widget()
                if not widget:
                    continue
                widget.setEnabled(False)
                widget.setToolTip('')
                ui.pushbutton_phase.setEnabled(True)
        else:
            ui.pushbutton_fluid.setEnabled(True)
            for i in range(1, 1+len(self.solids)): # Enable tabs for TFM solids
                item = ui.tab_layout.itemAtPosition(0, i)
                if not item:
                    continue
                widget = item.widget()
                if not widget:
                    continue
                enabled = self.project.get_value('solids_model', args=[i])=='TFM'
                widget.setEnabled(enabled)
                widget.setToolTip('TFM solids required' if not enabled else '')
            ui.pushbutton_scalar.setEnabled(self.project.get_value('nscalar', default=0) > 0)
            ui.pushbutton_phase.setEnabled(False)

        self.setup_monitors() # reinitialize all widgets
        ui.scrollarea_detail.ensureVisible(0, 0)  # scroll to top



    def fixup_monitors_table(self, stretch_column=1):
        ui = self.ui.monitors
        hv = QHeaderView
        tw = ui.tablewidget_regions # main table, adjust top splitter
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
        icon_height = sub_icon_height() + 8
        ui.top_frame.setMaximumHeight(height+icon_height)
        ui.top_frame.setMinimumHeight(header_height+icon_height+row_height*min(nrows,5))
        ui.top_frame.updateGeometry()
        tw.setMaximumHeight(height)
        tw.setMinimumHeight(header_height)
        tw.updateGeometry() #? needed?


    def monitors_update_enabled(self):
        if self.monitors:
            # Never disable if there are Monitors defined
            disabled = False
        else:
            # If there are no solids, no scalar equations, and the fluid solver is disabled,
            # then we have no input tabs on the Monitors pane, so disable it completely
            regions = self.ui.regions.get_region_dict()
            nregions = sum(1 for (name, r) in regions.items()
                           if r.get('type') in ('box', 'point')
                           or 'plane' in r.get('type'))
            disabled = (nregions==0
                        or (self.project.get_value('nrr', default=0) == 0
                            and self.project.get_value('nscalar',default=0)==0
                            and len(self.solids)==0))
        self.find_navigation_tree_item("Monitors").setDisabled(disabled)

    def monitors_tab_to_index(self, tab, solid):
        return (0 if tab==FLUID_TAB
                else len(self.solids)+1 if tab==SCALAR_TAB
                else len(self.solids)+2 if tab==REACTIONS_TAB
                else len(self.solids)+3 if tab==PHASE_TAB
                else solid)


    def monitors_change_tab(self, tab, solid):
        ui = self.ui.monitors
        index = self.monitors_tab_to_index(tab, solid)

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

        current_index = ui.stackedwidget_detail.currentIndex()
        # If we're switching from solid m to solid n, we need some
        # special handling, because both tabs are really the same
        # widget.  We make a picture of the current tab, display that
        # in a dummy pane, then slide back to the solids tab
        if tab == current_index == SOLIDS_TAB:
            if solid == self.monitors_current_solid:
                return # Really nothing to do

            if solid > (self.monitors_current_solid or 0):
                dummy_label = ui.label_dummy_solids_L
                dummy_tab = SOLIDS_TAB_DUMMY_L
            else:
                dummy_label = ui.label_dummy_solids_R
                dummy_tab = SOLIDS_TAB_DUMMY_R
            p = QPicture()
            ui.page_solids.render(p, flags=QWidget.DrawChildren)  #avoid rendering bg
            dummy_label.setPicture(p)
            ui.stackedwidget_detail.setCurrentIndex(dummy_tab)

        self.monitors_current_tab = tab
        self.monitors_current_solid = self.P = solid if tab==SOLIDS_TAB else None

        self.monitors_setup_current_tab()

        # change stackedwidget contents
        self.animate_stacked_widget(
            ui.stackedwidget_detail,
            ui.stackedwidget_detail.currentIndex(),
            tab,
            direction='horizontal',
            line = ui.tab_underline,
            to_btn = ui.tab_layout.itemAtPosition(0, index),
            btn_layout = ui.tab_layout)
        # Scroll to top
        ui.scrollarea_detail.ensureVisible(0, 0)


    def monitors_check_region_in_use(self, name):
        return any(data.get('region')==name for data in self.monitors.values())


    def monitors_update_region(self, name, data):
        for (i,mon) in self.monitors.items():
            if mon.get('region') == name:
                self.monitors_set_region_keys(name, i, data)


    def monitors_set_region_keys(self, name, idx, data, monitor_type=None):
        # Update the keys which define the region the monitor applies to
        if monitor_type is not None:
            self.update_keyword('monitor_type', monitor_type, args=[idx])
        no_k = self.project.get_value('no_k')
        for (key, val) in zip(('x_w', 'y_s', 'z_b',
                               'x_e', 'y_n', 'z_t'),
                              data['from']+data['to']):
            # monitor_z_t and monitor_z_b keywords should not be added when no_k=True
            if no_k and key in ('z_t', 'z_b'):
                continue
            self.update_keyword('monitor_'+key, val, args=[idx])


    def monitors_change_region_name(self, old_name, new_name):
        ui = self.ui.monitors
        for (key, val) in self.monitors.items():
            if val.get('region') == old_name:
                self.monitors[key]['region'] = new_name
                tw = ui.tablewidget_regions
                for i in range(tw.rowCount()):
                    data = tw.item(i,COLUMN_REGION).data(UserRole)
                    index, name = data
                    if key == index:
                        item = tw.item(i,COLUMN_REGION)
                        item.setData(UserRole, (index, new_name))
                        item.setText(new_name)
                        # Also update monitor_name, if it is at the default setting
                        monitor_name = self.project.get_value('monitor_name', args=[key])
                        if monitor_name and (monitor_name==name
                                             or monitor_name.startswith(name+'_')):
                            monitor_name = self.monitor_default_name(new_name)
                            self.update_keyword('monitor_name', monitor_name, args=[index])
                            tw.item(i, COLUMN_FILENAME).setText(monitor_name)

    def reset_monitors(self):
        self.monitors.clear()
        self.monitors_current_index = None
        self.monitors_current_region = None
        self.monitors_region_dict = None
        self.monitors_current_solid = self.P = None
        ui = self.ui.monitors
        ui.tablewidget_regions.clearContents()
        ui.tablewidget_regions.setRowCount(0)
        # anything else to do here?
        # TODO remove dynamically created input widgets, although this should
        #  get handled next time we call 'setup'

    def monitor_regions_to_str(self):
        ui = self.ui.monitors
        tw = ui.tablewidget_regions
        data = [tw.item(i,COLUMN_REGION).data(UserRole)
                for i in range(tw.rowCount())]
        return JSONEncoder().encode(data)


    def monitors_regions_from_str(self, s):
        if not s:
            return
        data = JSONDecoder().decode(s)
        for (index, region) in data:
            monitor_type = self.project.get_value('monitor_type', args=[index])
            self.monitors_add_regions_1([region], monitor_type=monitor_type,
                                        index=index, autoselect=False)


    def setup_monitors(self):
        ui = self.ui.monitors
        # Grab a fresh copy, may have been updated
        self.monitors_region_dict = self.ui.regions.get_region_dict()

        # Mark regions which are in use (this gets reset each time we get here)
        #for (i, data) in self.monitors.items():
        #    region = data['region']
        #    if region in self.monitors_region_dict:
        #        self.monitors_region_dict[region]['available'] = False
        self.fixup_monitors_table()
        row = get_selected_row(ui.tablewidget_regions)
        # Autoselect if only 1 row
        if row is None and ui.tablewidget_regions.rowCount() == 1:
            row = 0
            ui.tablewidget_regions.setCurrentCell(row, COLUMN_REGION)
        enabled = (row is not None)
        for item in (ui.toolbutton_delete,
                     ui.bottom_frame):
            item.setEnabled(enabled)

        mon = self.monitors_current_index
        enable_phase = self.monitor_requires_phase(mon)

        #**Fluid phase tab**
        b = ui.pushbutton_fluid
        b.setText(self.fluid_phase_name)
        font = b.font()
        font.setBold(self.monitors_current_tab == FLUID_TAB)
        b.setFont(font)
        w = b.fontMetrics().boundingRect(b.text()).width() + 20
        b.setMaximumWidth(w)
        b.setEnabled(not enable_phase)

        #**Solids Phase Tab** *(Requires TFM Solids)*
        #Each solid phase will have its own tab. The tab name should be the name of the solid
        solids_names = list(self.solids.keys())
        if self.monitors_saved_solids_names != solids_names:
            # Clear out the old ones
            n_cols = ui.tab_layout.columnCount()
            for i in range(n_cols-1, 0, -1):
                item = ui.tab_layout.itemAtPosition(0, i)
                if not item:
                    continue
                widget = item.widget()
                if not widget:
                    continue
                if widget in (ui.pushbutton_fluid, ui.pushbutton_scalar,
                              ui.pushbutton_reactions, ui.pushbutton_phase):
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
                font.setBold(self.monitors_current_tab==SOLIDS_TAB and i==self.monitors_current_solid)
                b.setFont(font)
                b.pressed.connect(lambda i=i: self.monitors_change_tab(SOLIDS_TAB, i))
                ui.tab_layout.addWidget(b, 0, i)

        # Only TFM solids
        for i in range(1, 1+len(self.solids)):
            enabled = self.project.get_value('solids_model', args=[i])=='TFM' and not enable_phase
            item = ui.tab_layout.itemAtPosition(0, i)
            if item:
                widget = item.widget()
            if widget:
                widget.setEnabled(enabled)

        #Scalar (tab) - Tab only available if scalar equations are solved
        # Move the 'Scalar' button to the right of all solids, if needed
        b = ui.pushbutton_scalar
        font = b.font()
        font.setBold(self.monitors_current_tab==SCALAR_TAB)
        b.setFont(font)
        nscalar = self.project.get_value('nscalar', default=0)
        enabled = (nscalar > 0) and not enable_phase
        b.setEnabled(enabled)
        if len(self.solids) != len(self.monitors_saved_solids_names):
            ui.tab_layout.removeWidget(b)
            ui.tab_layout.addWidget(b, 0, 1+len(self.solids))

        #Reactions (tab) - Tab only available if  nrr > 0
        # Move the 'Reactions' button to the right of all solids, if needed
        b = ui.pushbutton_reactions
        font = b.font()
        font.setBold(self.monitors_current_tab==REACTIONS_TAB)
        b.setFont(font)
        nrr = self.project.get_value('nrr', default=0)
        enabled = (nrr > 0) and not enable_phase
        if nrr==0 and not enable_phase:
            b.setToolTip("Requires nrr > 0")
        else:
            b.setToolTip('')
        b.setEnabled(enabled)
        if len(self.solids) != len(self.monitors_saved_solids_names):
            ui.tab_layout.removeWidget(b)
            ui.tab_layout.addWidget(b, 0, 2+len(self.solids))

        # Move the 'Phase' button to the right of all solids, if needed
        b = ui.pushbutton_phase
        font = b.font()
        font.setBold(self.monitors_current_tab==PHASE_TAB)
        b.setFont(font)
        b.setEnabled(enable_phase)
        if len(self.solids) != len(self.monitors_saved_solids_names):
            ui.tab_layout.removeWidget(b)
            ui.tab_layout.addWidget(b, 0, 3+len(self.solids))

        self.monitors_saved_solids_names = solids_names
        self.P = self.monitors_current_solid

        if mon is None:
            #Construct the GUI, even though disabled (species checkboxes, etc)
            self.monitors_setup_current_tab()
            return


        key = 'monitor_name'
        le = ui.lineedit_keyword_monitor_name_args_MONITOR
        val = self.project.get_value(key, args=[mon])
        if val is None: # Construct from region name
            val = self.monitor_default_name(self.monitors_current_region)
            self.update_keyword(key, val, args=[mon])
        le.updateValue(key, val)
        # Update table too
        tw = ui.tablewidget_regions
        for i in range(tw.rowCount()):
            data = tw.item(i, 0).data(UserRole)
            index, name = data
            if index == mon:
                tw.item(i, COLUMN_FILENAME).setText(val)

        #Specify write interval
        key = 'monitor_dt'
        default = 0.05
        le = ui.lineedit_keyword_monitor_dt_args_MONITOR
        val = self.project.get_value(key, args=[mon])
        if val is None:
            val = default
            self.update_keyword(key, val, args=[mon])
        le.updateValue(key, val)

        # Don't stay on a disabled tab
        index = self.monitors_tab_to_index(self.monitors_current_tab, self.monitors_current_solid)
        item = None if index is None else ui.tab_layout.itemAtPosition(0, index)
        b = item.widget() if item else None
        if ui.isEnabled() and not (b and b.isEnabled()):
            self.monitors_change_tab(*self.monitors_find_valid_tab())
        else:
            self.monitors_setup_current_tab()

        # make sure underline is in the right place, as # of solids may
        # have changed (lifted from animate_stacked_widget, which we
        # don't want to call here)
        tab = self.monitors_current_tab
        line_to = self.monitors_tab_to_index(tab, self.monitors_current_solid)
        line = ui.tab_underline
        btn_layout = ui.tab_layout
        if line_to is not None:
            btn_layout.addItem(btn_layout.takeAt(
                btn_layout.indexOf(line)), 1, line_to)


    def monitor_requires_phase(self, mon):
        if mon is None:
            return False
        val = self.project.get_value('monitor_type', args=[mon])
        return val in (VOLUME_FLOW_RATE, MASS_FLOW_RATE)


    def monitors_find_valid_tab(self):
        mon = self.monitors_current_index
        if self.monitor_requires_phase(mon):
            return (PHASE_TAB, None)
        else:
            return (FLUID_TAB, None) # Always available, even if fluid solver disabled


    def monitor_default_name(self, region_name):
        # Construct default value for MONITOR_NAME,
        # replacing possibly troublesome characters
        key = 'monitor_name'
        val = region_name
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


    def monitors_setup_current_tab(self):
        if self.monitors_current_tab == FLUID_TAB:
            self.setup_monitors_fluid_tab()
        elif self.monitors_current_tab == SOLIDS_TAB:
            self.setup_monitors_solids_tab(self.monitors_current_solid)
        elif self.monitors_current_tab == SCALAR_TAB:
            self.setup_monitors_scalar_tab()
        elif self.monitors_current_tab == REACTIONS_TAB:
            self.setup_monitors_reactions_tab()
        elif self.monitors_current_tab == PHASE_TAB:
            self.setup_monitors_phase_tab()

    def monitors_extract_regions(self):
        # Note, "monitors" is not a ConditionCollection
        # like BCs, VTKs, etc.  Should it be?
        if self.monitors:
            # We assume that monitor regions have been initialized correctly
            # from mfix_gui_comments.
            # TODO: verify that there is an monitor region for each monitor
            return

        if self.monitors_region_dict is None:
            self.monitors_region_dict = self.ui.regions.get_region_dict()

        for mon in self.project.get_key_indices('monitor_name'):
            extent = [self.project.get_value('monitor_'+k, args=[mon])
                      for k in ('x_w', 'y_s', 'z_b',
                                'x_e', 'y_n', 'z_t')]
            extent = [0 if x is None else x.value for x in extent]

            for (region_name, data) in self.monitors_region_dict.items():
                ext2 = [0 if x is None else x for x in
                        (data.get('from',[]) + data.get('to',[]))]
                if ext2 == extent:
                    if data.get('available', True):
                        monitor_type = self.project.get_value('monitor_type', args=[mon])
                        self.monitors_add_regions_1([region_name], monitor_type=monitor_type,
                                                    index=mon, autoselect=False)
                        break
            else:
                self.warn("monitor %s: could not match defined region %s" %
                          (mon, extent))
                kwlist = list(self.project.keywordItems())
                for kw in kwlist:
                    key, args = kw.key, kw.args
                    if key.startswith('monitor_') and args and args[0]==mon:
                        self.unset_keyword(key, args=args)


    def setup_monitors_fluid_tab(self):
        #Fluid (tab)
        ui = self.ui.monitors
        mon = self.monitors_current_index
        layout = ui.groupbox_fluid.layout()

        def get_widget(key):
            return getattr(ui, 'checkbox_keyword_%s_args_MONITOR'%key)

        # NB normally we bail out if index is None but we want to construct
        #  the fluid species checkboxes (for visual apperarance sake)
        for key in ('monitor_ep_g',
                    'monitor_p_g',
                    'monitor_u_g',
                    'monitor_v_g',
                    'monitor_w_g'):
            val = False if mon is None else self.project.get_value(key, default=False, args=[mon])
            widget = get_widget(key)
            widget.setChecked(bool(val))

        # Temperature requires fluid solver and ENERGY_EQ = .TRUE.
        key = 'monitor_t_g'
        enabled = self.project.get_value('energy_eq', default=True) and not self.fluid_solver_disabled
        val = False if mon is None else self.project.get_value(key, default=False, args=[mon])
        if val and not enabled:
            val = False
            self.update_keyword(key, val, args=[mon])
        widget = get_widget(key)
        widget.setChecked(bool(val))
        widget.setEnabled(enabled)

        # Turbulent kinetic energy and dissipation require TURBULENCE_MODEL='K_EPSILON'
        enabled = self.project.get_value('turbulence_model', default=DEFAULT_TURBULENCE_MODEL) == 'K_EPSILON'
        for key in ('monitor_k_turb_g',
                    'monitor_e_turb_g'):
            val = False if mon is None else self.project.get_value(key, default=False, args=[mon])
            if val and not enabled:
                val = False
                self.unset_keyword(key, args=[mon])
            widget = get_widget(key)
            widget.setChecked(bool(val))
            widget.setEnabled(enabled)

        key = 'monitor_x_g'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        names = list(self.fluid_species.keys())
        n = len(names)
        # Remove extra widgets if number decreased
        if len(cbs) > n:
            for (i, cb) in enumerate(cbs[n:], 1+n):
                self.unset_keyword(key, args=[mon, i]) # Should have been done already by fluid_delete_species_keys
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:n]

        while len(cbs) < n:
            cb = CheckBox()
            cb.key = key
            cb.args = ['MONITOR', 1+len(cbs)]
            assert len(cb.args)==len(keyword_args[key])
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            layout.addWidget(cb)
        # Set checkboxes to correct state
        for (i, cb) in enumerate(cbs, 1):
            val = self.project.get_value(key, args=[mon, i], default=False)
            cb.setChecked(bool(val))
            cb.setText(names[i-1]) #Species name may have changed


    def setup_monitors_solids_tab(self, P):
        # Solid-# (tab) - Rename tab to user provided solids name.
        # Note, solids phases are numbered 1-N
        ui = self.ui.monitors
        self.monitors_current_solid = self.P = P
        if P is None: # Nothing to do
            return
        mon = self.monitors_current_index
        if mon is None: # No region selected
            return
        if self.project.get_value('solids_model', args=[P]) != 'TFM':
            return

        layout = ui.groupbox_solids.layout()

        def get_widget(key):
            for pat in ('checkbox_keyword_%s_args_MONITOR',
                        'checkbox_keyword_%s_args_MONITOR_P',
                        'checkbox_keyword_%s_args_MONITOR_P_S'):
                widget = getattr(ui, pat % key, None)
                if widget:
                    return widget
            self.error('no widget for key %s' % key)

        def setup_key_widget(key):
            args = mkargs(key, monitor=mon, phase=P)
            val = self.project.get_value(key, args=args)
            get_widget(key).setChecked(bool(val))

        for key in ('monitor_u_s', 'monitor_v_s', 'monitor_w_s',
                    'monitor_rop_s', 'monitor_p_star'):
            setup_key_widget(key)

        # Solids temperature requires ENERGY_EQ = .TRUE.
        key = 'monitor_t_s'
        enabled = self.project.get_value('energy_eq', default=True)
        val = False if mon is None else self.project.get_value(key, default=False, args=[mon,P])
        if val and not enabled:
            val = False
            self.update_keyword(key, val, args=[mon,P])
        widget = get_widget(key)
        widget.setChecked(bool(val))
        widget.setEnabled(enabled)

        # Granular temperature requires KT_TYPE != 'ALGEBRAIC'
        key = 'monitor_theta_m'
        enabled = self.project.get_value('kt_type', default=DEFAULT_KT_TYPE) != 'ALGEBRAIC'
        val = False if mon is None else self.project.get_value(key, default=False, args=[mon,P])
        if val and not enabled:
            val = False
            self.update_keyword(key, val, args=[mon,P])
        widget = get_widget(key)
        widget.setChecked(bool(val))
        widget.setEnabled(enabled)


        key = 'monitor_x_s'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        names = list(self.solids_species[P].keys())
        n = len(names)
        # Remove extra widgets if number decreased
        if len(cbs) > n:
            for (i, cb) in enumerate(cbs[n:], 1+n):
                self.unset_keyword(key, args=[mon, P, i]) # Should have been done already by solids_delete_species_keys
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:n]

        while len(cbs) < n:
            cb = CheckBox()
            cb.key = key
            cb.args = ['MONITOR', P, 1+len(cbs)]
            assert len(cb.args)==len(keyword_args[key])
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            layout.addWidget(cb)
        # Set checkboxes to correct state
        for (i, cb) in enumerate(cbs, 1):
            val = self.project.get_value(key, args=[mon, i], default=False)
            cb.setChecked(bool(val))
            cb.setText(names[i-1]) #Species name may have changed


    def setup_monitors_scalar_tab(self):
        mon = self.monitors_current_index
        if mon is None:
            return # No selection

        ui = self.ui.monitors
        nscalar = self.project.get_value('nscalar', default=0)
        old_nscalar = getattr(ui, 'nscalar', None)
        ui.nscalar = nscalar

        key = 'monitor_scalar'
        if nscalar == old_nscalar:
            # What do we have to do?  Just update the checkboxes
            for i in range(1, nscalar+1):
                # TODO use ui.dynamic_widgets instead of getattr/setattr
                cb = getattr(ui, "checkbox_monitor_scalar_%s" % i, None)
                val = self.project.get_value(key, args=[mon, i], default=False)
                if cb:
                    cb.setChecked(bool(val))
            return

        layout = ui.groupbox_scalar.layout()

        for i in range(layout.rowCount()-1, -1, -1):
            item = layout.itemAtPosition(i,0)
            if not item:
                continue
            widget = item.widget()
            if not widget:
                continue
            if isinstance(widget, CheckBox):
                self.unset_keyword(widget.key, args=widget.args)
            widget.setParent(None)
            widget.deleteLater()

        #Sets keyword MONITOR_SCALAR(#,#)
        #DEFAULT False
        key = 'monitor_scalar'
        row = 0
        for i in range(1, nscalar+1):
            cb = CheckBox("Scalar %s" % i)
            cb.key = key
            cb.args = ['MONITOR', i]
            assert len(cb.args)==len(keyword_args[key])
            cb.value_updated.connect(self.project.submit_change)
            setattr(ui, 'checkbox_monitor_scalar_%s'%i, cb)
            self.add_tooltip(cb, key)
            val = self.project.get_value(key, args=[mon, i])
            cb.setChecked(bool(val))
            layout.addWidget(cb, row, 0)
            row += 1

    def setup_monitors_reactions_tab(self):
        mon = self.monitors_current_index
        if mon is None:
            return # No selection

        ui = self.ui.monitors
        nrr = self.project.get_value('nrr', default=0)
        old_nrr = getattr(ui, 'nrr', None)
        ui.nrr = nrr

        key = 'monitor_rrate'
        if nrr == old_nrr:
            # What do we have to do?  Just update the checkboxes
            for i in range(1, nrr+1):
                cb = getattr(ui, "checkbox_monitor_rrate_%s" % i, None)
                val = self.project.get_value(key, args=[mon, i], default=False)
                if cb:
                    cb.setChecked(bool(val))
            return

        layout = ui.groupbox_reactions.layout()
        for i in range(layout.rowCount()-1, -1, -1):
            item = layout.itemAtPosition(i,0)
            if not item:
                continue
            widget = item.widget()
            if not widget:
                continue
            if isinstance(widget, CheckBox):
                self.unset_keyword(widget.key, args=widget.args)
            widget.setParent(None)
            widget.deleteLater()

        #Sets keyword MONITOR_RRATE(#,#)
        #DEFAULT False
        key = 'monitor_rrate'
        row = 0
        for i in range(1, nrr+1):
            cb = CheckBox("Reaction rate %s" % i)
            cb.key = key
            cb.args = ['MONITOR', i]
            assert len(cb.args)==len(keyword_args[key])
            cb.value_updated.connect(self.project.submit_change)
            setattr(ui, 'checkbox_monitor_rrate_%s'%i, cb)
            self.add_tooltip(cb, key)
            val = self.project.get_value(key, args=[mon, i])
            cb.setChecked(bool(val))
            layout.addWidget(cb, row, 0)
            row += 1


    def setup_monitors_phase_tab(self):
        mon = self.monitors_current_index
        if mon is None:
            return # No selection
        ui = self.ui.monitors
        layout = ui.groupbox_phase.layout()
        phase_names = [self.fluid_phase_name] + list(self.solids.keys())

        for i in range(layout.rowCount()-1, -1, -1):
            item = layout.itemAtPosition(i,0)
            if not item:
                continue
            widget = item.widget()
            if not widget:
                continue
            widget.setParent(None)
            widget.deleteLater()
        row = 0
        for (i,name) in enumerate(phase_names):
            key = 'monitor_rop_s' if i>0 else 'monitor_ep_g'
            args = ['MONITOR', i] if i else ['MONITOR']
            cb = CheckBox("%s flow rate" % name)
            cb.key = key
            cb.args = args
            self.add_tooltip(cb, key)
            assert len(cb.args)==len(keyword_args[key])
            cb.value_updated.connect(self.project.submit_change)
            enabled = i==0 or self.project.get_value('solids_model', args=[i])=='TFM'
            val = self.project.get_value(key, args=[mon, i])
            if val and not enabled:
                val = False
                self.unset_keyword(key, args=[mon, i])
            cb.setEnabled(enabled)
            cb.setToolTip('TFM solids required' if not enabled else 'Write flow rate for selected species.')
            cb.setChecked(bool(val))
            layout.addWidget(cb, row, 0)
            row += 1
