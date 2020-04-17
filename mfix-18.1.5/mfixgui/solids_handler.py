# methods to deal with solids, split off from mfixgui.py


from copy import deepcopy
from collections import OrderedDict

from qtpy import QtWidgets
from qtpy.QtCore import Qt

from mfixgui.constants import *
from mfixgui.widgets.base import LineEdit

from mfixgui.tools import (append_row_column_triangular, drop_row_column_triangular,
                           format_key_with_args, keyword_args, safe_float)
from mfixgui.tools.qt import (get_combobox_item, get_selected_row,
                              set_combobox_tooltip, set_item_enabled,
                              set_item_noedit, widget_iter)

from mfixgui.species_handler import SpeciesHandler
from mfixgui.solids_tfm import SolidsTFM
from mfixgui.solids_dem import SolidsDEM
from mfixgui.solids_pic import SolidsPIC

from mfixgui import default_values

TAB_MATERIALS, TAB_TFM, TAB_DEM, TAB_PIC = range(4)

class SolidsHandler(SolidsTFM, SolidsDEM, SolidsPIC, SpeciesHandler):

    def init_solids_default_models(self):
        self.solids_density_model = CONSTANT
        self.solids_viscosity_model = OTHER
        self.solids_mol_weight_model = MIXTURE
        self.solids_specific_heat_model = CONSTANT
        self.solids_conductivity_model = OTHER


    def init_solids_handler(self):
        self.solids = OrderedDict() # keyed by name of solids phase
        self.solids_current_phase = None
        self.solids_species = {} #dict of OrderedDict, keyed by phase num
        #  The value dict  is keyed by species, but probably should
        #  be keyed by alias.
        #  Note we now enforce Alias=Species (alias-species integration)

        self.solids_current_tab = TAB_MATERIALS

        ui = self.ui.solids
        self.solids_pushbuttons = (ui.pushbutton_solids_materials,
                                   ui.pushbutton_solids_tfm,
                                   ui.pushbutton_solids_dem,
                                   ui.pushbutton_solids_pic)


        tb = ui.toolbutton_solids_add
        tb.clicked.connect(self.solids_add)
        tb = ui.toolbutton_solids_delete
        tb.clicked.connect(self.solids_delete)
        tb.setEnabled(False)
        tw_solids, tw_solids_species = ui.tablewidget_solids, ui.tablewidget_solids_species
        # Force summary table to update on kw updates
        class TableWidgetProxy:
            def objectName(self):
                return "proxy"
            def updateValue(*args): # pylint: disable=no-method-argument
                self.update_solids_table()
        # TODO maybe the solids_table should just be directly editable?
        self.project.register_widget(TableWidgetProxy(),
                             ['solids_model', 'd_p0', 'ro_s0'], args='*')
        tw_solids.itemSelectionChanged.connect(self.handle_solids_table_selection)
        cb = ui.combobox_solids_model
        cb.activated.connect(self.handle_combobox_solids_model)
        cb.setToolTip(cb.currentText())
        ui.lineedit_solids_phase_name.value_updated.connect(self.handle_solids_phase_name)

        ui.checkbox_enable_scalar_eq.clicked.connect(
            self.enable_solids_scalar_eq)
        ui.spinbox_nscalar_eq.valueChanged.connect(
            lambda val: self.set_nscalar_phase(val, phase=self.solids_current_phase))

        self.init_solids_default_models()
        ui.checkbox_keyword_species_eq_args_P.clicked.connect(
            self.handle_solids_species_eq)

        # REQUIRED keys
        le = ui.lineedit_keyword_d_p0_args_P
        le.required = True
        le.saved_value = default_values.d_p0
        le.initpython = default_values.d_p0

        # TODO Is density required?

        # Handle a number of cases which are essentially the same
        # avoid repetition in set_solids_*_model methods
        def make_solids_model_setter(self, name, key):
            def setter(model=None): # If not specified, determine correct model from keywords
                ui = self.ui.solids
                phase = self.solids_current_phase
                key_s0 = 'c_ps0' if key=='c_p' else key + '_s0'
                key_usr = 'usr_cps' if key=='c_p' else 'usr_' + key + 's'
                lineedit = getattr(ui, 'lineedit_keyword_%s_args_P' % key_s0)
                units_label = getattr(ui, 'label_%s_units' % key_s0)

                if model is None and phase is not None:
                    val_s0 = self.project.get_value(key_s0, args=[phase])
                    val_usr = self.project.get_value(key_usr, args=[phase])
                    model = (CONSTANT if val_s0 is not None
                             else UDF if val_usr is not None
                             else VARIABLE)
                    if val_s0 is not None:
                        lineedit.setText(str(val_s0))

                setattr(self, name, model) # self.solids_<name>_model = model
                combobox = getattr(ui, 'combobox_' + name)
                if model != combobox.currentIndex():
                    combobox.setCurrentIndex(model)
                # Make tooltip match setting (for longer names which are truncated)
                combobox.setToolTip(combobox.currentText())

                # Enable lineedit for constant model
                enabled = (model==CONSTANT)
                for item in (lineedit, units_label):
                    item.setEnabled(enabled)

                if phase is None:
                    return

                if model == CONSTANT:
                    value = lineedit.value # Possibly re-enabled gui item
                    if value=='None':
                        value = ''
                    if value != '' and self.project.get_value(key_s0, args=phase) != value:
                        self.update_keyword(key_s0, value, args=[phase]) # Restore keyword value
                    if value == '':
                        val_s0 = self.project.get_value(key_s0, args=[phase])
                        if val_s0 == 'None': # Should not happen, but filter out if found
                            self.unset_keyword(key_s0, args=[phase])
                            val_s0 = None
                        if val_s0 is not None:
                            lineedit.setText(str(val_s0))
                    self.unset_keyword(key_usr, args=phase) # Issues/435
                elif model == UDF:
                    self.unset_keyword(key_s0, args=phase)
                    self.update_keyword(key_usr, True, args=phase)
                else: # Continuum, mixture, etc
                    self.unset_keyword(key_s0, args=phase)
                    self.unset_keyword(key_usr, args=phase)

                if name == 'solids_density_model': # Extra handling: Density column changes
                    self.update_solids_baseline_groupbox(model) # availability
                    self.update_solids_table() # 'density' column changes

            return setter


        for (name, key) in (
                ('density', 'ro'),
                ('viscosity', 'mu'),
                ('specific_heat', 'c_p'),
                ('conductivity', 'k')):

            model_name = 'solids_%s_model' % name
            setter = make_solids_model_setter(self, model_name, key)
            setattr(self, 'set_%s' % model_name, setter)
            combobox = getattr(ui, 'combobox_'+model_name)
            combobox.activated.connect(setter)
            combobox.default_value = getattr(self, model_name)

        # Solids species
        tb = ui.toolbutton_solids_species_add
        tb.clicked.connect(self.solids_species_add)
        tb = ui.toolbutton_solids_species_copy # misnomer - edit
        tb.clicked.connect(self.solids_species_edit)
        tb.setEnabled(False)
        tb = ui.toolbutton_solids_species_delete
        tb.setEnabled(False)
        tb.clicked.connect(self.solids_species_delete)
        tw_solids_species.itemSelectionChanged.connect(self.handle_solids_species_selection)

        # Advanced
        cb = ui.checkbox_disable_close_pack
        cb.clicked.connect(self.disable_close_pack)
        self.add_tooltip(cb, 'close_packed')

        cb = ui.checkbox_enable_added_mass_force
        cb.clicked.connect(self.enable_added_mass_force)
        self.add_tooltip(cb, 'added_mass')

        # connect solid tab buttons
        for i, btn in enumerate(self.solids_pushbuttons):
            btn.pressed.connect(lambda i=i, btn=btn: self.solids_change_tab(i, btn))

        for tw in (ui.tablewidget_solids, ui.tablewidget_solids_species,
                   ui.tablewidget_solids_baseline):
            self.fixup_solids_table(tw)

        self.init_solids_tfm()
        self.init_solids_dem()
        self.init_solids_pic()

        # Add some tooltips
        self.add_tooltip(ui.checkbox_enable_scalar_eq, key=None,
                         description='Enable scalar equations for this phase')
        self.add_tooltip(ui.spinbox_nscalar_eq, key=None,
                         description='Number of scalar equations for this phase')
        cb = ui.combobox_solids_model
        key = 'solids_model'
        self.add_tooltip(cb, key=key)
        d = self.keyword_doc[key]
        # XXX this will need to change when PIC and Hybrid are enabled
        get_combobox_item(cb, 0).setToolTip(d['valids']['TFM']['note'])
        get_combobox_item(cb, 1).setToolTip(d['valids']['DEM']['note'])

    def solids_update_tabs(self):
        ui = self.ui.solids
        solver = self.project.solver
        if solver == SINGLE: # We shouldn't be here
            return

        enable_dict = {TFM: (True, True, False, False),
                       DEM: (True, False, True, False),
                       PIC: (True, False, False, True),
                       HYBRID: (True, True, True, False)}

        enabled = enable_dict[solver] if self.solids else (True, False, False, False)

        for (item, e) in zip(self.solids_pushbuttons, enabled):
            item.setDisabled(not e)

        # Don't stay on a disabled tab!
        # (Do we ever disable "Materials"? - don't think so)
        active_tab = ui.stackedwidget_solids.currentIndex()
        if active_tab > 0 and not enabled[active_tab-1]:
            if solver==SINGLE:
                i, p = 0, ui.pushbutton_solids_materials
            elif solver in (TFM, HYBRID):
                i, p = 1, ui.pushbutton_solids_tfm
            elif solver==DEM:
                i, p = 2, ui.pushbutton_solids_dem
            elif solver==PIC:
                i, p = 3, ui.pushbutton_solids_pic
            self.solids_change_tab(i, p)


    # Solids sub-pane navigation
    def solids_change_tab(self, tabnum, to_btn):
        ui = self.ui.solids
        self.solids_current_tab = tabnum
        self.animate_stacked_widget(
            ui.stackedwidget_solids,
            ui.stackedwidget_solids.currentIndex(),
            tabnum,
            direction='horizontal',
            line=ui.line_solids,
            to_btn=to_btn,
            btn_layout=ui.gridlayout_tab_btns)
        self.setup_solids_tab(tabnum)
        for btn in self.solids_pushbuttons:
            btn.setChecked(btn==to_btn)
            font = btn.font()
            font.setBold(btn==to_btn)
            btn.setFont(font)


    def setup_solids(self):
        # Don't stay on disabled tab!
        ui = self.ui.solids
        tabnum = self.solids_current_tab
        solver = self.project.solver
        if tabnum == TAB_MATERIALS:
            ok = True
        elif tabnum == TAB_TFM:
            ok = solver in (TFM, HYBRID)
        elif tabnum == TAB_DEM:
            ok = solver in (DEM, HYBRID)
        elif tabnum == TAB_PIC:
            ok = (solver == PIC)
        if not ok:
            # Switch to tab 0
            self.solids_change_tab(0, ui.pushbutton_solids_materials)
        else:
            self.setup_solids_tab(tabnum)


    def setup_solids_tab(self, tabnum):
        if tabnum == TAB_MATERIALS:
            self.update_solids_table()
            self.update_solids_detail_pane()
        elif tabnum == TAB_TFM:
            self.setup_tfm_tab()
        elif tabnum == TAB_DEM:
            self.setup_dem_tab()
        elif tabnum == TAB_PIC:
            self.setup_pic_tab()
        else:
            raise ValueError(tabnum)


    # Advanced
    def disable_close_pack(self, val):
        ui = self.ui.solids
        cb = ui.checkbox_disable_close_pack
        if val != cb.isChecked(): # not from a click action
            cb.setChecked(val)
            return
        phase = self.solids_current_phase
        if phase is None:
            return
        close_packed = self.project.get_value('close_packed', default=None, args=phase)
        if (close_packed is not False) and val: # Disabling - popup as per SRS p15
            resp=self.message(text="Disabling close-packing for %s\nAre you sure?" % self.solids_current_phase_name,
                              buttons=['yes','no'],
                              default = 'no')
            if resp != 'yes':
                cb.setChecked(False)
                return

        if close_packed is None and not val:
            return # We will leave the keyword at its default unset value

        self.update_keyword('close_packed', not val, args=phase)


    def enable_added_mass_force(self, val):
        ui = self.ui.solids
        cb = ui.checkbox_enable_added_mass_force
        if val != cb.isChecked(): # not from a click action
            cb.setChecked(val)
            return
        phase = self.solids_current_phase
        if phase is None:
            return
        # Warn when unsetting added mass for other phases
        prev_phase = self.project.get_value('m_am')
        if prev_phase is not None and prev_phase != phase and val:
            prev_phase_name = list(self.solids.keys())[prev_phase-1]
            resp=self.message(text="Disabling added mass force for %s\nAre you sure?" % prev_phase_name,
                              buttons=['yes','no'],
                              default = 'no')
            if resp == 'no':
                cb.setChecked(False)
                return

        if val:
            self.update_keyword('m_am', phase)
            self.update_keyword('added_mass', True)

        else:
            self.unset_keyword('m_am')
            self.unset_keyword('added_mass')


    def fixup_solids_table(self, tw, stretch_column=0):
        # Should we just hide the entire table (including header) if no rows?
        ui = self.ui.solids
        hv = QtWidgets.QHeaderView
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
                       + nrows*row_height + 4) # extra to avoid unneeded scrollbar (?)
        if tw == ui.tablewidget_solids: # In a splitter
            # top_frame is the tab bar, not the top part of the splitter!!
            #icon_height = sub_icon_height() + 8
            #ui.top_frame.setMaximumHeight(height+icon_height)
            #ui.top_frame.setMinimumHeight(header_height+icon_height+row_height*min(nrows,5))
            #ui.top_frame.updateGeometry()
            tw.setMaximumHeight(height)
            tw.setMinimumHeight(header_height+row_height*min(nrows,5))
        else:
            tw.setMaximumHeight(height) # Works for tablewidget inside groupbox
            tw.setMinimumHeight(height) #? needed for tablewidget_des_en_input. should we allow scrollbar?
        tw.updateGeometry() #? needed?


    def handle_solids_species_eq(self, enabled):
        ui = self.ui.solids
        if not enabled:
            self.set_solids_density_model(CONSTANT)
            #ui.combobox_solids_density_model.setCurrentIndex(CONSTANT)
        set_item_enabled(get_combobox_item(ui.combobox_solids_density_model,
                                                VARIABLE), enabled)
        self.update_solids_species_groupbox() # availability

        # issues/183
        self.bcs_check_wall_keys()


    def setup_combobox_solids_model(self):
        """solids model combobox is tied to solver setting"""
        solver = self.project.solver
        if solver == SINGLE:
            # Note, if Single-Phase solver is enabled, this pane is disabled
            return
        ui = self.ui.solids
        cb = ui.combobox_solids_model
        model = cb.model()
        #          TFM,  DEM,  PIC
        enabled = [False, False, False]
        enabled[0] = (solver==TFM or solver==HYBRID)
        enabled[1] = (solver==DEM or solver==HYBRID)
        enabled[2] = (solver==PIC)
        if not self.pic_enabled: # Issues/440
            del enabled[2] # No "pic" item present in menu
        for (i, e) in enumerate(enabled):
            set_item_enabled(get_combobox_item(cb,i), e)

        # Set for current phase
        if self.solids_current_phase is not None:
            mod = self.project.get_value("solids_model", args=self.solids_current_phase)
            i = 0 if mod=='TFM' else 1 if mod=='DEM' else 2 if mod=='PIC' else None
            if i is None:
                i = 0 if solver in (TFM, HYBRID) else 1 if solver == DEM else 2

            if not enabled[i]:
                # Current selection no longer valid, so pick first valid choice
                # Don't leave a non-enabled item selected!
                i = enabled.index(True)
            cb.setCurrentIndex(i)
            set_combobox_tooltip(cb)
        else: # Set based on overall solver
            i = 0 if solver in (TFM, HYBRID) else 1 if solver == DEM else 2
            cb.setCurrentIndex(i)
            set_combobox_tooltip(cb)


    def handle_combobox_solids_model(self, index):
        ## NB:  Solids model is not the same as solver!
        # Solver values are enumerated in constants.py.  Solids models are strings, 'TFM', 'DEM', 'PIC'

        if self.solids_current_phase is None:
            return # shouldn't get here

        phase = self.solids_current_phase
        name, data = list(self.solids.items())[phase-1]
        model = ('TFM', 'DEM', 'PIC')[index]
        self.update_keyword('solids_model', model, args=self.solids_current_phase)
        data['model'] = model
        self.update_solids_table()
        self.update_solids_detail_pane()
        self.update_nav_tree() # PSs depends on solids_model


    def make_solids_name(self, n):
        while True:
            name = 'Solid %d' % n
            if name not in self.solids:
                break
            n += 1
        return name


    def solids_add(self):
        """Define a new solids phase.  It is given a generic name which can be
        changed by the user"""

        if self.project.solver == SINGLE: # Should not get here! this pane is disabled.
            return
        else:
            model = [None, 'TFM', 'DEM', 'PIC', 'TFM'][self.project.solver]

        ui = self.ui.solids
        tw = ui.tablewidget_solids
        nrows = tw.rowCount()
        n = nrows + 1 # index of new solid
        tw.setRowCount(n)
        name = self.make_solids_name(n)

        diameter = default_values.d_p0
        density = default_values.ro_s0
        self.update_keyword('d_p0', diameter, args=[n])
        self.update_keyword('ro_s0', density, args=[n])
        self.update_keyword('solids_model', model, args=n)
        self.solids[name] = {'model': model,
                             'diameter': diameter,
                             'density': density}

        self.solids_species[n] = OrderedDict()
        self.update_keyword('mmax', len(self.solids))
        #self.update_keyword('nmax_s', 0, args=[n]) ? needed
        self.update_keyword('species_eq', False, args=[n]) # Disable species eq by default, per SRS
        self.update_solids_table()
        tw.setCurrentCell(nrows, 0) # Select new item
        #Since c_ps0 is not defined the spec. heat model will default to VARIABLE.  But SRS
        # says default should be CONSTANT, so force that.  However, if user leaves this pane
        # without setting a value for spec. heat, the setting will revert to VARIABLE
        self.set_solids_specific_heat_model(CONSTANT)

        # Reshape triangular matrices
        for key in ('des_et_input', 'des_en_input'):
            prev_size = (n*(n-1))//2 # Size before row added
            vals = [self.project.get_value(key, args=i)
                    for i in range(1, 1+prev_size)]
            if any(v is not None for v in vals):
                new_vals = append_row_column_triangular(vals, n-1)
                for (i, val) in enumerate(new_vals, 1):
                    if val is None:
                        self.unset_keyword(key, args=i)
                    else:
                        self.update_keyword(key, val, args=i)

        # Initialize input fields for new phase
        self.update_solids_detail_pane()
        # Tabs enable/disable depending on number of solids
        self.solids_update_tabs()
        # Set BC keys for solids species at any defined walls
        self.bcs_check_wall_keys()
        # ICs enabled/disabled depends on number of solids
        self.update_nav_tree()


    def handle_solids_table_selection(self):
        ui = self.ui.solids
        self.P = self.solids_current_phase # should already be set
        # Clear out lineedits
        for w in widget_iter(ui.groupbox_solids_parameters):
            if isinstance(w, LineEdit):
                w.setText('')

        tw = ui.tablewidget_solids
        row = get_selected_row(tw)
        enabled = (row is not None)
        ui.toolbutton_solids_delete.setEnabled(enabled)
        #ui.toolbutton_solids_copy.setEnabled(enabled) - removed from ui
        self.solids_current_phase_name = None if row is None else tw.item(row,0).text()
        self.solids_current_phase = (row+1) if row is not None else None
        self.update_solids_detail_pane()


    def update_solids_detail_pane(self):
        """update the solids detail pane for currently selected solids phase.
        if no solid phase # selected, pane is cleared and disabled"""
        ui = self.ui.solids
        phase = self.P = self.solids_current_phase
        if phase is None: #name is None or phase is None: # current solid phase name.
            # Disable all inputs
            self.update_solids_species_table()
            self.fixup_solids_table(ui.tablewidget_solids)
            ui.detail_pane.setEnabled(False)
            for item in widget_iter(ui.detail_pane):
                if isinstance(item, QtWidgets.QCheckBox):
                    item.setChecked(False)
                if isinstance(item, QtWidgets.QLineEdit):
                    # Surprise, a spinbox includes a lineedit!
                    if 'spinbox' in item.objectName(): # 'qt_spinbox_lineedit':
                        item.setText('0')
                    else:
                        item.setText('')
            return

        name = list(self.solids.keys())[phase-1]
        solid = self.solids[name]
        model = solid['model']

        # Enable the input areas, initialize to values for current solid
        ui.detail_pane.setEnabled(ui.input_enabled)
        self.solids_current_phase_name = name
        ui.lineedit_solids_phase_name.setText(self.solids_current_phase_name)
        self.setup_combobox_solids_model()

        # Initialize all the line edit widgets
        def as_str(x):
            return '' if x is None else str(x)
        def get_widget(key):
            return getattr(ui, 'lineedit_keyword_%s_args_P' % key)
        for key in ('d_p0', 'ro_s0', 'des_em'):
            val = self.project.get_value(key, args=phase)
            w = get_widget(key)
            w.setText(as_str(val))
            w.saved_value = val # Reversion value for invalid input

        # And the checkboxes
        for key in ('momentum_x_eq', 'momentum_y_eq', 'momentum_z_eq'):
            cb = getattr(ui, 'checkbox_keyword_%s_args_P'%key)
            if model == 'TFM': # momentum eq only avail w/ TFM solid model
                val = self.project.get_value(key, default=True, args=phase)
                cb.setEnabled(True)
                cb.setChecked(val)
                self.add_tooltip(cb, key)
            else:
                cb.setEnabled(False)
                cb.setChecked(False)
                self.add_tooltip(cb, key, 'Only available for TFM solids model')

        # Set species eq checkbox to correct value
        key = 'species_eq'
        species_eq = self.project.get_value(key, default=True, args=[phase])
        cb = ui.checkbox_keyword_species_eq_args_P
        cb.setChecked(species_eq)

        # Deal with scalar eq
        cb = ui.checkbox_enable_scalar_eq
        sb = ui.spinbox_nscalar_eq
        # Option to enable scalar equations
        # Selection only available for MFIX-TFM solids model
        if model != 'TFM':
            cb.setEnabled(False)
            cb.setChecked(False)
            sb.setMinimum(0)
            sb.setValue(0)
            sb.setEnabled(False)
        else:
            nscalar = self.project.get_value('nscalar', default=0)
            nscalar_phase = self.get_nscalar_phase(phase)
            enabled = (nscalar_phase > 0)
            # Lower minimum before setting value
            if not enabled:
                sb.setMinimum(0)
            sb.setValue(nscalar_phase)
            # Raise minimum after value is set to avoid spurious callback
            if enabled:
                sb.setMinimum(1)
            sb.setEnabled(enabled)
            cb.setChecked(enabled)
            cb.setEnabled(True)

        ### Restrictions (see SRS p13)

        # Variable density model requires species equations
        cb_density =  ui.combobox_solids_density_model
        enabled = bool(species_eq)
        set_item_enabled(get_combobox_item(cb_density, VARIABLE), enabled)
        ro_s0 = self.project.get_value('ro_s0', args=[phase], default=None)
        self.set_solids_density_model(VARIABLE if (enabled and ro_s0 is None) else CONSTANT)

        # Viscosity only available for TFM solids
        self.set_solids_viscosity_model()
        enabled = (model=='TFM')
        for item in (ui.label_solids_viscosity_model,
                     ui.combobox_solids_viscosity_model):
            item.setEnabled(enabled)
        if not enabled:
            for item in (ui.lineedit_keyword_mu_s0_args_P,
                         ui.label_mu_s0_units):
                item.setEnabled(False)

        # Mol. wt is locked to MIXTURE

        # Specific heat only available when solving energy eq
        self.set_solids_specific_heat_model()
        energy_eq = self.project.get_value('energy_eq', default=True)
        enabled = (energy_eq==True)
        for item in (ui.combobox_solids_specific_heat_model,
                     ui.label_solids_specific_heat_model):
            item.setEnabled(enabled)
        if not enabled:
            for item in (ui.lineedit_keyword_c_ps0_args_P,
                         ui.label_c_ps0_units):
                item.setEnabled(False)

        # Thermal Conductivity Model:
        # Selection only available for MFIX-TFM solids model
        # Selection only available when solving thermal energy equations
        cb = ui.combobox_solids_conductivity_model
        if model != 'TFM':
            self.set_solids_conductivity_model(CONSTANT)
            for (i,e) in enumerate((True,False,False)):
                set_item_enabled(get_combobox_item(cb,i), e)
        else:
            self.set_solids_conductivity_model()
            for (i,e) in enumerate((True,True,True)):
                set_item_enabled(get_combobox_item(cb,i), e)
        enabled = bool(energy_eq)
        for item in (ui.combobox_solids_conductivity_model,
                     ui.label_solids_conductivity_model):
            item.setEnabled(enabled)
        if not enabled:
            for item in (ui.lineedit_keyword_k_s0_args_P,
                         ui.label_k_s0_units):
                item.setEnabled(False)

        # Specify solids phase emissivity
        # Selection only available for MFIX-DEM solids model
        enabled = (model=='DEM')
        for item in (ui.label_des_em,
                     ui.lineedit_keyword_des_em_args_P):
            item.setEnabled(enabled)

        # Species input is in its own function
        self.update_solids_species_groupbox()
        # as is the baseline table
        self.update_solids_baseline_groupbox(self.solids_density_model)

        self.update_solids_species_table()
        self.fixup_solids_table(ui.tablewidget_solids)

        # Advanced
        enabled = (model=='TFM')
        ui.groupbox_advanced.setEnabled(enabled)
        kt_type = self.project.get_value('kt_type')
        if enabled:
            close_packed = self.project.get_value('close_packed', default=True, args=phase)
            self.disable_close_pack(not(close_packed))
        else:
            ui.checkbox_disable_close_pack.setChecked(False)
            ui.checkbox_enable_added_mass_force.setChecked(False)

        added_mass = self.project.get_value('added_mass', default=False)
        m_am = self.project.get_value('m_am', default=None)
        self.enable_added_mass_force(added_mass and (m_am == phase))
        # Added mass force not allowed with GHD model
        ui.checkbox_enable_added_mass_force.setEnabled(kt_type != 'GHD')


    def update_solids_table(self):
        ui = self.ui.solids
        table = ui.tablewidget_solids
        if self.solids is None:
            table.clearContents()
            self.unset_keyword('mmax')
            return

        nrows = len(self.solids)
        table.setRowCount(nrows)

        # helper fn
        def make_item(val):
            item = QtWidgets.QTableWidgetItem('' if val is None else str(val))
            set_item_noedit(item)
            return item

        #Update the internal table from keywords
        for (i, (k,v)) in enumerate(self.solids.items(), 1):
            for (myname, realname) in (('model', 'solids_model'),
                                       ('diameter', 'd_p0'),
                                       ('density', 'ro_s0')):
                self.solids[k][myname] = self.project.get_value(realname, args=i)
                #if self.solids[k]['density'] is None and self.solids_density_model==VARIABLE:
                #    self.solids[k]['density'] = "Variable"


        for (row,(k,v)) in enumerate(self.solids.items()):
            table.setItem(row, 0, make_item(k))
            for (col, key) in enumerate(('model', 'diameter', 'density'), 1):
                table.setItem(row, col, make_item(v[key]))

        if nrows == 1: # If there's only 1 let's auto-select it for the user's convenience
            table.setCurrentCell(0,0)

        # trim excess horizontal space - can't figure out how to do this in designer
        header_height = table.horizontalHeader().height()
        if nrows==0:
            # 34 px is empirical, fixme, should calc. row height
            table.setMaximumHeight(header_height + 34)
        else:
            table.setMaximumHeight(header_height+nrows*table.rowHeight(0) + 4)
            # a little extra to avoid horiz scrollbar when not needed

        # Enable/disable the 'add' button
        # GHD is only valid for MMAX <= 2
        # NB: If we are going to enforce this, there are many other
        # MMAX-dependent settings ... search spec.txt for 'MMAX'
        kt_type  = self.project.get_value('kt_type')
        if kt_type == 'GHD':
            if len(self.solids) > 1:
                ui.toolbutton_solids_add.setEnabled(False)
        elif kt_type in ('GD_99', 'GTSH'):
            if len(self.solids):
                ui.toolbutton_solids_add.setEnabled(False)
        else:
            enabled = len(self.solids) < DIM_M
            ui.toolbutton_solids_add.setEnabled(enabled)


    def handle_solids_phase_name(self, widget, value_dict, args):
        ui = self.ui.solids
        le = ui.lineedit_solids_phase_name
        new_name = le.text()
        phase = self.solids_current_phase
        if phase is None:
            return
        old_name = list(self.solids.keys())[phase-1]
        if new_name == old_name:
            return # Nothing to do
        if new_name in self.solids or new_name == self.fluid_phase_name: # Reject the input
            self.warning("%s: name is in use" % new_name, popup=True)
            le.setText(old_name)
            return
        self.solids_current_phase_name = new_name
        # rewriting dict to change key while preserving order - hack
        d = OrderedDict()
        for (k,v) in self.solids.items():
            if k==old_name:
                k = new_name
            d[k] = v
        self.solids = d
        self.update_solids_table()
        key = 'solids_phase_name(%s)'%phase
        if self.project.mfix_gui_comments.get(key) != new_name:
            self.project.mfix_gui_comments[key] = new_name
            self.set_unsaved_flag()
        return True


    def solids_delete(self):
        ui = self.ui.solids
        tw = ui.tablewidget_solids
        row = get_selected_row(tw)

        if row is None: # No selection
            return

        phase = row+1
        phase_name = tw.item(row,0).text()

        key = self.solids_check_phase_in_use(phase)
        if key:
            self.message(text="%s is referenced by %s" %
                         (phase_name, key))
            return

        self.solids_current_phase = self.P = None
        self.solids_current_phase_name = None

        tw.itemSelectionChanged.disconnect() # Avoid selection callbacks during delete.  Reenabled below
        try:
            name = tw.item(row, 0).text()
            tw.removeRow(row)
            del self.solids[name] # This is an ordered dict, keyed by name - no 'hole' problem

            if len(self.solids) < DIM_M:
                ui.toolbutton_solids_add.setEnabled(True)

            self.update_keyword('mmax', len(self.solids))

            # Clear out all keywords related to deleted phase
            # Note Must delete self.solids[name] before calling delete_phase_keys
            self.solids_delete_phase_keys(phase)

            # these panels all have a 'current solid' (index) which must
            # be adjusted
            self.bcs_delete_solids_phase(phase)
            self.ics_delete_solids_phase(phase)
            self.pss_delete_solids_phase(phase)
            self.iss_delete_solids_phase(phase)
            self.output_delete_solids_phase(phase)

            # Fixup phase names in mfix_gui_comments
            for (k,v) in list(self.project.mfix_gui_comments.items()):
                if k.startswith('solids_phase_name('):
                    del self.project.mfix_gui_comments[k]
            for (i,name) in enumerate(self.solids.keys(), 1):
                self.project.mfix_gui_comments['solids_phase_name(%s)'%i] = name

            # species dictionary (TODO, also need to remap keys)
            for n in range(phase, len(self.solids)+1):
                self.solids_species[n] = self.solids_species[n+1]
            del self.solids_species[len(self.solids_species)]

            # fix nscalar
            #  TODO fix initial conditions for scalars (see scalar_handler.py)
            #  Note, for now, don't allow removing solids phase with scalar equations.
            #   See solids_check_phase_in_use
            # nscalar = self.project.get_value('nscalar', default=0)
            # nscalar_new = nscalar - self.get_nscalar_phase(phase)
            # key = 'phase4scalar'
            # vals = [self.project.get_value(key, default=0, args=i) for i in range(1, nscalar+1)]
            # new_vals = [v if v<phase else v-1 for v in vals if v != phase]
            # for (i, val) in enumerate(new_vals, 1):
            #     self.update_keyword(key, val, args=i)
            # for i in range(len(new_vals)+1, nscalar+1):
            #     self.unset_keyword(key, args=i)
            # self.update_keyword('nscalar', nscalar_new)

            # Fix hole in restitution coeffs
            n = len(self.solids)
            for key in ('des_et_input', 'des_en_input'):
                prev_size = ((n+1)*(n+2))//2 # Size before row deleted
                vals = [self.project.get_value(key, args=i)
                        for i in range(1, 1+prev_size)]
                if any(v is not None for v in vals):
                    new_vals = drop_row_column_triangular(vals, n+1, phase)
                    for (i, val) in enumerate(new_vals, 1):
                        self.update_keyword(key, val, args=i)
                    for i in range(len(new_vals)+1,  len(vals)+1):
                        self.unset_keyword(key, args=i)

            # Should be handled by delete_phase_keys
            # for key in ('des_et_wall_input', 'des_en_wall_input'):
            #     prev_size = n+1
            #     vals = [self.project.get_value(key, args=i)
            #             for i in range(1, 1+prev_size)]
            #     if any(v is not None for v in vals):
            #         new_vals = vals[:]
            #         del new_vals[phase-1] # 1-based
            #         for (i, val) in enumerate(new_vals, 1):
            #             self.update_keyword(key, val, args=i)
            #         for i in range(len(new_vals)+1,  len(vals)+1):
            #             self.unset_keyword(key, args=i)

            self.update_solids_table()

            # ICs enabled/disabled depends on nscalar & number of solids
            self.update_nav_tree()

            # Tabs enable/disable depending on number of solids
            self.solids_update_tabs()
        finally:
            # selection callbacks were disabled
            tw.itemSelectionChanged.connect(self.handle_solids_table_selection)
            self.handle_solids_table_selection()
            self.set_unsaved_flag()


    def enable_solids_scalar_eq(self, enabled):
        phase = self.solids_current_phase
        if phase is None:
            return

        ui = self.ui.solids
        cb = ui.checkbox_enable_scalar_eq
        sb = ui.spinbox_nscalar_eq
        cb.setChecked(enabled)
        val = 1 if enabled else 0
        sb.setMinimum(val)
        sb.setValue(val)
        sb.setEnabled(enabled)
        #self.set_nscalar_phase(val, phase) # handled by sb callback


    def update_solids_species_groupbox(self):
        """enable/disable species tables based on state"""
        # Species data required under any of the following conditions:
        #  Solving species equations
        #  Energy equations are solved with mixture specific heat model
        phase = self.solids_current_phase
        ui = self.ui.solids

        if phase is None:
            enabled = False
        else:
            species_eq = self.project.get_value('species_eq', args=phase, default=True)
            energy_eq = self.project.get_value('energy_eq', default=True)
            enabled = (species_eq) or (energy_eq and self.solids_specific_heat_model == MIXTURE)

        ui.groupbox_species.setEnabled(enabled)
        # Buttons seem to take up a lot of space when table is shrunk
        ui.frame_add_delete_copy_species.setVisible(enabled)


    def update_solids_baseline_groupbox(self, density_model):
        #Baseline (unreacted) composition selection:
        # Available only for variable solids density model
        ui = self.ui.solids
        enabled = density_model==VARIABLE
        ui.groupbox_baseline.setEnabled(enabled)
        if not enabled:
            ui.tablewidget_solids_baseline.clearContents()
            ui.tablewidget_solids_baseline.setRowCount(0)
        else:
            self.update_solids_baseline_table()

    def handle_solids_mass_fraction(self, widget, value_dict, args):
        phase = self.solids_current_phase
        if phase is None:
            return
        ui = self.ui.solids
        key = 'x_s0'
        val = value_dict[key]
        # TODO defaults for x_s0?
        table = ui.tablewidget_solids_baseline
        widget.updateValue(key, val)
        if val == '':
            self.unset_keyword(key, args=[phase]+args)
        else:
            self.update_keyword(key, val, args=[phase]+args)
        self.update_solids_mass_fraction_total()

    def update_solids_mass_fraction_total(self):
        ui = self.ui.solids
        key = 'x_s0'
        phase = self.solids_current_phase
        if phase is None:
            return
        table = ui.tablewidget_solids_baseline
        if table.rowCount() == 0:
            return
        total = sum(safe_float(self.project.get_value(key, default=0.0, args=[phase,i]))
                    for i in range(1,len(self.solids_species[phase])+1))
        total = round(total, 6)
        item = table.item(table.rowCount()-1, 1)
        font = item.font()
        font.setBold(True)
        item.setFont(font)
        item.setText(str(total))
        if total != 1.0:
            item.setForeground(Qt.red)
            #We should warn, but this creates too many popups while solid is being set up
            #self.warning("Mass fractions sum to %s, must be 1.0" % total, popup=True)
        elif ui.isEnabled():
            item.setForeground(Qt.black) # Fixme looks wrong when greyed-out


    def handle_solids_inert_species(self, species_index, val):
        phase = self.solids_current_phase
        if phase is None:
            return
        if val:
            self.update_keyword('inert_species', species_index, args=phase)
        else:
            self.unset_keyword('inert_species', args=phase)

        for (i, cb) in enumerate(self.solids_inert_species_checkboxes, 1):
            if i != species_index:
                cb.setChecked(False)


    def update_solids_baseline_table(self):
        ui = self.ui.solids
        table = ui.tablewidget_solids_baseline
        phase = self.solids_current_phase
        if phase is None:
            return
        table.clearContents()
        if self.solids_species[phase]:
            nrows = len(self.solids_species[phase])+1
        else:
            nrows = 0
        table.setRowCount(nrows) # "Total" row at end
        def make_item(val):
            item = QtWidgets.QTableWidgetItem('' if val is None else str(val))
            set_item_noedit(item)
            return item

        inert_species = self.project.get_value('inert_species', default=None, args=phase)
        self.solids_inert_species_checkboxes = []
        for (row, (species,data)) in enumerate(self.solids_species[phase].items()):
            alias = data.get('alias', species) # default to species if no alias
            table.setItem(row, 0, make_item(alias))

            # mass fraction
            le = LineEdit()
            le.setdtype('dp')
            le.setValInfo(min=0.0, max=1.0)
            key = 'x_s0'
            le.key = key
            le.args = [row+1]
            val = self.project.get_value(key, args=[phase, row+1], default=None)
            if val is not None:
                le.updateValue(key, val)

            le.value_updated.connect(self.handle_solids_mass_fraction)
            table.setCellWidget(row, 1, le)

            # "Inert" checkbox
            cb = QtWidgets.QCheckBox()
            self.solids_inert_species_checkboxes.append(cb)
            cb.setChecked(row+1 == inert_species)
            cb.clicked.connect(lambda val, species_index=row+1:
                               self.handle_solids_inert_species(species_index, val))
            # center checkbox in cell
            layout =QtWidgets.QHBoxLayout()
            layout.addWidget(cb)
            layout.setAlignment(Qt.AlignCenter)
            layout.setContentsMargins(0,0,0,0)
            widget = QtWidgets.QWidget()
            widget.setLayout(layout)
            table.setCellWidget(row, 2, widget)

        if nrows > 0:
            table.setItem(nrows-1, 0, make_item("Total"))
            table.setItem(nrows-1, 1, make_item(''))
            item = table.item(nrows-1, 0)
            font = item.font()
            font.setBold(True)
            item.setFont(font)
            self.update_solids_mass_fraction_total()

        self.fixup_solids_table(table)


    def solids_species_revert(self):
        # Nothing to do, popup was operating on a copy all along
        pass


    def solids_species_save(self):
        ui = self.ui.solids
        phase = self.solids_current_phase
        if phase is None:
            return
        old_aliases = {i: data.get('alias', name)
                       for i,(name,data) in enumerate(self.solids_species[phase].items(), 1)}

        self.set_unsaved_flag()
        # Species/alias unification
        self.solids_species[phase] = OrderedDict((data.get('alias', species), deepcopy(data))
            for (species, data) in self.species_popup.defined_species.items())

        self.update_solids_species_table()
        self.update_solids_baseline_groupbox(self.solids_density_model)
        self.fixup_solids_table(ui.tablewidget_solids_species)
        self.fixup_solids_table(ui.tablewidget_solids_baseline)

        for i,(name,data) in enumerate(self.solids_species[phase].items(),1):
            old_alias = old_aliases.get(i)
            new_alias = self.species_popup.defined_species.get(name,{}).get('alias', name)
            if old_alias is None:
                continue
            if new_alias != old_alias:
                self.chemistry_rename_species(old_alias, new_alias)

        self.update_nav_tree() # Chemistry
        self.bcs_check_wall_keys() ## Set BC keys for solids species at any defined walls


    def update_solids_species_table(self):
        """Update table in solids pane.  Also sets nmax_s, species_s and species_alias_s keywords,
        which are not tied to a single widget"""
        ui = self.ui.solids
        table = ui.tablewidget_solids_species
        table.clearContents()
        phase = self.solids_current_phase
        if phase is None or self.solids_species.get(phase) is None:
            self.update_solids_species_groupbox()
            return

        nrows = len(self.solids_species[phase])
        table.setRowCount(nrows)
        def make_item(val):
            item = QtWidgets.QTableWidgetItem('' if val is None else str(val))
            set_item_noedit(item)
            return item
        old_nmax_s = self.project.get_value('nmax_s', default=0, args=phase)
        nmax_s = len(self.solids_species[phase])
        self.update_keyword('nmax_s', nmax_s, args=phase)

        for (row, (species,data)) in enumerate(self.solids_species[phase].items()):
            # order must match table headers
            args = [phase,row+1]
            for (col, key) in enumerate(('alias', 'phase', 'density', 'mol_weight', 'h_f')):
                alias = data.get('alias', species) # default to species if no alias
                data['alias'] = alias # for make_item
                table.setItem(row, col, make_item(data.get(key)))
                self.update_keyword('species_s', species, args=args)
                self.update_keyword('species_alias_s', alias, args=args)
                density = data.get('density')
                if density is not None:
                    self.update_keyword('ro_xs0', density, args=args)
                else:
                    self.unset_keyword('ro_xs0', args=args)

                # We're avoiding mw_s in favor of the settings in THERMO DATA
                #self.update_keyword('mw_s', data['mol_weight'], args=row+1)#

        # Clear any keywords with indices above nmax_s NEEDED?
        for i in range(nmax_s+1, old_nmax_s+1):
            for kw in ('species_s', 'species_alias_s', 'x_s0', 'ro_xs0'):
                self.unset_keyword(kw, args=[phase,i])
            #self.unset_keyword('mw_s', args=[phase,i]) # TBD FIXME

        # FIXME, what's the right place for this?
        self.project.update_thermo_data(self.solids_species[phase])
        self.fixup_solids_table(ui.tablewidget_solids_species)
        self.fixup_solids_table(ui.tablewidget_solids_baseline)

        # Autoselect if unique row
        for tw in (ui.tablewidget_solids_species, ui.tablewidget_solids_baseline):
            if tw.rowCount()==1 and get_selected_row(tw) is None:
                tw.setCurrentCell(0, 0)


    def handle_solids_species_selection(self):
        ui = self.ui.solids
        table = self.ui.solids.tablewidget_solids_species
        row = get_selected_row(table)
        enabled = (row is not None)
        ui.toolbutton_solids_species_delete.setEnabled(enabled)
        ui.toolbutton_solids_species_copy.setEnabled(enabled) # edit
        if enabled:
            table.doubleClicked.connect(self.solids_species_edit)
        else:
            try:
                table.doubleClicked.disconnect() #self.solids_species_edit)
            except:
                pass


    def solids_species_add(self):
        phase = self.solids_current_phase
        if phase is None:
            return
        # Workaround for deleted phase
        if phase not in self.solids_species:
            self.solids_species[phase] = OrderedDict()
        sp = self.species_popup
        sp.set_phases('SC')
        sp.do_search('')
        # how to avoid this if dialog open already?
        sp.reset_signals()
        sp.cancel.connect(self.solids_species_revert)
        sp.save.connect(self.solids_species_save)
        sp.defined_species = deepcopy(self.solids_species[phase])
        sp.extra_aliases = self.solids_make_extra_aliases(phase)
        sp.update_defined_species()
        sp.setWindowTitle("Species for %s" %self.solids_current_phase_name)
        sp.enable_density(True)
        sp.popup()


    def solids_make_extra_aliases(self, phase):
        # Construct the 'extra_aliases' set to pass to the species popup
        # Exclude the specified phase
        aliases = set(f['alias'].lower() for f in self.fluid_species.values())
        for (p, ss) in self.solids_species.items():
            if p == phase:
                continue
            aliases.update(s['alias'].lower() for s in ss.values())
        return aliases


    def solids_species_delete(self):
        phase = self.solids_current_phase
        if phase is None:
            return
        ui = self.ui.solids

        tw = ui.tablewidget_solids_species
        row = get_selected_row(tw)
        if row is None: # No selection
            return

        alias = tw.item(row,0).text()
        msg = self.solids_check_species_in_use(phase, alias)
        if msg:
            self.message(text="%s is used in %s " % (alias, msg))
            return

        tw.clearSelection() #?

        # NB Must remove from solids species before calling 'delete_species_keys'
        species_index = 1 + list(self.solids_species[phase].keys()).index(alias)
        self.solids_species[phase].pop(alias, None)
        self.solids_delete_species_keys(phase, species_index)

        self.update_solids_species_table()
        self.update_solids_baseline_groupbox(self.solids_density_model)
        self.fixup_solids_table(ui.tablewidget_solids_species)
        self.fixup_solids_table(ui.tablewidget_solids_baseline)

        self.update_nav_tree() # Chemistry

        # Sigh, we have to update the row in the popup too.
        # Should the popup just be modal, to avoid this?
        sp = self.species_popup
        sp.defined_species = deepcopy(self.solids_species[phase])
        sp.update_defined_species()


    def solids_species_edit(self):
        phase = self.solids_current_phase
        if phase is None:
            return
        ui = self.ui.solids
        table = ui.tablewidget_solids_species
        row = get_selected_row(table)
        sp = self.species_popup
        sp.set_phases('SC')
        sp.reset_signals()
        sp.cancel.connect(self.solids_species_revert)
        sp.save.connect(self.solids_species_save)
        sp.defined_species = deepcopy(self.solids_species[phase])
        sp.extra_aliases = self.solids_make_extra_aliases(phase)
        sp.update_defined_species()
        if row is None:
            sp.do_search('')
            sp.tablewidget_defined_species.clearSelection()
        else:
            # Initialize search box to current species (?)
            sp.do_search(list(self.solids_species[phase].keys())[row])
            sp.tablewidget_defined_species.setCurrentCell(row, 0)
        sp.setWindowTitle("Species for %s" % self.solids_current_phase_name)
        sp.enable_density(True)
        sp.popup()


    def solids_check_species_in_use(self, phase, species):
        """return False if OK to delete given species, else a string indicating
        what species is referenced by (B,C chem eq, etc)"""
        msg = self.chemistry_check_species_in_use(species)
        if msg:
            return("reaction %s" % msg)

        species_num = 1 + list(self.solids_species[phase].keys()).index(species) # :(

        for key in keyword_args.keys_by_type['species']:
            indices = self.project.get_key_indices(key)
            if not indices: #Keys not set
                continue
            arg_types = keyword_args.keyword_args[key]
            if 'phase' not in arg_types: # It's a fluid species, not solid
                continue
            if arg_types == ('phase', 'species'): # This will be deleted
                continue
            phase_pos = arg_types.index('phase')
            species_pos = arg_types.index('species')
            for args in indices:
                if args[phase_pos] != phase or args[species_pos] != species_num:
                    continue
                if self.project.get_value(key, args=args): # Ignore settings of None, False, or 0
                    return format_key_with_args(key,args)

            return False # Ok to delete, no refs


    def solids_check_phase_in_use(self, phase):
        """return False if OK to delete phase, else a string indicating
        what object holds a reference to this phase (BC, chem eq, etc)"""

        if self.get_nscalar_phase(phase) > 0:
            return "scalar equations"

        for species in self.solids_species[phase]:
            msg = self.solids_check_species_in_use(phase, species)
            if msg:
                return ("%s (%s)" % (msg, species))

        for key in keyword_args.keys_by_type['phase']:
            indices = self.project.get_key_indices(key)
            if not indices:
                continue
            arg_types = keyword_args.keyword_args[key]

            if arg_types == ('phase',):
                # Single reference is OK.  We will
                # delete these keys along with the phase in solids_delete_phase_keys
                continue
            if arg_types == ('phase', 'species'):
                # These will all be deleted when we delete the phase
                continue
            phase_pos = arg_types.index('phase')

            for args in indices:
                if args[phase_pos] != phase:
                    continue
                val = self.project.get_value(key, args=args)
                if val is not None:
                # TODO check if value is default and if so, allow delete
                    if val != 0.0: # Assume 0 is default
                        return format_key_with_args(key,args)

        return False # Ok to delete phase, no refs


    def solids_delete_species_keys(self, phase_index, species_index):
        """Delete all keywords associated with specified species,
        fixing up the resulting gap in sequence"""
        prev_size = len(self.solids_species[phase_index]) + 1 # Size before species deleted
        for key in keyword_args.keys_by_type['species']:
            indices = self.project.get_key_indices(key)
            if not indices:
                continue
            arg_types = keyword_args.keyword_args[key]
            if 'phase' not in arg_types: # fluid species
                continue
            phase_pos = arg_types.index('phase')
            species_pos = arg_types.index('species')
            new_vals = {}
            for args in indices:
                args_phase = args[phase_pos]
                args_species = args[species_pos]
                if args_phase != phase_index:
                    continue
                new_args = list(args)
                if args_species > species_index:
                    new_args[species_pos] -= 1 #Slide along 'species_pos' axis
                new_vals[tuple(new_args)] = self.project.get_value(key, args=args)
            for (args, val) in new_vals.items():
                self.update_keyword(key, val, args=args)
            for args in indices: # Trim
                key_phase = args[phase_pos]
                key_species = args[species_pos]
                if (key_phase, key_species) == (args_phase, prev_size):
                    self.unset_keyword(key, args)


    def solids_delete_phase_keys(self, phase):
        """Delete all keywords associated with specified phase (1-based index),
        fixing up the resulting gap in sequence"""
        prev_size = len(self.solids) + 1 # Size before row deleted
        for key in keyword_args.keys_by_type['phase']:
            arg_types = keyword_args.keyword_args[key]
            indices = self.project.get_key_indices(key)
            if not indices:
                continue

            if arg_types == ('phase',):
                # Copy/slide/trim
                vals = [self.project.get_value(key, args=i)
                        for i in range(1, 1+prev_size)]
                del vals[phase-1] # Slide (1-based)
                for (i, val) in enumerate(vals, 1):
                    self.update_keyword(key, val, args=i)
                self.unset_keyword(key, args=prev_size) #Trim off the end

            elif arg_types == ('phase', 'phase'):
                tmp_data = {}
                for args in indices:
                    p1, p2 = args
                    p1a = p1-1 if p1>=phase else p1
                    p2a = p2-1 if p2>=phase else p2
                    tmp_data[(p1a,p2a)] = self.project.get_value(key, args=args)
                    if (p1==prev_size or p2==prev_size):
                        self.unset_keyword(key, args=(p1, p2))
                for (args, val) in tmp_data.items():
                    self.update_keyword(key, val, args=args)
            else:
                phase_pos = arg_types.index('phase')
                new_vals = {}
                for args in indices:
                    args_phase = args[phase_pos]
                    if args_phase < phase: # Unaffected
                        continue
                    if args_phase == phase:
                        continue # skip the phase we're deleting
                    new_args = list(args)
                    if args_phase > phase:
                        new_args[phase_pos] -= 1 #Slide along 'phase_pos' axis
                    new_vals[tuple(new_args)] = self.project.get_value(key, args=args)
                for (args, val) in new_vals.items():
                    self.update_keyword(key, val, args=args)
                for args in indices: # Trim
                    args_phase = args[phase_pos]
                    if args_phase == prev_size:
                        self.unset_keyword(key, args)


    def reset_solids(self):
        ui = self.ui.solids
        # Set all solid-related state back to default
        self.init_solids_default_models()
        self.solids_current_phase = self.P = None
        self.solids_current_phase_name = None
        self.solids.clear()
        self.solids_species.clear()
        self.update_solids_table()
        self.update_solids_detail_pane()
        self.solids_current_tab = 0
        ui.toolbutton_solids_add.setEnabled(True)
        self.solids_nscalar_eq = None
        self.solids_change_tab(0, ui.pushbutton_solids_materials)
        # TODO (?)  reset THERMO_DATA ?
