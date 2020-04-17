# methods to deal with fluid phase
from copy import deepcopy
from collections import OrderedDict

from qtpy import QtWidgets

from mfixgui import default_values
from mfixgui.constants import *
from mfixgui.species_handler import SpeciesHandler
from mfixgui.tools import format_key_with_args, keyword_args
from mfixgui.tools.qt import get_selected_row, set_item_noedit

class FluidHandler(SpeciesHandler):
    # Defaults
    def init_fluid_default_models(self):
        self.fluid_density_model = CONSTANT
        self.fluid_viscosity_model = CONSTANT
        self.fluid_mol_weight_model = CONSTANT
        self.fluid_specific_heat_model = CONSTANT
        self.fluid_conductivity_model = AIR
        self.fluid_diffusion_model = AIR


    ## Fluid phase methods
    def enable_fluid_species_eq(self, enabled):
        ui = self.ui.fluid
        for item in (ui.combobox_fluid_diffusion_model,
                     ui.label_fluid_diffusion_model,
                     # more ?
                     ):
            item.setEnabled(enabled)
        # dif_g0 == diffusion coeff model
        items = (ui.lineedit_keyword_dif_g0,
                 ui.label_dif_g0_units)
        for item in items:
            item.setEnabled(enabled and (self.fluid_diffusion_model == CONSTANT))


    def enable_fluid_scalar_eq(self, enabled):
        ui = self.ui.fluid
        cb = ui.checkbox_enable_scalar_eq
        sb = ui.spinbox_nscalar_eq
        cb.setChecked(enabled)
        val = 1 if enabled else 0
        sb.setMinimum(val)
        sb.setValue(val)
        sb.setEnabled(enabled)
        #self.set_nscalar_phase(val, 0) #handled by sb callback


    def init_fluid_handler(self):
        self.fluid_species = OrderedDict() # keyed by ALIAS

        ui = self.ui.fluid

        ui.lineedit_fluid_phase_name.default_value = self.fluid_phase_name = "Fluid"

        self.init_fluid_default_models()
        # Handle a number of cases which are essentially the same
        # see 'set_fluid_mol_weight_model' below to help understand this
        def make_fluid_model_setter(self, name, key):
            def setter(model):
                ui = self.ui.fluid
                setattr(self, name, model) # self.fluid_<name>_model = model
                combobox = getattr(ui, 'combobox_' + name)
                prev_model = combobox.currentIndex()
                if model != prev_model:
                    combobox.setCurrentIndex(model)
                # Make tooltip match setting (for longer names which are truncated)
                combobox.setToolTip(combobox.currentText())

                # Enable lineedit for constant model
                key_g0 = 'c_pg0' if key=='c_p' else key + '_g0'
                key_usr = 'usr_cpg' if key=='c_p' else 'usr_' + key + 'g'
                lineedit = getattr(ui, 'lineedit_keyword_%s' % key_g0)
                label = getattr(ui, 'label_%s_units' % key_g0)

                for item in (lineedit, label):
                    item.setEnabled(model==CONSTANT)

                # Workaround for disabled fluid solver
                if self.fluid_solver_disabled and key_g0 == 'ro_g0':
                    lineedit.minimum = 0
                    lineedit.saved_value = 0.0
                    return

                if model == CONSTANT:
                    if key_g0 == 'ro_g0':
                        if lineedit.saved_value is None or lineedit.saved_value == 0.0:
                            lineedit.saved_value = default_values.ro_g0 # fallback
                    value = lineedit.value # Possibly re-enabled gui item
                    if value != '' and self.project.get_value(key_g0) != value:
                        self.update_keyword(key_g0, value) # Restore keyword value
                    self.unset_keyword(key_usr) # Issues/435
                elif model == UDF:
                    self.unset_keyword(key_g0)
                    self.update_keyword(key_usr, True)
                else: # Ideal gas law, Sutherland, etc
                    self.unset_keyword(key_g0)
                    self.unset_keyword(key_usr)
                if key_g0 == 'ro_g0':
                    if model == CONSTANT:
                        lineedit.required = True
                        lineedit.exclude_endpoints = True
                        if not lineedit.text():
                            lineedit.saved_value = default_values.ro_g0
                            lineedit.updateValue(key_g0, default_values.ro_g0)
                            self.update_keyword(key_g0, default_values.ro_g0)
                    else:
                        lineedit.required = False
                        lineedit.exclude_endpoints = False
                        lineedit.setText('')
                # anything else to do in this case? validation?

            return setter

        # Create setters for the cases which are similar (mol. wt. handled separately)
        for (name, key) in (
                ('density', 'ro'),
                ('viscosity', 'mu'),
                ('specific_heat', 'c_p'),
                ('conductivity', 'k'),
                ('diffusion', 'dif')):
            model_name = 'fluid_%s_model' % name
            setattr(self, 'set_'+model_name, make_fluid_model_setter(self, model_name, key))

            # Set the combobox default value (?)
            combobox = getattr(ui, 'combobox_'+model_name)
            combobox.default_value = getattr(self, model_name)
            #print(model_name, combobox.default_value)

        combobox = ui.combobox_fluid_mol_weight_model
        combobox.default_value = self.fluid_mol_weight_model

        # more stuff moved from mfixgui.__init__
        checkbox = ui.checkbox_keyword_species_eq_args_0
        checkbox.clicked.connect(self.enable_fluid_species_eq)

        ui.lineedit_fluid_phase_name.value_updated.connect(
            self.handle_fluid_phase_name)
        ui.checkbox_enable_scalar_eq.clicked.connect(
            self.enable_fluid_scalar_eq)
        ui.spinbox_nscalar_eq.valueChanged.connect(
            lambda val: self.set_nscalar_phase(val, phase=0))

        # Fluid phase models
        for name in ('density', 'viscosity', 'specific_heat', 'mol_weight',
                     'conductivity', 'diffusion'):
            model_name = 'fluid_%s_model' % name
            combobox = getattr(ui, 'combobox_%s' % model_name)
            setter = getattr(self,'set_%s' % model_name)
            combobox.currentIndexChanged.connect(setter)

        # Fluid species
        tb = ui.toolbutton_fluid_species_add
        tb.clicked.connect(self.fluid_species_add)
        tb = ui.toolbutton_fluid_species_copy # misnomer
        tb.clicked.connect(self.fluid_species_edit)
        tb.setEnabled(False)
        tb = ui.toolbutton_fluid_species_delete
        tb.setEnabled(False)
        tb.clicked.connect(self.fluid_species_delete)
        tw = ui.tablewidget_fluid_species
        tw.itemSelectionChanged.connect(self.handle_fluid_species_selection)
        self.fixup_fluid_table()
        #ui.lineedit_keyword_ro_g0.required = True # Only required when constant density
        self.add_tooltip(ui.checkbox_enable_scalar_eq, key=None,
                         description='Enable scalar equations for fluid phase')
        self.add_tooltip(ui.spinbox_nscalar_eq, key=None,
                         description='Number of scalar equations for fluid phase')


    def fixup_fluid_table(self):
        hv = QtWidgets.QHeaderView
        ui = self.ui.fluid
        tw = ui.tablewidget_fluid_species
        resize = tw.horizontalHeader().setSectionResizeMode
        for n in range(tw.columnCount()):
            resize(n, hv.ResizeToContents if n>0
                   else hv.Stretch)


    # molecular wt model only has 2 choices, and the key names don't
    # follow the same pattern, so create its setter specially
    def set_fluid_mol_weight_model(self, model):
        ui = self.ui.fluid
        self.fluid_mol_weight_model = model
        combobox = ui.combobox_fluid_mol_weight_model
        # Make tooltip match setting (for longer names which are truncated)
        combobox.setToolTip(combobox.currentText())
        prev_model = combobox.currentIndex()
        if model != prev_model:
            combobox.setCurrentIndex(model)
        # Enable lineedit for constant mol_weight model
        lineedit = ui.lineedit_keyword_mw_avg
        label = ui.label_mw_avg_units
        for item in (lineedit, label):
            item.setEnabled(model==CONSTANT)
        if model == CONSTANT:
            value = lineedit.value # Possibly re-enabled gui item
            if value != '' and self.project.get_value("mw_avg") != value:
                self.update_keyword("mw_avg", value) # Restore keyword value
        else: # Mixture
            # TODO: validate, require mw for all component species
            self.unset_keyword("mw_avg")


    def handle_fluid_phase_name(self, widget, value_dict, args):
        ui = self.ui.fluid
        le = ui.lineedit_fluid_phase_name
        old_name = self.fluid_phase_name
        new_name = le.text()
        if new_name in self.solids: # Reject the input
            self.warning("%s: name is in use" % new_name, popup=True)
            le.setText(old_name)
        else:
            self.set_fluid_phase_name(new_name)


    def set_fluid_phase_name(self, value):
        if value != self.ui.fluid.lineedit_fluid_phase_name.text():
            self.ui.fluid.lineedit_fluid_phase_name.setText(value) # set GUI state
        if self.fluid_phase_name == value:
            return
        self.fluid_phase_name = value
        if self.project.mfix_gui_comments.get('fluid_phase_name') != value:
            self.project.mfix_gui_comments['fluid_phase_name'] = value
            self.set_unsaved_flag()

    def fluid_species_revert(self):
        pass

    def fluid_species_save(self):
        self.set_unsaved_flag()
        old_aliases = {i: data.get('alias', name)
                       for i,(name,data) in enumerate(self.fluid_species.items(), 1)}

        # Species/alias unification
        self.fluid_species = OrderedDict((data.get('alias', species), deepcopy(data))
            for (species,data) in self.species_popup.defined_species.items())

        self.update_fluid_species_table()

        for i,(name,data) in enumerate(self.fluid_species.items(),1):
            old_alias = old_aliases.get(i)
            new_alias = self.species_popup.defined_species.get(name,{}).get('alias', name)
            if old_alias is None:
                continue
            if new_alias != old_alias:
                self.chemistry_rename_species(old_alias, new_alias)

        self.update_nav_tree() # Chemistry


    def update_fluid_species_table(self):
        """Update table in fluid pane.  Also set nmax_g, species_g and species_alias_g keywords,
        which are not tied to a single widget"""
        tw = self.ui.fluid.tablewidget_fluid_species
        tw.clearContents()
        if self.fluid_species is None:
            self.fixup_fluid_table()
            return
        nrows = len(self.fluid_species)
        tw.setRowCount(nrows)
        def make_item(val):
            item = QtWidgets.QTableWidgetItem('' if val is None else str(val))
            set_item_noedit(item)
            return item
        old_nmax_g = self.project.get_value('nmax_g')
        nmax_g = len(self.fluid_species)
        if nmax_g > 0:
            self.update_keyword('nmax_g', nmax_g)
        else:
            self.unset_keyword('nmax_g')
        for (row, (species,data)) in enumerate(self.fluid_species.items()):
            for (col, key) in enumerate(('alias', 'phase', 'mol_weight', 'h_f')):
                alias = data.get('alias', species) # default to species if no alias
                data['alias'] = alias # for make_item
                tw.setItem(row, col, make_item(data.get(key)))
                # Fixme, we should not be setting keywords in a 'update_table' method
                self.update_keyword('species_g', species, args=row+1)
                self.update_keyword('species_alias_g', alias, args=row+1)
                # We're avoiding mw_g in favor of the settings in THERMO DATA
                #self.update_keyword('mw_g', data['mol_weight'], args=row+1)#

        # Clear any keywords with indices above nmax_g
        if old_nmax_g is None:
            old_nmax_g = 0
        for i in range(nmax_g+1, old_nmax_g+1):
            self.unset_keyword('species_g', args=i)
            self.unset_keyword('species_alias_g', args=i)
            #self.unset_keyword('mw_g', i) # TBD

        self.project.update_thermo_data(self.fluid_species)
        self.fixup_fluid_table()


    def handle_fluid_species_selection(self):
        ui = self.ui.fluid
        tw = ui.tablewidget_fluid_species
        row = get_selected_row(tw)
        enabled = (row is not None)
        ui.toolbutton_fluid_species_delete.setEnabled(enabled)
        ui.toolbutton_fluid_species_copy.setEnabled(enabled)
        if enabled:
            tw.doubleClicked.connect(self.fluid_species_edit)
        else:
            try:
                tw.doubleClicked.disconnect() #self.fluid_species_edit)
            except:
                pass


    def fluid_species_add(self):
        sp = self.species_popup
        sp.set_phases('GL')
        sp.do_search('') # Init to full db
        # how to avoid this if dialog open already?
        sp.reset_signals()
        sp.cancel.connect(self.fluid_species_revert)
        sp.save.connect(self.fluid_species_save)
        sp.defined_species = deepcopy(self.fluid_species)
        sp.extra_aliases = self.fluid_make_extra_aliases()
        sp.update_defined_species()
        sp.setWindowTitle("Fluid Species")
        sp.enable_density(False)
        sp.popup()


    def fluid_make_extra_aliases(self):
        # Construct the 'extra_aliases' set to pass to the species popup
        # Exclude the fluid phase
        aliases = set()
        for ss in self.solids_species.values():
            aliases.update(set(s['alias'].lower() for s in ss.values()))
        return aliases


    def fluid_species_delete(self):
        ui = self.ui.fluid
        tw = ui.tablewidget_fluid_species
        row = get_selected_row(tw)
        if row is None: # No selection
            return

        alias = tw.item(row,0).text()
        msg = self.fluid_check_species_in_use(alias)
        if msg:
            self.message(text="%s is used in %s " % (alias, msg))
            return

        tw.clearSelection() #?
        species_index = 1 + list(self.fluid_species.keys()).index(alias)
        self.bcs_delete_fluid_species(species_index) # special handling for memoized eq_type
        self.fluid_species.pop(alias, None)
        self.fluid_delete_species_keys(species_index) # Must remove fluid species first (why?)

        self.update_fluid_species_table()
        # Sigh, we have to update the row in the popup too.
        # Should the popup just be modal, to avoid this?
        sp = self.species_popup
        sp.defined_species = deepcopy(self.fluid_species)
        sp.update_defined_species()
        self.update_nav_tree() # Chemistry

    def fluid_species_edit(self):
        tw = self.ui.fluid.tablewidget_fluid_species
        row = get_selected_row(tw)
        sp = self.species_popup
        sp.set_phases('GL')
        sp.reset_signals()
        sp.cancel.connect(self.fluid_species_revert)
        sp.save.connect(self.fluid_species_save)
        sp.defined_species = deepcopy(self.fluid_species)
        sp.extra_aliases = self.fluid_make_extra_aliases()
        sp.update_defined_species()
        if row is None:
            sp.tablewidget_defined_species.clearSelection()
        else:
            sp.tablewidget_defined_species.setCurrentCell(row, 0)
        sp.setWindowTitle("Fluid Species")
        sp.enable_density(False)
        sp.popup()


    def fluid_check_species_in_use(self, species):
        """return False if OK to delete given species, else a string indicating
        what species is referenced by (BC, chem eq, etc)"""
        msg = self.chemistry_check_species_in_use(species)
        if msg:
            return("reaction %s" % msg)

        species_num = 1 + list(self.fluid_species.keys()).index(species) # :(

        for key in keyword_args.keys_by_type['species']:
            indices = self.project.get_key_indices(key)
            if not indices: #Keys not set
                continue

            arg_types = keyword_args.keyword_args[key]
            if 'phase' in arg_types: # It's a solid species, not fluid
                continue
            if arg_types == ('species',): # This will be deleted
                continue
            species_pos = arg_types.index('species')
            for args in indices:
                if args[species_pos] != species_num:
                    continue
                if self.project.get_value(key, args=args): # Ignore settings of None, False, or 0
                    return format_key_with_args(key,args)

            return False # Ok to delete, no refs


    def fluid_delete_species_keys(self, species):
        """Delete all keywords associated with specified species,
        fixing up the resulting gap in sequence"""
        prev_size = len(self.fluid_species) + 1 # Size before species deleted
        for key in keyword_args.keys_by_type['species']:
            indices = self.project.get_key_indices(key)
            if not indices:
                continue
            arg_types = keyword_args.keyword_args[key]
            if 'phase' in arg_types: # solids species
                continue
            species_pos = arg_types.index('species')
            new_vals = {}
            for args in indices:
                args_species = args[species_pos]
                new_args = list(args)
                if args_species > species:
                    new_args[species_pos] -= 1 #Slide along 'species_pos' axis
                new_vals[tuple(new_args)] = self.project.get_value(key, args=args)
            for (args, val) in new_vals.items():
                self.update_keyword(key, val, args=args)
            for args in indices: # Trim
                args_species = args[species_pos]
                if args_species == prev_size:
                    self.unset_keyword(key, args)


    def setup_fluid(self):
        # Called whenever we switch to fluid tab
        self.P = 0
        ui = self.ui.fluid
        tw = ui.tablewidget_fluid_species
        # Autoselect if unique row
        if get_selected_row(tw) is None and tw.rowCount() == 1:
            tw.setCurrentCell(0,0)
        cb = ui.checkbox_enable_scalar_eq
        sb = ui.spinbox_nscalar_eq
        nscalar_phase = self.get_nscalar_phase(0)
        enabled = (nscalar_phase > 0)
        cb.setChecked(enabled)
        sb.setEnabled(enabled)
        # Lower minimum before setting value
        if not enabled:
            sb.setMinimum(0)
        sb.setValue(nscalar_phase)
        # Raise minimum after value is set to avoid spurious callback
        if enabled:
            sb.setMinimum(1)


    def reset_fluid(self):
        # Set all fluid-related state back to default
        ui = self.ui.fluid
        self.fluid_phase_name = 'Fluid'
        self.fluid_species.clear()
        self.init_fluid_default_models()
        le = ui.lineedit_keyword_ro_g0
        cb = ui.combobox_fluid_density_model
        le.required = False
        le.minimum = 0.0
        le.saved_value = default_values.ro_g0 # fallback
        le.updateValue('ro_g0', le.saved_value)
        # TODO remove dynamically created input widgets, although this should
        #  get handled next time we call 'setup'

#Fluid phase Task Pane Window: (unavailable if fluid phase was disable)
#    Option to rename the phase (e.g, air, gas)

#    Option to disable Momentum Equations (enabled by default)
# Sets keyword: MOMENTUM_X/Y/Z_EQ(0)

#    Option to enable Species Equations
# Sets keyword: SPECIES_EQ(0)

#    Option to enable scalar equations
# Define the number of scalar equations
# Value sums into keyword NSCALAR
# Sets keyword PHASE4SCALAR(*)=0 for total listed scalars

#    Select Density Model:
# Selection always available
# Available selections:
#  Constant: [DEFAULT]
#    Selection always available
#    Specify constant gas density, RO_G0
#  Ideal gas law:
#    Selection always available
#    Keyword RO_G0 must be undefined

#    Requires a fluid phase molecular weight
#    Requires temperature field for full domain
#  UDF
#    Selection is always available
#    Sets keyword USR_ROg
#    MFIX runtime check verifies UDF was provided

#Select Viscosity Model:
# Selection always available
# Available selections:
#  Constant: [DEFAULT]
#    Selection always available
#    Specify constant gas viscosity, MU_G0
#  Sutherland's law
#    Selection always available
#    Keyword MU_G0 must be undefined
#    Requires temperature field for full domain
#  UDF
#    Selection always available
#    Sets keyword USR_MUg
#    MFIX runtime check verifies UDF was provided

#Select Molecular Weight Model:
# Selection always available
# Available selections:
#  Constant; [DEFAULT]
#    Specification always available
#    Specify constant molecular weight, MW_AVG
#  Mixture:
#    Selection always available
#    Requires molecular weights for all species components

#Select Specific Heat Model:
# Selection available only when solving thermal energy equations
# Available selections:
#  Constant; [DEFAULT]
#    Selection always available
#    Specify constant fluid phase specific heat, C_PG0
#  Mixture:
#    Selection always available
#    Keyword C_PG0 must be undefined
#    Requires specific heats for all species components
#  UDF
#    Selection always available
#    Sets keyword USR_CPg
#    MFIX runtime check verifies UDF was provided

#Select Thermal Conductivity Model:
# Selection only available when solving thermal energy equations
# Available selections:
#  Constant
#    Selection always available
#    Specify constant thermal conductivity, K_G0
#  Temperature dependent (air); [DEFAULT]
#    Selection always available
#    Keyword K_G0 must be undefined
#  UDF
#    Selection always available
#    Set keyword USR_KG
#    MFIX runtime check verifies UDF was provided

#Select Diffusion Coefficient Model:
# Selection only available when solving species equations
# Available selections:
#  Constant
#    Selection always available
#    Specify a constant diffusion coefficient, DIF_G0
#  Dilute Mixture Approximation (air); [DEFAULT]
#    Selection always available
#    Keyword DIF_G0 must be undefined
#    Requires temperature field for full domain
#  UDF
#    Selection always available
#    Sets keyword USR_DIFG
#    MFIX runtime check verifies UDF was provided

#Fluid phase species selection:
# Species data required under any of the following conditions:
#  Solving species equations
#  Density model is the ideal gas law with mixture molecular weight model
#  Energy equations are solved with mixture specific heat model
# Specification panel operates as a popup window triggered by an Add/Edit button
# Summary window provides a list of the species and an overview of some properties

#Fluid phase Material Database window (popup):
#    Select database (BURCAT); later could link in other databases.
#    Capability to search selected database for chemical name
#    Import from database copies the usable information from the database into a new entry in the 'run database'
#    New creates a new 'blank' species in the 'run database' where the user must supply all the thermochemical data.
#    Delete removes an entry from the 'run database'

#NOTE: The gas phase species molecular weights, MW_G(#) cannot be directly specified. This
#keyword is not needed because users can edit the molecular weight in the material database popup
#window.
