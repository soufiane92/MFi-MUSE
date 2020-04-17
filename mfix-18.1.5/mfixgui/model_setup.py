# -*- coding: utf-8 -*-

from mfixgui.constants import *
from mfixgui import default_values
from mfixgui.tools.qt import get_combobox_item

class ModelSetup(object):
    #    Model Setup Task Pane Window: Select MFIX solver and other conservation equations

    def init_model_setup(self):
        ui = self.ui.model_setup
        ui.combobox_solver.activated.connect(self.set_solver)

        ui.checkbox_disable_fluid_solver.clicked.connect(self.disable_fluid_solver)

        ui.checkbox_keyword_energy_eq.clicked.connect(self.enable_energy_eq)

        key = 'turbulence_model'
        cb = ui.combobox_turbulence_model
        cb.activated.connect(self.set_turbulence_model)
        for (i, value) in enumerate(TURBULENCE_MODELS):
            item = get_combobox_item(cb, i)
            self.add_tooltip(item, key, value=value)

        key = 'drag_type'
        cb = ui.combobox_drag_type
        cb.activated.connect(self.set_drag_type)
        assert ui.combobox_drag_type.count() == len(DRAG_TYPES)
        for item in (ui.label_drag_type, cb):
            self.add_tooltip(item, key)
        for (i, val) in enumerate(DRAG_TYPES):
            item = get_combobox_item(cb, i)
            self.add_tooltip(item, key=key, description=self.keyword_doc[key]['valids'][val]['note'], value=val)

        cb = ui.combobox_momentum_formulation
        cb.activated.connect(self.set_momentum_formulation)
        self.add_tooltip(ui.label_momentum_formulation, key=None, description=self.keyword_doc['model_b']['description'])

        for i in range(4):
            item = get_combobox_item(cb, i)
            if i == 0:
                key = 'model_b'
                value = False
            elif i == 1:
                key = 'model_b'
                value = True
            elif i == 2:
                key = 'jackson'
                value = None
            else:
                key = 'ishii'
                value = None
            self.add_tooltip(item, key=key, value=value)

        key = 'subgrid_type'
        cb = ui.combobox_subgrid_type
        cb.activated.connect(self.set_subgrid_type)
        for (i, value) in enumerate(SUBGRID_TYPES):
            if value == 'NONE':
                self.add_tooltip(get_combobox_item(cb, i), key, value='None', description='No subgrid model')
            else:
                self.add_tooltip(get_combobox_item(cb, i), key, value=value.title())
        self.fluid_solver_disabled = False # squelch pylint access-member-before-definition


    def set_solver(self, solver):
        #    Select MFIX Solver:
        # Available selections:
        #  Single phase
        #    Selection disables 'Solids' task pane menu
        #    Selection disables 'Continuum Solids Model' task pane menu
        #    Selection disables 'Discrete Element Model' task pane menu
        #    Selection disables 'Particle-in-Cell' task pane menu
        #  MFIX-TFM
        #    Selection enables 'Solids' task pane menu
        #    Selection enables 'Continuum Solids Model' task pane menu
        #  MFIX-DEM
        #    Selection enables 'Solids' task pane menu
        #    Selection enables 'Discrete Element Model' task pane menu
        #  MFIX-PIC
        #    Selection enables 'Solids' task pane menu
        #    Selection enables 'Particle-in-Cell' task pane menu
        #  MFIX-Hybrid
        #    Selection enables 'Solids' task pane menu
        #    Selection enables 'Continuum Solids Model' task pane menu
        #    Selection enables 'Discrete Element Model' task pane menu


        self.project.solver = solver
        if solver is None: #
            return

        # Issues/440 hide PIC and Hybrid options unless they were already present
        # Must do this first before setting up UI since some items may have been
        #  removed
        if self.project.solver in (PIC, HYBRID):
            self.enable_pic()
        else:
            # Don't disable PIC once it's been enabled in a project
            #self.disable_pic() # this is done in 'reset'
            pass

        ui = self.ui.model_setup
        cb = ui.combobox_solver
        if cb.currentIndex != solver:
            cb.setCurrentIndex(solver)

        solver_name = {SINGLE:"MFiX Single-Phase",
                       TFM:"MFiX-TFM",
                       DEM:"MFiX-DEM",
                       PIC:"MFiX-PIC",
                       HYBRID:"MFiX-Hybrid"}.get(solver, "MFiX")

        self.print_internal("Solver: %s" % solver_name)
        self.solver_name = solver_name

        enabled = (solver != SINGLE)
        self.find_navigation_tree_item("Solids").setDisabled(not enabled)
        # issues/583 Prevent fluid solver disabling for pure fluid runs
        ui.checkbox_disable_fluid_solver.setEnabled(enabled)
        if solver == SINGLE:
            self.fluid_solver_disabled = False

        if enabled:
            self.solids_update_tabs()

        # Solids Model selection tied to Solver
        valid_models = (("DEM",) if solver==DEM
                        else ("TFM",) if solver==TFM
                        else ("PIC",) if solver==PIC
                        else ("TFM", "DEM"))

        for (i,(k,v)) in enumerate(self.solids.items(), 1):
            model = v.get('model')
            if model not in valid_models:
                model = valid_models[0]
                self.update_keyword('solids_model', model, args=[i])
                v['model'] = model

        #self.update_solids_table() # some of these settings are dependent on solver
        #  but we'll update this when we switch to the solids pane
        #self.setup_combobox_solids_model()

        self.setup_model_setup()

        #self.update_solids_detail_pane()
        self.update_window_title()
        self.update_nav_tree()


    def setup_model_setup(self):
        ui = self.ui.model_setup
        solver = self.project.solver
        ui.checkbox_disable_fluid_solver.setChecked(self.fluid_solver_disabled)
        self.add_tooltip(ui.checkbox_disable_fluid_solver, key=None, description="""Granular flow in vacuum:
Sets ro_g0 (fluid density) to 0 and disables fluid momentum equations for faster convergence""")
        self.disable_fluid_solver(self.fluid_solver_disabled)

        # Don't allow setting single-phase once solids are defined
        cb = ui.combobox_solver
        get_combobox_item(cb, 0).setEnabled(not self.solids)

        key = 'turbulence_model'
        turbulence_model = self.project.get_value(key, default=DEFAULT_TURBULENCE_MODEL)
        if turbulence_model not in TURBULENCE_MODELS:
            self.error("Invalid turbulence model %s" % turbulence_model)
            turbulence_model = DEFAULT_TURBULENCE_MODEL
            self.update_keyword(key, turbulence_model)
        else:
            self.set_turbulence_model(TURBULENCE_MODELS.index(turbulence_model))

        #Specify Gravitational acceleration
        #    Specification always available
        #    Sets keywords GRAVITY_X, GRAVITY_Y, GRAVITY_Z
        #    DEFAULT values
        # handled by keyword widgets

        #  Leave blank when converting dat files to mfx
        #  New cases take the values from the mfix.dat.template
        # Implementation Note:  Not doing this.  There's not a
        # strong reason for it, and we don't distinguish converting dat files from opening existing projects.


        #    If keyword GRAVITY is specified
        #  GRAVITY_Y = -GRAVITY
        #  GRAVITY_X = GRAVITY_Z = 0.0


        #Specify drag model
        # Selection requires TFM, DEM, or PIC solver
        enabled = self.project.solver in (TFM, DEM, PIC)

        for item in (ui.label_drag_type, ui.combobox_drag_type):
            item.setEnabled(enabled)

        val = self.project.get_value('drag_type')
        if val is None:
            idx = DRAG_TYPES.index(DEFAULT_DRAG_TYPE)
        else:
            if not val in DRAG_TYPES:
                self.error('Invalid drag_type %s' % val)
                idx = DRAG_TYPES.index(DEFAULT_DRAG_TYPE)
            else:
                idx = DRAG_TYPES.index(val)
        ui.combobox_drag_type.setCurrentIndex(idx)
        drag_type = DRAG_TYPES[idx]

        # SYAM_OBRIEN
        #    Specify model parameter: DRAG_C1
        #      DEFAULT 0.8
        #    Specify model parameter: DRAG_D1
        #      DEFAULT 2.65
        enabled = self.project.solver in (TFM, DEM, PIC) and drag_type == 'SYAM_OBRIEN'
        for item in (ui.label_syam_obrien,
                     ui.label_drag_c1, ui.lineedit_keyword_drag_c1,
                     ui.label_drag_d1, ui.lineedit_keyword_drag_d1):
            item.setEnabled(enabled)
        if enabled:
            for (key, default) in (('drag_c1', 0.8), ('drag_d1', 2.65)):
                val = self.project.get_value(key)
                if val is None:
                    val = default
                    self.update_keyword(key, val)

        # HYS
        #    Specify model parameter LAM_HYS
        #      DEFAULT 1.0e-6 (meters)
        key = 'lam_hys'
        default = 1.0e-6
        enabled =  self.project.solver in (TFM, DEM, PIC) and drag_type == 'HYS'
        for item in (ui.label_lam_hys, ui.lineedit_keyword_lam_hys,
                     ui.label_lam_hys_units):
            item.setEnabled(enabled)

        if enabled:
            val = self.project.get_value(key)
            if val is None:
                val = default
                self.update_keyword(key, val)


        #Specify momentum equation formulation; Select Model A, Model B; Jackson, Ishii
        #MODEL_B
        #[ .FALSE. ] -------------------------- Use Model A
        #.TRUE. ----------------------------- Use Model B.
        model_b = self.project.get_value('model_b', default=False)

        #ISHII
        #Flag to enable Ishii form of momentum equations.         two-phase flow.
        #.TRUE. ----------------------------- Solve Ishii form of momentum equations.
        # [ .FALSE. ]
        ishii = self.project.get_value('ishii', default=False)

        #JACKSON
        #LOGICAL
        #Flag to enable Jackson form of momentum equations.

        #.TRUE. ----------------------------- Solve Jackson form of momentum equations.
        #[ .FALSE. ] -------------------------- Default
        jackson = self.project.get_value('jackson', default=False)

        if model_b + ishii + jackson > 1:
            self.error("Invalid momentum equations: more than 1 of model_b, ishii and jackson specified")
        cb = ui.combobox_momentum_formulation
        idx = 0 # model A
        if model_b:
            idx = 1
        elif jackson:
            idx = 2
        elif ishii:
            idx = 3
        cb.setCurrentIndex(idx)
        # set combobox
        cb.setToolTip(get_combobox_item(cb, idx).toolTip())


        #Select sub-grid model:
        # Selection requirements:
        #  Only available with MFIX-TFM solver
        #  DRAG_TYPE="WEN_YU"
        #  KT_TYPE="ALGEBRAIC"
        #  TURBULENCE_MODEL /= K_EPSILON
        #  BLENDING_STRESS = NONE
        #  FRICTION_MODEL /= SRIVASTAVA
        #  (There are more restrictions…)
        key = 'subgrid_type'
        cb = ui.combobox_subgrid_type
        drag_type = self.project.get_value('drag_type', default=DEFAULT_DRAG_TYPE)
        kt_type = self.project.get_value('kt_type', default=DEFAULT_KT_TYPE)
        turbulence_model = self.project.get_value('turbulence_model', default=DEFAULT_TURBULENCE_MODEL)
        blending_stress = self.project.get_value('blending_stress') # Note, not set anywhere in GUI
        friction_model = self.project.get_value('friction_model', default=DEFAULT_FRICTION_MODEL)

        enabled = (solver == TFM
                   and drag_type == 'WEN_YU'
                   and kt_type == 'ALGEBRAIC'
                   and turbulence_model != 'K_EPSILON'
                   and bool(blending_stress) == False
                   and friction_model != 'SRIVASTAVA')
        cb = ui.combobox_subgrid_type
        for item in (ui.label_subgrid_type, cb,
                     ui.label_filter_size_ratio, ui.lineedit_keyword_filter_size_ratio,
                     ui.checkbox_keyword_subgrid_wall):
            item.setEnabled(enabled)
        if not enabled:
            self.unset_keyword(key)

        value = self.project.get_value(key, default=DEFAULT_SUBGRID_TYPE)
        if value not in SUBGRID_TYPES:
            self.error("Invalid subgrid_type %s" % value)
            value = DEFAULT_SUBGRID_TYPE
        idx = SUBGRID_TYPES.index(value)
        cb.setCurrentIndex(idx)
        cb.setToolTip(get_combobox_item(cb, idx).toolTip())


        #Specify sub-grid model filter size ratio:
        # Specification requires SUBGRID_TYPE =/ NONE
        # Sets keyword FILTER_SIZE_RATIO
        # DEFAULT 2.0 # handled by widget.default
        #Enable sub-grid wall correction model:
        # Specification requires SUBGRID_TYPE =/ NONE
        # Sets keyword SUBGRID_WALL
        # DEFAULT FALSE # handled by widget.default
        enabled = self.project.get_value('subgrid_type') not in (None, 'NONE')
        for item in (ui.label_filter_size_ratio, ui.lineedit_keyword_filter_size_ratio,
                     ui.checkbox_keyword_subgrid_wall):
            item.setEnabled(enabled)


    def set_subgrid_type(self, idx):
        # Sets keyword SUBGRID_TYPE
        # Available selections
        #  NONE (DEFAULT)
        #  IGCI
        #  MILIOLI
        ui = self.ui.model_setup
        cb = ui.combobox_subgrid_type
        self.update_keyword('subgrid_type', SUBGRID_TYPES[idx])
        cb.setToolTip(get_combobox_item(cb, idx).toolTip())

        self.setup_model_setup()


    def reset_model_setup(self):
        pass #?


    def disable_fluid_solver(self, disabled):
        # Option to disable the fluid phase
        # Disables the Fluid task pane menu
        # Sets keyword RO_G0 to 0.0
        m = self.ui.model_setup
        enabled = not disabled
        item = self.find_navigation_tree_item("Fluid")
        item.setDisabled(disabled)
        m.combobox_turbulence_model.setEnabled(enabled)
        self.update_nav_tree()
        # TODO update nscalar (?)

        if self.fluid_solver_disabled:
            self.ui.fluid.lineedit_keyword_ro_g0.minimum = 0
            self.update_keyword('ro_g0', 0)
            self.ui.fluid.lineedit_keyword_ro_g0.required = (self.fluid_density_model == CONSTANT)

        # This hack avoids clobbering ro_g0 during project load (review this!)
        if self.fluid_solver_disabled == disabled:
            return
        self.fluid_solver_disabled = disabled

        if disabled:
            self.unset_keyword('turbulence_model')
            self.saved_ro_g0 = self.project.get_value('ro_g0') # Probably too much work to save ro_g0
            self.update_keyword('ro_g0', 0) # issues/124
            if self.saved_ro_g0 is not None:
                self.ui.fluid.lineedit_keyword_ro_g0.setText(str(self.saved_ro_g0))
        else:
            val = self.saved_ro_g0 if self.fluid_density_model == CONSTANT else None
            self.update_keyword('ro_g0', val)
        if disabled: # Disable fluid momentum equations
            for c in 'xyz':
                key = 'momentum_%s_eq' % c
                self.update_keyword(key, False, args=[0])


    def enable_energy_eq(self, enabled):
        #    Option to enable thermal energy equations
        # This keyword should always be specified in the input deck
        # Sets keyword ENERGY_EQ
        # DEFAULT .FALSE.
        # Note, the mfix default is True.  We initialize to False in
        # the new project template (mfix.dat.template)

        # This is an additional callback on top of automatic keyword update,
        # since this has to change availability of several other GUI items
        ui = self.ui.model_setup
        ui.checkbox_keyword_energy_eq.setChecked(enabled)

        # It might not be necessary to do all this - will the fluid or
        # solid panes get updated before we display them?
        f = self.ui.fluid
        for item in (f.label_fluid_specific_heat_model,
                     f.combobox_fluid_specific_heat_model,
                     f.label_fluid_conductivity_model,
                     f.combobox_fluid_conductivity_model,
                     # more ?
                     ):
            item.setEnabled(enabled)

        # c_pg0 == specific heat for fluid phase
        lineedit = f.lineedit_keyword_c_pg0
        label = f.label_c_pg0_units
        for item in (lineedit, label):
            item.setEnabled(enabled and (self.fluid_specific_heat_model == CONSTANT))

        # k_g0 == thermal conductivity fluid phase
        lineedit = f.lineedit_keyword_k_g0
        label = f.label_k_g0_units
        for item in (lineedit, label):
            item.setEnabled(enabled and (self.fluid_conductivity_model == CONSTANT))


    def set_turbulence_model(self, val):
        # Available selections:
        #  None; [DEFAULT]
        #  Mixing Length:
        #    Selection always available
        #    Sets keyword TURBULENCE_MODEL to MIXING_LENGTH
        #    Requires IC_L_SCALE for all IC regions
        #  K-Epsilon
        #    Selection always available
        #    Sets keyword TURBULENCE_MODEL to K_EPSILON
        #    Requires IC_K_TURB_G for all IC regions TODO
        #    Requires IC_E_TURB_G for all IC regions TODO
        #    Requires BC_K_TURB_G for inflow (MI and PI) BC regions TODO
        #    Requires BC_E_TURB_G for inflow (MI and PI) BC regions TODO
        ui = self.ui.model_setup
        cb = ui.combobox_turbulence_model
        if cb.currentIndex() != val:
            cb.setCurrentIndex(val)
        turbulence_model = TURBULENCE_MODELS[val]
        # Avoid resetting turbulence_model from None to 'NONE' at startup
        if turbulence_model != self.project.get_value('turbulence_model',
                                                      default=DEFAULT_TURBULENCE_MODEL):
            self.update_keyword('turbulence_model', turbulence_model)
        # Set combobox tooltip to match item value
        cb.setToolTip(get_combobox_item(cb, val).toolTip())

        enabled = (turbulence_model != 'NONE')
        #Specify maximum fluid viscosity (not shown in mockup)
        # Selection available if TURBULENCE_MODEL =/ 'NONE'
        # Sets keyword MU_GMAX
        # DEFAULT 1.0e3 (Pa.s)
        for item in (ui.label_mu_gmax, ui.lineedit_keyword_mu_gmax, ui.label_mu_gmax_units):
            item.setEnabled(enabled)
        if enabled:
            val = ui.lineedit_keyword_mu_gmax.value
            if val=='' or val is None:
                val = default_values.mu_g0
            self.update_keyword('mu_gmax', val)
        else:
            self.unset_keyword('mu_gmax')

    def set_drag_type(self, idx):
        #Specify drag model
        # Selection requires TFM, DEM, or PIC solver
        # Sets keyword DRAG_TYPE
        if self.project.solver not in (TFM, DEM, PIC):
            # Should't be here
            return

        self.update_keyword('drag_type', DRAG_TYPES[idx])
        self.setup_model_setup()


    def set_momentum_formulation(self, idx):
        #Specify momentum equation formulation; Select Model A, Model B; Jackson, Ishii
        vals = [None, None, None]
        if idx > 0:
            vals[idx-1] = True
        for (key, val) in zip(('model_b', 'jackson', 'ishii'), vals):
            if val is None:
                self.unset_keyword(key)
            else:
                self.update_keyword(key, val)
        self.setup_model_setup()

    #Specify heat transfer correlation (requires TFM, DEM, or PIC solver)
    #*This option may be premature as MFIX is limited in heat HTCs.
    # NOT IMPLEMENTED
