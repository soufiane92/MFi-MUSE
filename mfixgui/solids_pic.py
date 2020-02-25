# Methods to deal with solids pic tab, slip off from solids_handler.py


from mfixgui.tools.qt import get_combobox_item, set_item_enabled

"""Particle in Cell Model Task Pane Window: (requires PIC solver)"""
# In-line comments from MFIX-UI_SRS as of 2016-07-01
#  Please update comments along with SRS/code changes!

des_interp_schemes = ['NONE', 'SQUARE_DPVM',  'GARG_2012']
NONE, SQUARE_DPVM, GARG_2012 = (0,1,2)

class SolidsPIC(object):

    ui = None
    project = None

    def init_solids_pic(self):
        ui = self.ui.solids
        ui.combobox_des_interp_2.activated.connect(self.set_des_interp_2)
        ui.combobox_des_interp_scheme_2.activated.connect(self.set_des_interp_scheme_2)
        ui.combobox_coupling_method_2.activated.connect(self.set_coupling_method_2)
        self.add_tooltip(ui.combobox_mppic_solid_stress_snider, 'mppic_solid_stress_snider')

    def set_des_interp_2(self, val):
        des_interp_on = not bool(val>>1)
        des_interp_mean_fields = not bool(val%2)
        self.update_keyword('des_interp_on', des_interp_on)
        self.update_keyword('des_interp_mean_fields', des_interp_mean_fields)
        self.setup_pic_tab()

    def set_des_interp_scheme_2(self, val):
        des_interp_scheme = des_interp_schemes[val]
        self.update_keyword('des_interp_scheme', des_interp_scheme)
        self.setup_pic_tab()

    def set_coupling_method_2(self, val):
        self.update_keyword('des_oneway_coupled', [True,False][val])
        self.setup_pic_tab()

    def setup_pic_tab(self):
        # Note - we are doing this setup on first show of this tab, rather
        # than at init or project load time
        ui = self.ui.solids

        #Specify void fraction at close pack (required)
        # Sets keyword EP_STAR [0.42]
        #Define parcel CFL number
        # Sets keyword CFL_PIC
        # DEFAULT 0.1
        for (key, default) in [('ep_star', 0.42),
                               ('cfl_pic', 0.1)]:
            val = self.project.get_value(key)
            if val is None:
                self.update_keyword(key, default)

        #Select solid stress model
        # Selection always available
        # Available selection
        # Snider, 2001 [DEFAULT]
        # Selection always available
        # Sets keyword MPPIC_SOLID_STRESS_SNIDER=.T.
        self.update_keyword('mppic_solid_stress_snider', True)
        # TODO FIXME does this ever get cleared?

        #Option to enable implicit treatment of drag force
        # Sets keyword: MPPIC_PDRAG_IMPLICIT
        # Disabled [DEFAULT]
        pass

        #Define particle-particle momentum retention
        # Sets keyword MPPIC_COEFF_EN1
        # DEFAULT 0.4
         #Define wall normal momentum retention
        # Sets keyword MPPIC_COEFF_EN_WALL
        # DEFAULT 0.3
         #Define wall tangential momentum retention
        # Sets keyword MPPIC_COEFF_ET_WALL
        # DEFAULT 0.99
        for (key, default) in [('mppic_coeff_en1', 0.4),
                               ('mppic_coeff_en_wall', 0.3),
                               ('mppic_coeff_et_wall', 0.99)]:
            val = self.project.get_value(key)
            if val is None:
                self.update_keyword(key, default)

        #Select gas-solids coupling scheme:
        # Selection unavailable if fluid model is disabled
        # Available selections:
        # One-way Coupled
        # Selection always available
        # Sets keyword DES_ONEWAY_COUPLED true
        # Fully Coupled
        # Selection always available
        # Sets keyword DES_ONEWAY_COUPLED false
        enabled = not self.fluid_solver_disabled
        for item in (ui.label_coupling_method_2, ui.combobox_coupling_method_2):
            item.setEnabled(enabled)
        des_oneway_coupled = self.project.get_value('des_oneway_coupled', default=False)
        if des_oneway_coupled not in (True, False):
            self.warn("Invalid des_oneway_coupled %s" % des_oneway_coupled)
            des_oneway_coupled = False
            self.update_keyword('des_oneway_coupled', des_oneway_coupled)
        ui.combobox_coupling_method_2.setCurrentIndex(0 if des_oneway_coupled else 1)

        #Select interpolation framework:
        # Selection always available
        # Available selections:
        # field-to-particle and particle-to-field [DEFAULT]
        # Sets keyword DES_INTERP_ON to true
        # Sets keyword DES_INTERP_MEAN_FIELDS to true
        # field-to-particle only
        # Sets keyword DES_INTERP_ON to true
        # Sets keyword DES_INTERP_MEAN_FIELDS to false
        # particle-to-field only
        # Sets keyword DES_INTERP_ON to false
        # Sets keyword DES_INTERP_MEAN_FIELDS to true
        # no-interpolation
        # Sets keyword DES_INTERP_ON to false
        # Sets keyword DES_INTERP_MEAN_FIELDS to false
        des_interp_on = self.project.get_value('des_interp_on', default=True)
        if des_interp_on not in (True, False):
            self.warn("Invalid des_interp_on %s" % des_interp_on)
            des_interp_on = True
            self.update_keyword('des_interp_on', des_interp_on)

        des_interp_mean_fields = self.project.get_value('des_interp_mean_fields', default=True)
        if des_interp_mean_fields not in (True, False):
            self.warn("Invalid des_interp_mean_fields %s" % des_interp_mean_fields)
            des_interp_mean_fields = True
            self.update_keyword('des_interp_mean_fields', des_interp_mean_fields)

        index = 2*(1-des_interp_on) + (1-des_interp_mean_fields)
        ui.combobox_des_interp_2.setCurrentIndex(index)


        #Select interpolation scheme:
        # Selection available except when no-interpolation framework is selected
        # Available selections:
        # None [locked default for no-interpolation framework]
        # Selection not available
        # Sets keyword DES_INTERP_SCHEME='NONE' # Todo, update SRS
        # Garg 2012
        # Selection not available with explicit coupling enabled
        # Sets keyword DES_INTERP_SCHEME='GARG_2012'
        # Square DPVM
        # Selection always available
        # Requires an interpolation width, DES_INTERP_WIDTH
        # Sets keyword DES_INTERP_SCHEME='SQUARE_DPVM'
        #
        cb = ui.combobox_des_interp_scheme_2
        label = ui.label_des_interp_scheme_2
        des_interp_scheme = self.project.get_value('des_interp_scheme')
        des_explicitly_coupled = self.project.get_value('des_explicitly_coupled')
        interp_enabled = des_interp_on or des_interp_mean_fields # not no-interp
        for item in (cb, label):
            item.setEnabled(interp_enabled)
        if not interp_enabled:
            cb.setCurrentIndex(NONE)
            des_interp_scheme = 'NONE'
        else:
            if des_interp_scheme not in des_interp_schemes:
                self.warning("invalid des_interp_scheme %s" % des_interp_scheme)
                des_interp_scheme = 'SQUARE_DPVM'
            idx = des_interp_schemes.index(des_interp_scheme)
            cb.setCurrentIndex(idx)

        # per-item enable flags
        enabled = (not interp_enabled, not des_explicitly_coupled, True)
        for (i,e) in enumerate(enabled):
            set_item_enabled(get_combobox_item(cb,i), e)
        # Make sure we don't leave the combobox on an invalid item!
        if not enabled[cb.currentIndex()]:
            idx = enabled.index(True) # 2 is always True so this is safe
            cb.setCurrentIndex(idx)
            des_interp_scheme = des_interp_schemes[idx]
        # Value of des_interp_scheme may have changed
        self.update_keyword('des_interp_scheme', des_interp_scheme)

        #Define interpolation width (DPVM only) (required)
        # Specification only available with SQUARE_DPVM interpolation scheme
        # Sets keyword DES_INTERP_WIDTH
        # TODO default?
        key = 'des_interp_width'
        enabled = interp_enabled and (des_interp_scheme=='SQUARE_DPVM') #?
        for item in (ui.label_des_interp_width_2, ui.lineedit_keyword_des_interp_width_2,
                     ui.label_des_interp_width_2_units):
            item.setEnabled(enabled)
        if des_interp_scheme != 'SQUARE_DPVM':
            self.unset_keyword(key)
        else:
            self.update_keyword('des_interp_width', ui.lineedit_keyword_des_interp_width_2.value)


        #Define solids stress model parameter: pressure constant
        # Sets keyword PSFAC_FRIC_PIC
        # DEFAULT 10.0
        #Define solids stress model parameter: volume fraction exponent
        # Sets keyword FRIC_EXP_PIC
        # DEFAULT 3.0
        #Define solids stress model parameter: non-singularity factor
        # Sets keyword FRIC_NON_SING_FAC
        # DEFAULT 1.0E-8
        for (key, default) in [('psfac_fric_pic', 10.0),
                               ('fric_exp_pic', 3.0),
                               ('fric_non_sing_fac', 1.0e-8)]:
            val = self.project.get_value(key)
            if val is None:
                self.update_keyword(key, default)

        # Fin!
