# Methods to deal with solids tfm tab, slip off from solids_handler.py

from mfixgui.tools.qt import get_combobox_item, set_item_enabled

from mfixgui.constants import *

class SolidsTFM(object):

    ui = None
    project = None

    def init_solids_tfm(self):
        ui = self.ui.solids
        key = 'kt_type'
        cb = ui.combobox_kt_type
        cb.activated.connect(self.set_kt_type)
        self.add_tooltip(cb, key)
        for (i, v) in enumerate(KT_TYPES):
            self.add_tooltip(get_combobox_item(cb, i), key, value=v)

        key = 'friction_model'
        cb = ui.combobox_friction_model
        self.add_tooltip(cb, key)
        for (i, v) in enumerate(FRICTION_MODELS):
            self.add_tooltip(get_combobox_item(cb, i), key, value=v)
        cb.activated.connect(self.set_friction_model)

        key = 'rdf_type'
        cb =  ui.combobox_rdf_type
        self.add_tooltip(cb, key)
        for (i, v) in enumerate(RDF_TYPES):
            self.add_tooltip(get_combobox_item(cb, i), key, value=v or 'CARNAHAN_STARLING')
        cb.activated.connect(self.set_rdf_type)

        key = 'blending_function'
        cb = ui.combobox_blending_function
        self.add_tooltip(cb, key)
        for (i, v) in enumerate(BLENDING_FUNCTIONS):
            self.add_tooltip(get_combobox_item(cb, i), key, value=v)
        cb.activated.connect(self.set_blending_function)

        # This does not correspond to a single keyword
        label = ui.label_max_packing_correlation
        cb = ui.combobox_max_packing_correlation
        description='Correlation to compute maximum packing for polydisperse systems'
        self.add_tooltip(label, key=None, description=description)
        self.add_tooltip(cb, key=None, description=description)
        self.add_tooltip(get_combobox_item(cb, 0), key=None, description="Constant packing correlation")
        self.add_tooltip(get_combobox_item(cb, 1), key='yu_standish')
        self.add_tooltip(get_combobox_item(cb, 2), key='fedors_landel')
        cb.activated.connect(self.set_max_packing_correlation)

    def setup_tfm_tab(self):
        # Note - we are doing this setup on first show of this tab, rather
        # than at init or project load time
        ui = self.ui.solids

        # Select Viscous Stress Model (KTGS):
        # Selection is unavailable for constant solids viscosity (MU_S0 defined)
        # FIXME This is not right, solids viscosity model is phase-dependent
        #enabled = (self.solids_viscosity_model != CONSTANT) # SRS p18
        #for item in (ui.label_kt_type, ui.combobox_kt_type,
        #             ui.label_friction_model, ui.combobox_friction_model):
        #    item.setEnabled(enabled)

        # SRS p18 - enable/disable menu items in viscous stress model
        key = 'kt_type'
        kt_type = self.project.get_value(key, default=DEFAULT_KT_TYPE)
        cb = ui.combobox_kt_type
        if kt_type:
            if kt_type not in KT_TYPES:
                self.warn("Invalid kt_type %s" % kt_type)
                self.unset_keyword(key)
                self.add_tooltip(cb, key=key)
            else:
                cb.setCurrentIndex(KT_TYPES.index(kt_type))
                self.add_tooltip(cb, key=key, value=kt_type)
        else:
            self.add_tooltip(cb, key=key)

        mmax = self.project.get_value('mmax', default=1)
        k_e = (self.project.get_value('turbulence_model', default=DEFAULT_TURBULENCE_MODEL) == 'K_EPSILON')
        added_mass = self.project.get_value('m_am') or self.project.get_value('added_mass')
        drag_type = self.project.get_value('drag_type')
        friction_model = self.project.get_value('friction_model', default=DEFAULT_FRICTION_MODEL)
        enabled = [True, #ALGEBRAIC
                   True, #LUN_1984
                   True, #IA_NONEP
                   k_e,  #SIMONIN
                   k_e,  #AHMADI
                   mmax==1, #GD99
                   mmax==1, #GTSH
                   mmax<=2 and (not added_mass) and drag_type in ('WEN_YU', 'HYS')] # GHD
        #assert len(enabled) == len(KT_TYPES)
        for (i,e) in enumerate(enabled):
            set_item_enabled(get_combobox_item(cb,i), e)

        # Select Frictional Stress Model
        cb = ui.combobox_friction_model
        key = 'friction_model'
        # Srivastava and Sundaresan, 2003
        # Unavailable for Algebraic Formulation viscous stress model
        set_item_enabled(get_combobox_item(cb,1), (kt_type!='ALGEBRAIC'))
        if kt_type == 'ALGEBRAIC':
            if friction_model == 'SRIVASTAVA': # Forbidden
                cb.setCurrentIndex(2) # None
                friction_model = FRICTION_MODELS[2]
                self.update_keyword(key, friction_model)

        if friction_model not in FRICTION_MODELS:
            self.warn("Unrecogized friction_model %s" % friction_model)
            cb.setCurrentIndex(2) # None
            friction_model = FRICTION_MODELS[2]
            self.update_keyword(key, friction_model)
        else:
            cb.setCurrentIndex(FRICTION_MODELS.index(friction_model))
        self.add_tooltip(cb, key=key, value=friction_model)

        # Specify solids volume fraction at onset of friction
        enabled = (friction_model == 'SRIVASTAVA')
        for item in (ui.label_eps_f_min, ui.lineedit_keyword_eps_f_min):
            item.setEnabled(enabled)

        #Specify particle-particle restitution coefficient
        # Specification available only when required
        # Required for MMAX >=2
        # Required for viscous stress models except GHD and algebraic formulation
        # Sets keyword C_E
        enabled = (mmax>=2) or (kt_type not in ('GHD', 'ALGEBRAIC'))
        for item in (ui.label_c_e, ui.lineedit_keyword_c_e):
            item.setEnabled(enabled)

        #  Garzo, Hrenya and Dufty, 2007
        #    Selection not available for MMAX > 2
        #    Selection not available with added mass force
        #    Sets keyword KT_TYPE to GHD
        #    Requires WEN_YU or HYS drag model
        #    Specify coefficient of restitution; R_p (optional)
        # note R_P *replaces* C_E for kt_type==GHD

        ghd = (kt_type=='GHD')
        if ghd:
            names = list(self.solids.keys())
            if names:
                ui.label_r_p_1_1.setText(
                    "%s restitution coeff." % names[0])
            if len(names) > 1:
                ui.label_r_p_1_2.setText(
                    "%s-%s restitution coeff." % (names[0], names[1]))
                ui.label_r_p_2_2.setText(
                    "%s restitution coeff." % names[1])

        for item in (ui.label_c_e,
                     ui.lineedit_keyword_c_e):
            item.hide() if ghd else item.show()

        for item in (ui.label_r_p_1_1,
                     ui.lineedit_keyword_r_p_args_1_1):
            item.show() if ghd else item.hide()

        for item in (ui.label_r_p_1_2,
                     ui.label_r_p_2_2,
                     ui.lineedit_keyword_r_p_args_1_2,
                     ui.lineedit_keyword_r_p_args_2_2):
            item.show() if (ghd and len(names)>1) else item.hide()

        #Specify interphase friction coefficient
        # Specification available only when required
        # Required for MMAX >= 2
        # Sets keyword C_F
        enabled = (mmax>=2)
        for item in (ui.label_c_f, ui.lineedit_keyword_c_f):
            item.setEnabled(enabled)

        #Specify angle of particle-particle friction
        # Specification available only when required
        # Required for FRICTION_MODEL=SCHAEFFER
        # Required for FRICTION_MODEL=SRIVASTAVA
        # Sets keyword PHI
        enabled = friction_model in ('SCHAEFFER', 'SRIVASTAVA')
        for item in (ui.label_phi, ui.lineedit_keyword_phi):
            item.setEnabled(enabled)

        ### Advanced
        # Select radial distribution function
        key = 'rdf_type'
        rdf_type = self.project.get_value(key)
        if mmax > 1:
            if rdf_type not in RDF_TYPES:
                self.warn('Invalid rdf_type %s' % rdf_type)
                rdf_type = 'LEBOWITZ'
                self.update_keyword('rdf_type', rdf_type)
        if mmax < 2:
            if rdf_type:
                self.warn('Invalid rdf_type %s' % rdf_type)
                rdf_type = None
                self.update_keyword(key, rdf_type)

        cb = ui.combobox_rdf_type
        index = RDF_TYPES.index(rdf_type)
        if index == 0 and mmax > 1:
            index = 1 # Lebowitz
            rdf_type = 'LEBOWITZ' # So that tooltip will be set correctly
        cb.setCurrentIndex(index)
        self.add_tooltip(cb, key=key, value=rdf_type or 'CARNAHAN_STARLING')

        enabled = [mmax==1] + 4*[mmax>1]
        for (i,e) in enumerate(enabled):
            set_item_enabled(get_combobox_item(cb,i), e)

        # Select stress blending model
        # Selection only available with FRICTION_MODEL=SCHAEFFER
        key = 'blending_function'
        cb = ui.combobox_blending_function
        blending_function = self.project.get_value('blending_function',
                                                   default=DEFAULT_BLENDING_FUNCTION)
        if blending_function not in BLENDING_FUNCTIONS:
            self.warn('Invalid blending_function %s' % blending_function)
            blending_function = DEFAULT_BLENDING_FUNCTION
            self.update_keyword(key, blending_function)
        cb.setCurrentIndex(BLENDING_FUNCTIONS.index(blending_function))
        self.add_tooltip(cb, key, value=blending_function)
        enabled = (friction_model=='SCHAEFFER')
        for item in (ui.label_blending_function, cb):
                    item.setEnabled(enabled)
        if not enabled:
            self.unset_keyword(key)
        else: # Restore value (should we do this?)
            v = cb.currentIndex()
            if self.project.get_value(key, default=DEFAULT_BLENDING_FUNCTION) != BLENDING_FUNCTIONS[v]:
                self.update_keyword(key, BLENDING_FUNCTIONS[v])


        # Specify the segregation slope coefficient
        #  Only available for MMAX > 1 in conjunction with the following viscous stress
        # algebraic formulation; Lun. 1984; Simonin, 1996; Ahmadi, 1995
        enabled = (mmax>1) and kt_type in ['ALGEBRAIC', 'LUN_1984', 'SIMONIN', 'AHMADI']
        for item in (ui.label_segregation_slope_coefficient,
                     ui.lineedit_keyword_segregation_slope_coefficient):
                    item.setEnabled(enabled)

        # Select maximum packing correlation
        # Selection only available with FRICTION_MODEL=SCHAEFFER and MMAX >1
        enabled = (friction_model=='SCHAEFFER') and (mmax>1)
        for item in (ui.label_max_packing_correlation, ui.combobox_max_packing_correlation):
            item.setEnabled(enabled)
        # Constant [DEFAULT]
        # Selection always available
        # Yu & Standish
        # Selection always available
        cb = ui.combobox_max_packing_correlation
        yu_standish = self.project.get_value('yu_standish')
        fedors_landel = self.project.get_value('fedors_landel')
        set_item_enabled(get_combobox_item(cb, 2), mmax==2) # Only enable F_L for binary mixture

        if yu_standish and fedors_landel:
            self.warn("YU_STANDISH and FEDORS_LANDEL both set")
            self.unset_keyword('yu_standish')
            self.unset_keyword('fedors_landel')
            yu_standish = fedors_landel = False
        if fedors_landel and (mmax != 2):
            self.warn("FEDORS_LANDEL only valid for binary mixtures")
            self.unset_keyword('fedors_landel')
            fedors_landel = False
        if yu_standish:
            cb.setCurrentIndex(1)
            self.add_tooltip(cb, key='YU_STANDISH')
        elif fedors_landel:
            cb.setCurrentIndex(2)
            self.add_tooltip(cb, key='FEDORS_LANDEL')
        else:
            cb.setCurrentIndex(0)
            self.add_tooltip(cb, key=None, description="Constant packing correlation")

        # Specify excluded volume in Boyle-Massoudi stress (optional)
        # Only available with algebraic formulation of viscous stress model
        enabled = (kt_type=='ALGEBRAIC')
        for item in (ui.label_v_ex, ui.lineedit_keyword_v_ex):
            item.setEnabled(enabled)

    def set_kt_type(self, i):
        key = 'kt_type'
        val = KT_TYPES[i]
        self.update_keyword(key, val)
        self.setup_tfm_tab()

    def set_friction_model(self, i):
        key = 'friction_model'
        val =  FRICTION_MODELS[i]
        self.update_keyword(key, val)
        self.setup_tfm_tab()

    def set_rdf_type(self, i):
        key = 'rdf_type'
        val = RDF_TYPES[i]
        self.update_keyword(key, val)
        self.setup_tfm_tab()

    def set_blending_function(self, i):
        key = 'blending_function'
        val =  BLENDING_FUNCTIONS[i]
        self.update_keyword(key, val)
        self.setup_tfm_tab()

    def set_max_packing_correlation(self, i):
        # Set one key, unset the other
        self.update_keyword('yu_standish', i==1 or None)
        self.update_keyword('fedors_landel', i==2 or None)
        self.setup_tfm_tab()
