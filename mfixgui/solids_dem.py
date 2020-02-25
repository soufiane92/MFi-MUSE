# Methods to deal with solids DEM tab, slip off from solids_handler.py
"""Discrete Element Model Task Pane Window: (requires DEM solver)"""

from qtpy.QtWidgets import QLabel, QTableWidgetItem
from qtpy.QtCore import Qt

from mfixgui.widgets.base import LineEdit
from mfixgui.project import Equation
from mfixgui.tools.qt import get_combobox_item, set_item_enabled, set_item_noedit
from mfixgui import default_values

# In-line comments from MFIX-UI_SRS as of 2016-07-01
#  Please update comments along with SRS/code changes!


des_intg_methods = ['EULER', 'ADAMS_BASHFORTH']
des_coll_models = ['LSD', 'HERTZIAN']
des_interp_schemes = ['NONE', 'SQUARE_DPVM', 'GARG_2012']
NONE, SQUARE_DPVM, GARG_2012 = (0,1,2)

class SolidsDEM(object):
    # After any item in this tab is edited, we call setup_dem_tab again to make
    # sure all constraints (disabled/enabled inputs) are enforced
    # Therefore, 'setup_dem_tab' must not call any of the setters

    ui = None
    project = None

    def init_solids_dem(self):
        ui = self.ui.solids
        ui.checkbox_keyword_gener_part_config.clicked.connect(self.set_gener_part_config)
        ui.combobox_des_intg_method.activated.connect(self.set_des_intg_method)
        ui.combobox_des_coll_model.activated.connect(self.set_des_coll_model)
        ui.combobox_coupling_method.activated.connect(self.set_coupling_method)
        ui.checkbox_keyword_des_explicitly_coupled.post_update = self.setup_dem_tab
        ui.combobox_des_interp.activated.connect(self.set_des_interp)
        ui.combobox_des_interp_scheme.activated.connect(self.set_des_interp_scheme)
        ui.checkbox_enable_des_diffuse_width.clicked.connect(self.enable_des_diffuse_width)
        ui.combobox_cohesion_model.activated.connect(self.set_cohesion_model)
        ui.checkbox_enable_des_usr_var_size.clicked.connect(self.enable_des_usr_var_size)
        self.add_tooltip(ui.checkbox_enable_des_usr_var_size, key='des_usr_var_size')
        ui.combobox_des_neighbor_search.activated.connect(self.set_des_neighbor_search)
        ui.lineedit_keyword_des_diffuse_width.setdtype('dp')
        # issues/593
        for item in (ui.lineedit_keyword_ep_star_3,
                     ui.label_ep_star_3):
            self.add_tooltip(item, key='ep_star', description='For DEM simulations, this specifies minimum (clipping) value for the fluid phase volume fraction.')

        self.solids_dem_saved_solids_names = [] # keep track of when we need to rebuild per-phase gui items


    def set_gener_part_config(self, val):
        ui = self.ui.solids
        self.update_keyword('gener_part_config', val)
        if val:
            self.unset_keyword("particles")
        else:
            self.update_keyword("particles", ui.lineedit_keyword_particles.value or 0)
        enabled = not val
        for item in (ui.label_particles, ui.lineedit_keyword_particles):
            item.setEnabled(enabled)


    def set_des_intg_method(self, val):
        self.update_keyword('des_intg_method', des_intg_methods[val])
        self.setup_dem_tab()

    def set_des_coll_model(self, val):
        self.update_keyword('des_coll_model', des_coll_models[val])
        self.setup_dem_tab()

    def set_coupling_method(self, val):
        self.update_keyword('des_oneway_coupled', [True,False][val])
        self.setup_dem_tab()

    def set_des_interp(self, val):
        des_interp_on = not bool(val>>1)
        des_interp_mean_fields = not bool(val%2)
        self.update_keyword('des_interp_on', des_interp_on)
        self.update_keyword('des_interp_mean_fields', des_interp_mean_fields)
        self.setup_dem_tab()

    def set_des_interp_scheme(self, val):
        des_interp_scheme = des_interp_schemes[val]
        self.update_keyword('des_interp_scheme', des_interp_scheme)
        self.setup_dem_tab()


    def enable_des_diffuse_width(self, val):
        ui = self.ui.solids
        enabled = val
        for item in (ui.label_des_diffuse_width, ui.lineedit_keyword_des_diffuse_width,
                     ui.label_des_diffuse_width_units):
            item.setEnabled(enabled)
        if not enabled:
            self.unset_keyword('des_diffuse_width')
        else: #Restore value
            self.update_keyword('des_diffuse_width',
                                ui.lineedit_keyword_des_diffuse_width.value)


    def set_cohesion_model(self, val):
        for kw in ('use_cohesion', 'van_der_waals'):
            self.update_keyword(kw, bool(val))
        self.setup_dem_tab()

    def enable_des_usr_var_size(self, val):
        ui = self.ui.solids
        if not val:
            self.unset_keyword('des_usr_var_size')
        else:
            self.update_keyword('des_usr_var_size',
                                ui.lineedit_keyword_des_usr_var_size.value)
        self.setup_dem_tab()

    def set_des_neighbor_search(self, val):
        self.update_keyword('des_neighbor_search', 4 if val==0 else 1)
        # No setup_dem_tab needed

    def setup_dem_tab(self):
        # Ensures all constraints (items enabled/disabled) are set
        # called by each 'set_' function, so don't call those here

        ui = self.ui.solids
        # Inline comments from MFIX-UI_SRS as of 2016-07-01
        #  Please update as needed!

        #MFIX-UI_SRS
        #Enable automatic particle generation
        # Enabled sets keyword GENER_PART_CONFIG to true
        # Disabled enables the user to specify number of entries in particle input file
        # Default value is 0
        default = 0
        # Sets keyword PARTICLES
        gener_part_config = self.project.get_value('gener_part_config')
        particles = self.project.get_value('particles')

        if gener_part_config:
            if particles: # Should not both be set
                self.warning("gener_part_config set, particles=%s" % particles)
                particles = default
        else:
            if particles is None: # set to 0 if not set
                particles = default
            elif particles < 0:
                self.warning("Invalid particles %s" % particles)
                particles = default
        self.update_keyword('particles', particles)


        enabled = not gener_part_config
        for item in (ui.label_particles, ui.lineedit_keyword_particles):
            item.setEnabled(enabled)

        #Select numerical integration method
        # Selection always available
        # Available selections
        #Euler [DEFAULT]
        # Selection always available
        # Sets keyword DES_INTG_METHOD to 'EULER'
        #Adams-Bashforth
        # Selection always available
        # Sets keyword DES_INTG_METHOD to 'ADAMS_BASHFORTH'1
        des_intg_method = self.project.get_value('des_intg_method', default='EULER')
        if des_intg_method not in des_intg_methods:
            self.warn("Invalid des_intg_method %s" % des_intg_method)
            des_intg_method = 'EULER'
        ui.combobox_des_intg_method.setCurrentIndex(des_intg_methods.index(des_intg_method))

        #Selection collision model
        # Selection always available
        # Available selections
        #Linear Spring-Dashpot [DEFAULT]
        # Selection always available
        # Sets keyword DES_COLL_MODEL to 'LSD'
        #Hertzian
        # Selection always available
        # Sets keyword DES_COLL_MODEL to 'HERTZIAN'
        des_coll_model = self.project.get_value('des_coll_model', default='LSD')
        if des_coll_model not in des_coll_models:
            self.warn("Invalid des_coll_model %s" % des_coll_model)
            des_coll_model = 'LSD'
        ui.combobox_des_coll_model.setCurrentIndex(des_coll_models.index(des_coll_model))

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
        for item in (ui.label_coupling_method, ui.combobox_coupling_method):
            item.setEnabled(enabled)
        des_oneway_coupled = self.project.get_value('des_oneway_coupled', default=False)
        if des_oneway_coupled not in (True, False):
            self.warn("Invalid des_oneway_coupled %s" % des_oneway_coupled)
            des_oneway_coupled = False
            self.update_keyword('des_oneway_coupled', des_oneway_coupled)
        ui.combobox_coupling_method.setCurrentIndex(0 if des_oneway_coupled else 1)

        #Optional to enable explicitly coupled simulation
        # Unavailable for GARG_2012 interpolation
        des_interp_scheme = self.project.get_value('des_interp_scheme')
        enabled = (des_interp_scheme!='GARG_2012')
        ui.checkbox_keyword_des_explicitly_coupled.setEnabled(enabled)

        #Select interpolation framework:
        # Selection always available
        # Available selections:
        # Field-to-Particle and Particle-to-Field [DEFAULT]
        #  Sets keyword DES_INTERP_ON to true
        #  Sets keyword DES_INTERP_MEAN_FIELDS to true
        # Field-to-Particle only
        #  Sets keyword DES_INTERP_ON to true
        #  Sets keyword DES_INTERP_MEAN_FIELDS to false
        # Particle-to-Field only
        #  Sets keyword DES_INTERP_ON to false
        #  Sets keyword DES_INTERP_MEAN_FIELDS to true
        # No Interpolation
        #  Sets keyword DES_INTERP_ON to false
        #  Sets keyword DES_INTERP_MEAN_FIELDS to false
        #
        # issues/116 must also set DES_INTERP_SCHEME to None when no-interpolation
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
        ui.combobox_des_interp.setCurrentIndex(index)

        #Select interpolation scheme:
        # Selection available except when no-interpolation framework is selected
        # Available selections:
        #  None [locked default for no-interpolation framework]
        #  Selection always available
        #  Sets keyword DES_INTERP_SCHEME='NONE'
        # Garg 2012
        #  Selection not available with explicit coupling enabled
        #  Sets keyword DES_INTERP_SCHEME='GARG_2012'
        # Square DPVM
        #  Selection always available
        #  Requires an interpolation width, DES_INTERP_WIDTH
        #  Sets keyword DES_INTERP_SCHEME='SQUARE_DPVM'
        #
        cb = ui.combobox_des_interp_scheme
        label = ui.label_des_interp_scheme
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
                des_interp_scheme = 'NONE'
            cb.setCurrentIndex(des_interp_schemes.index(des_interp_scheme))

        #
        # per-item enable flags
        enabled = (True, True, not des_explicitly_coupled)
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
        for item in (ui.label_des_interp_width, ui.lineedit_keyword_des_interp_width,
                     ui.label_des_interp_width_units):
            item.setEnabled(enabled)
        if des_interp_scheme != 'SQUARE_DPVM':
            self.unset_keyword(key)
        else:
            self.update_keyword(key, ui.lineedit_keyword_des_interp_width.value)


        #Option to enable diffusion of particle data
        # Selection unavailable with GARG_2012 interpolation scheme
        # No keyword is set by this option
        # Enables the user to specify a diffusion width
        # Sets keyword DES_DIFFUSE_WIDTH
        key = 'des_diffuse_width'
        enabled = (des_interp_scheme!='GARG_2012')
        ui.checkbox_enable_des_diffuse_width.setEnabled(enabled)
        if not enabled:
            ui.checkbox_enable_des_diffuse_width.setChecked(False)
            self.unset_keyword(key)
            ui.lineedit_keyword_des_diffuse_width.clear() # ??? FIXME
        enabled = ui.checkbox_enable_des_diffuse_width.isChecked()
        for item in (ui.label_des_diffuse_width, ui.lineedit_keyword_des_diffuse_width,
                     ui.label_des_diffuse_width_units):
            item.setEnabled(enabled)
            if enabled:
                self.update_keyword(key, ui.lineedit_keyword_des_diffuse_width.value)


        #Specify friction coefficient
        # Specification always required
        # Sets keyword MEW (MEW_W)
        pass

        #Specify normal spring constant
        # Only available for LSD collision model
        # Sets keyword KN (KN_W)
        #Specify tangential spring constant factor
        # Only available for LSD collision model
        # Sets keyword KT_FAC (KT_W_FAC)
        # Default values of 2.0/7.0
        #Specify tangential damping coefficient factor
        # Only available for LSD collision model
        # Sets keyword DES_ETAT_FAC (DES_ETAT_W_FAC)
        # Default values of 0.5
        enabled = (des_coll_model=='LSD')
        for item in (ui.label_kn,
                     ui.lineedit_keyword_kn,
                     ui.lineedit_keyword_kn_w,
                     ui.label_kn_units,
                     ui.label_kt_fac,
                     ui.lineedit_keyword_kt_fac,
                     ui.lineedit_keyword_kt_w_fac,
                     ui.label_des_etat_fac,
                     ui.lineedit_keyword_des_etat_fac,
                     ui.lineedit_keyword_des_etat_w_fac):
            item.setEnabled(enabled)


        if enabled: # TODO set these defaults at load-time, not when this tab is shown
            for (key, default) in [('kt_fac', Equation('2/7')), ('kt_w_fac', Equation('2/7')),
                                   ('des_etat_fac', 0.5), ('des_etat_w_fac', 0.5)]:
                if self.project.get_value(key) is None:
                    self.update_keyword(key, default)

        # Unset keywords if not enabled?
        #Specify Young's modulus
        # Only available for Hertzian collision model
        # Sets keyword E_YOUNG (EW_YOUNG)

        layout = ui.gridlayout_dem_parameters
        enabled = (des_coll_model=='HERTZIAN')
        for item in (ui.label_e_young,
                     ui.lineedit_keyword_ew_young,
                     ui.label_e_young_units,
                     ui.label_v_poisson,
                     ui.lineedit_keyword_vw_poisson):
            item.setEnabled(enabled)

        key = 'e_young'
        solids_names = list(self.solids.keys())
        if self.solids_dem_saved_solids_names != solids_names:

            # We put these back after inserting rows
            for item in (ui.label_v_poisson,
                         ui.lineedit_keyword_vw_poisson):
                item.hide()
                layout.removeWidget(item)

            # Delete all the old ones...
            for idx in range(layout.count()-1, -1, -1):
                item = layout.itemAt(idx)
                w = item.widget()
                if not w:
                    continue
                name = w.objectName()
                if '_args_' in  name:
                       w.hide()
                       if isinstance(w, LineEdit):
                           self.project.unregister_widget(w)
                       layout.removeWidget(w)
                       w.setParent(None)
                       w.deleteLater()

            # ...and make new ones
            idx = layout.indexOf(ui.lineedit_keyword_ew_young)
            columns = 4
            row = 1 + int(idx/columns)
            for (p, name) in enumerate(self.solids.keys(), 1):
                row += 1
                label = QLabel('    ' + name)
                label.setObjectName('label_%s_args_%s' % (key,p))
                # FIXME use dynamic_widgets instead of setattr
                setattr(ui, label.objectName(), label)
                label.args = [p]
                self.add_tooltip(label, key)
                layout.addWidget(label, row, 0, 1, 1)
                label.setEnabled(enabled)

                le = LineEdit()
                le.setMaximumWidth(150) # matches ui file
                le.key = key
                le.args = [p]
                le.dtype = float
                le.setValInfo(min=0.0)
                le.setObjectName('lineedit_keyword_%s_args_%s' % (key, p))
                setattr(ui, le.objectName(), le)
                self.add_tooltip(le, key)
                layout.addWidget(le, row, 1, 1, 1)
                le.setEnabled(enabled)
                val = self.project.get_value(key, args=[p])
                if val is not None:
                    le.updateValue(key, val)
                self.project.register_widget(le, keys=[key], args=[p])

                label = QLabel('Pa')
                label.setObjectName('label_%s_units_args_%s' % (key, p))
                layout.addWidget(label, row, 3, 1, 1)
                label.setEnabled(enabled)

            #Specify Poisson ratio:
            # Only available for Hertzian collision model
            # Sets keyword V_POISSON (VW_POISSON)
            row += 1
            layout.addWidget(ui.label_v_poisson, row, 0, 1, 1)
            layout.addWidget(ui.lineedit_keyword_vw_poisson, row, 2, 1, 1)
            for item in (ui.label_v_poisson, ui.lineedit_keyword_vw_poisson):
                item.show()
                item.setEnabled(enabled)

            row += 1
            key = 'v_poisson'

            for (p, name) in enumerate(self.solids.keys(), 1):
                row += 1
                label = QLabel('    ' + name)
                label.setObjectName('label_%s_args_%s' % (key, p))
                setattr(ui, label.objectName(), label)
                label.args = [p]
                self.add_tooltip(label, key)
                layout.addWidget(label, row, 0, 1, 1)
                label.setEnabled(enabled)
                le = LineEdit()
                le.setMaximumWidth(150) #?
                le.key = key
                le.args = [p]
                le.dtype = float
                le.setValInfo(min=0.0)
                le.setObjectName('lineedit_keyword_%s_args_%s' % (key, p))
                setattr(ui, le.objectName(), le)
                self.add_tooltip(le, key)
                layout.addWidget(le, row, 1, 1, 1)
                le.setEnabled(enabled)
                val = self.project.get_value(key, args=[p])
                if val is not None:
                    le.updateValue(key, val)
                self.project.register_widget(le, keys=[key], args=[p])

        # Use dynamic_widgets here instead of setattr/getattr
        for key in 'e_young', 'v_poisson':
            for p in range(1, 1+len(self.solids.keys())):
                for pat in 'label_%s_args_%s', 'lineedit_keyword_%s_args_%s':
                    w = getattr(ui, pat%(key,p), None)
                    if w:
                        w.setEnabled(enabled)

        #Specify normal restitution coefficient
        # Specification always required
        # Sets keyword DES_EN_INPUT (DES_EN_WALL_INPUT)
        # Input given as an upper triangular matrix
        mmax = self.project.get_value('mmax', default=len(self.solids))
        tw = ui.tablewidget_des_en_input
        def make_item(str):
            item = QTableWidgetItem(str)
            set_item_noedit(item)
            set_item_enabled(item, False)
            return item

        if (self.solids_dem_saved_solids_names != solids_names
            or tw.rowCount() != mmax+1
            or tw.columnCount() != mmax):

            # Clear out old lineedit widgets
            for row in range(tw.rowCount()):
                for col in range(tw.columnCount()):
                    w = tw.cellWidget(row, col)
                    if w:
                        self.project.unregister_widget(w)
                        w.deleteLater()
            tw.clearContents()

            # Make a new batch
            tw.setRowCount(mmax+1) # extra row for "Wall"
            tw.setColumnCount(mmax)
            tw.setHorizontalHeaderLabels(solids_names)
            tw.setVerticalHeaderLabels(solids_names + ['Wall'])

            arg = 1 # One-based
            key = 'des_en_input'
            for row in range(mmax):
                for col in range(mmax):
                    if col < row:
                        tw.setItem(row, col, make_item('--'))
                    else:
                        le = LineEdit()
                        le.setMaximumWidth(150)
                        le.key = key
                        le.args = [arg]
                        le.setdtype('dp')
                        self.add_tooltip(le, key)
                        tw.setCellWidget(row, col, le)
                        val = self.project.get_value(key, args=[arg])
                        if val is not None:
                            le.updateValue(key, val)
                        self.project.register_widget(le, keys=[key], args=[arg])
                        arg += 1
            arg = 1
            key = 'des_en_wall_input'
            row = mmax
            for col in range(mmax):
                le = LineEdit()
                le.setMaximumWidth(150)
                le.key = key
                le.args = [arg]
                le.setdtype('dp')
                self.add_tooltip(le, key)
                tw.setCellWidget(row, col, le)
                val = self.project.get_value(key, args=[arg])
                if val is not None:
                    le.updateValue(key, val)
                self.project.register_widget(le, keys=[key], args=[arg])
                arg += 1

        self.fixup_solids_table(tw, stretch_column=mmax-1)
        # This makes the table look a little nicer
        tw.setShowGrid(False)
        # Move column headers to left so they line up with lineedits
        for i in range(tw.columnCount()):
            item =  tw.horizontalHeaderItem(i)
            if item:
                item.setTextAlignment(Qt.AlignLeft)

        #Specify tangential restitution coefficient
        # Specification available for Hertzian collision model
        # Sets keyword DES_ET_INPUT (DES_ET_WALL_INPUT)
        # Input given as an upper triangular matrix
        enabled = (des_coll_model=='HERTZIAN')
        ui.label_des_et_input.setEnabled(enabled)
        tw = ui.tablewidget_des_et_input
        # note - this is too much of a duplicate of des_en_input above
        if not enabled:
            # Clear out old lineedit widgets
            for row in range(tw.rowCount()):
                for col in range(tw.columnCount()):
                    w = tw.cellWidget(row, col)
                    if w:
                        self.project.unregister_widget(w)
                        w.deleteLater()
            tw.clearContents()
            tw.setRowCount(0)
            tw.setColumnCount(0)

        if enabled:
            if (self.solids_dem_saved_solids_names != solids_names
                or tw.rowCount() != mmax+1
                or tw.columnCount() != mmax):

                # Clear out old lineedit widgets
                for row in range(tw.rowCount()):
                    for col in range(tw.columnCount()):
                        w = tw.cellWidget(row, col)
                        if w:
                            self.project.unregister_widget(w)
                            w.deleteLater()
                tw.clearContents()
                # Make a new batch
                tw.setRowCount(mmax+1) # extra row for "Wall"
                tw.setColumnCount(mmax)
                tw.setHorizontalHeaderLabels(solids_names)
                tw.setVerticalHeaderLabels(solids_names + ['Wall'])

                arg = 1
                key = 'des_et_input'
                for row in range(mmax):
                    for col in range(mmax):
                        if col < row:
                            tw.setItem(row, col, make_item('--'))
                        else:
                            le = LineEdit()
                            le.setMaximumWidth(150)
                            le.key = key
                            le.args = [arg]
                            le.setdtype('dp')
                            self.add_tooltip(le, key)
                            tw.setCellWidget(row, col, le)
                            val = self.project.get_value(key, args=[arg])
                            if val is not None:
                                le.updateValue(key, val)
                            self.project.register_widget(le, keys=[key], args=[arg])
                            arg += 1
                key = 'des_et_wall_input'
                row = mmax
                arg = 1
                for col in range(mmax):
                    le = LineEdit()
                    le.setMaximumWidth(150)
                    le.key = key
                    le.args = [arg]
                    le.setdtype('dp')
                    tw.setCellWidget(row, col, le)
                    val = self.project.get_value(key, args=[arg])
                    if val is not None:
                        le.updateValue(key, val)
                    self.project.register_widget(le, keys=[key], args=[arg])
                    arg += 1
        self.fixup_solids_table(tw, stretch_column=mmax-1)
        # This makes the table look a little nicer
        tw.setShowGrid(False)
        # Move column headers to left so they line up with lineedits
        for i in range(tw.columnCount()):
            item = tw.horizontalHeaderItem(i)
            if item:
                item.setTextAlignment(Qt.AlignLeft)

        #Select cohesion model
        # Selection always available
        # Available selections
        #None [DEFAULT]
        #Selection always available
        #Sets keyword USE_COHESION to false
        #Sets keyword VAN_DER_WAALS to false
        #Van der Waals
        #Selection always available
        #Sets keyword USE_COHESION to true
        #Sets keyword VAN_DER_WAALS to true
        use_cohesion = self.project.get_value('use_cohesion')
        van_der_waals = self.project.get_value('van_der_waals')
        cb = ui.combobox_cohesion_model
        if use_cohesion:
            if not van_der_waals:
                self.warn('inconsistent value for keyword van_der_waals')
                self.unset_keyword('van_der_waals')
            cb.setCurrentIndex(1)
        else:
            if van_der_waals:
                self.warn('inconsistent value for keyword van_der_waals')
                self.update_keyword('van_der_waals', True)
            cb.setCurrentIndex(0)

        #Specify Hamaker constant
        # Specification only available for Van der Waals cohesion model
        # Sets keyword HAMAKER_CONSTANT (WALL_HAMAKER_CONSTANT)
        #Specify outer cutoff
        # Specification only available for Van der Waals cohesion model
        # Sets keyword VDW_OUTER_CUTOFF (WALL_OUTER_CUTOFF)
        #Specify inner cutoff
        # Specification only available for Van der Waals cohesion model
        # Sets keyword VDW_INNER_CUTOFF (WALL_INNER_CUTOFF)
        #Specify asperities
        # Specification only available for Van der Waals cohesion model
        # Sets keyword ASPERITIES
        enabled = bool(van_der_waals)
        ui.groupbox_cohesion_parameters.setEnabled(enabled)
        # (settings handled by keyword widgets.  TODO:
        #  decide if we want to unset keywords if not enabled

        #List the following options under an 'Advanced' section header.
        #Select Neighbor Search Method
        # Selection always available
        # Available selection
        #Grid-based [DEFAULT]
        #Selection always available
        #Sets keyword DES_NEIGHBOR_SEARCH 4
        #N-Square
        #Selection always available
        #Sets keyword DES_NEIGHBOR_SEARCH 1
        des_neighbor_search = self.project.get_value('des_neighbor_search', default=4)
        if des_neighbor_search not in (1, 4):
            self.warn("Invalid des_neighbor_search %s" % des_neighbor_search)
            des_neighbor_search = 4
            self.update_keyword('des_neighbor_search', des_neighbor_search)
        cb = ui.combobox_des_neighbor_search
        cb.setCurrentIndex(0 if des_neighbor_search==4 else 1)

        #Specify maximum steps between neighbor search
        #Specification always available
        # Sets keyword NEIGHBOR_SEARCH_N
        #Specify factor defining particle neighborhood
        #Specification always available
        # Sets keyword FACTOR_RLM
        #Specify neighborhood search radius ratio
        #Specification always available
        #Sets keyword NEIGHBOR_SEARCH_RAD_RATIO
        #Specify search grid partitions (optional)
        #Specification always available
        #Sets keyword DESGRIDSEARCH_IMAX
        #Sets keyword DESGRIDSEARCH_JMAX
        #Sets keyword DESGRIDSEARCH_KMAX
        pass # handled by keyword widgets

        #Enable user scalar tracking
        #Selection always available
        #Does not directly set any keywords
        #Enables specification of number of user scalars
        # Sets keyword DES_USR_VAR_SIZE
        des_usr_var_size = self.project.get_value('des_usr_var_size', default=None)
        enabled = (des_usr_var_size is not None)
        cb = ui.checkbox_enable_des_usr_var_size
        cb.setChecked(enabled)
        ui.lineedit_keyword_des_usr_var_size.setEnabled(enabled)

        #Define minimum distance for contact conduction (optional)
        #Unavailable if not solving energy equations
        #Define fluid lens proportion constant (optional)
        # Unavailable if not solving energy equations
        enabled = self.project.get_value('energy_eq', default=True)
        for item in (ui.label_des_min_cond_dist,
                     ui.lineedit_keyword_des_min_cond_dist,
                     ui.label_des_min_cond_dist_units,
                     ui.label_flpc,
                     ui.lineedit_keyword_flpc):
            item.setEnabled(enabled)

        # Remember the names of solids phases, to track changes
        self.solids_dem_saved_solids_names = solids_names
        # Fin!
