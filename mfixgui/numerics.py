import logging

from qtpy.QtWidgets import QHeaderView

from mfixgui.constants import *

from mfixgui.tools.qt import get_combobox_item, sub_icon_height

from mfixgui.widgets.base import LineEdit, ComboBox

#from mfixgui.project import FloatExp

(TAB_RESIDUALS, TAB_DISCRETIZATION, TAB_LINEAR_SOLVER,
 TAB_PRECONDITIONER, TAB_ADVANCED) = range(5)

# Discretization table
(COL_SCHEME, COL_RELAX) = (0, 1)
DISCRETIZATION_NAMES = [
    "First-order upwind",
    "First-order upwind (dwf)",
    "Superbee",
    "SMART",
    "ULTRA-QUICK",
    "QUICKEST",
    "MUSCL",
    "van Leer",
    "minmod",
    "Central"
]

log = logging.getLogger(__name__)

# Linear solver table
(COL_SOLVER, COL_ITER, COL_TOL) = (0, 1, 2)
(BICGSTAB, GMRES) = (2, 3)
LEQ_METHODS = [BICGSTAB, GMRES]
LEQ_METHOD_NAMES = ['BiCGSTAB', 'GMRES']


# Preconditioner table
(COL_PRECON, COL_SWEEP) = (0, 1)
PRECON_NAMES = ['None', 'Line Relaxation', 'Diagonal Scaling']
SWEEP_NAMES = ['Red-black sweep', 'All sweep', 'I-sweep', 'J-sweep', 'K-sweep']


class Numerics(object):
    """Numerics Task Pane Window

    The numerics input is split into tabs to group similar inputs and reduce
    the amount of input needed on a single pane.

    """

    def init_numerics(self):
        ui = self.ui.numerics
        self.numerics_pushbuttons = (ui.pushbutton_residuals,
                                     ui.pushbutton_discretization,
                                     ui.pushbutton_linear_solver,
                                     ui.pushbutton_preconditioner,
                                     ui.pushbutton_advanced)
        for (i, btn) in enumerate(self.numerics_pushbuttons):
            btn.pressed.connect(lambda i=i, btn=btn: self.numerics_change_tab(i, btn))

        self.init_numerics_residuals_pane()
        self.init_numerics_discretization_pane()
        self.init_numerics_linear_solver_pane()
        self.init_numerics_preconditioner_pane()
        self.init_numerics_advanced_pane()
        self.numerics_current_tab = TAB_RESIDUALS


    def init_numerics_residuals_pane(self):
        pass

    def init_numerics_discretization_pane(self):
        # Add some extra tooltips
        ui = self.ui.numerics
        cb = ui.combobox_cn_on
        key = 'cn_on'
        cb.currentIndexChanged.connect(self.set_cn_on)
        self.add_tooltip(get_combobox_item(cb, 0), key, value='.FALSE.')
        self.add_tooltip(get_combobox_item(cb, 1), key, value='.TRUE.')

        tw = ui.tablewidget_discretization
        key = 'discretize'
        self.add_tooltip(tw.horizontalHeaderItem(COL_SCHEME), key)
        key = 'ur_fac'
        self.add_tooltip(tw.horizontalHeaderItem(COL_RELAX), key)

        # Populate table with combobox and lineedit widgets

        for i in range(1, 1+DIM_EQS):
            row = i-1
            key = 'discretize'
            cb = ComboBox()
            cb.args = [i]
            for name in DISCRETIZATION_NAMES:
                cb.addItem(name)
            for j in range(len(DISCRETIZATION_NAMES)):
                item = get_combobox_item(cb, j)
                item.args = [i]
                self.add_tooltip(item, key, value=j)
            cb.setToolTip(get_combobox_item(cb, 0).toolTip())
            cb.currentIndexChanged.connect(lambda val, i=i: self.set_discretize(val, i))
            tw.setCellWidget(row, COL_SCHEME, cb)

            key = 'ur_fac'
            le = LineEdit()
            le.dtype = float
            le.key = key
            le.args = [i]
            tw.setCellWidget(row, COL_RELAX, le)
            le.value_updated.connect(self.project.submit_change)
            self.add_tooltip(le, key)


    def init_numerics_linear_solver_pane(self):
        ui = self.ui.numerics
        tw = ui.tablewidget_linear_solver
        # Add some tooltips
        key = 'leq_method'
        self.add_tooltip(tw.horizontalHeaderItem(COL_SOLVER), key)
        key = 'leq_it'
        self.add_tooltip(tw.horizontalHeaderItem(COL_RELAX), key)
        key = 'leq_tol'
        self.add_tooltip(tw.horizontalHeaderItem(COL_TOL), key)

        # Populate table with combobox and lineedit widgets
        for i in range(1, 1+DIM_EQS):
            row = i-1
            key = 'leq_method'
            cb = ComboBox()
            cb.args = [i]
            for (j, name) in enumerate(LEQ_METHOD_NAMES):
                cb.addItem(name)
            for (j, method) in enumerate(LEQ_METHODS):
                item = get_combobox_item(cb, j)
                item.args = [i]
                self.add_tooltip(item, key, value=method)

            cb.setToolTip(get_combobox_item(cb,0).toolTip())
            cb.currentIndexChanged.connect(lambda val, i=i: self.set_leq_method(val, i))
            tw.setCellWidget(row, COL_SOLVER, cb)

            key = 'leq_it'
            le = LineEdit()
            le.dtype = int
            le.min = 0
            le.key = key
            le.args = [i]
            tw.setCellWidget(row, COL_ITER, le)
            le.value_updated.connect(self.project.submit_change)
            self.add_tooltip(le, key)

            key = 'leq_tol'
            le = LineEdit()
            le.dtype = float
            le.min = 0
            le.key = key
            le.args = [i]
            tw.setCellWidget(row, COL_TOL, le)
            le.value_updated.connect(self.project.submit_change)
            self.add_tooltip(le, key)


    def init_numerics_preconditioner_pane(self):
        ui = self.ui.numerics
        tw = ui.tablewidget_preconditioner
        # Add some tooltips to column headers
        key = 'leq_pc'
        self.add_tooltip(tw.horizontalHeaderItem(COL_PRECON), key)
        key = 'leq_sweep'
        self.add_tooltip(tw.horizontalHeaderItem(COL_SWEEP), key)
        # Populate table
        for i in range(1, 1+DIM_EQS):
            row = i-1
            key = 'leq_pc'
            cb = ComboBox()
            cb.args = [i]
            for j,name in enumerate(PRECON_NAMES):
                cb.addItem(name)
                item = get_combobox_item(cb, j)
                item.args = [i]
                self.add_tooltip(item, key, value=PRECON_TYPES[j])

            cb.currentIndexChanged.connect(lambda val, i=i: self.set_preconditioner(val, i))
            tw.setCellWidget(row, COL_PRECON, cb)
            self.add_tooltip(cb, key)

            key = 'leq_sweep'
            cb = ComboBox()
            cb.args = [i]
            for j,name in enumerate(SWEEP_NAMES):
                cb.addItem(name)
                item = get_combobox_item(cb, j)
                item.args = [i]
                self.add_tooltip(item, key, value=SWEEP_TYPES[j])
            cb.currentIndexChanged.connect(lambda val, i=i: self.set_sweep(val, i))
            tw.setCellWidget(row, COL_SWEEP, cb)
            self.add_tooltip(cb, key)


    def init_numerics_advanced_pane(self):
        pass


    def set_cn_on(self, val):
        ui = self.ui.numerics
        cb = ui.combobox_cn_on
        key = 'cn_on'
        self.update_keyword(key, bool(val))
        cb.setToolTip(get_combobox_item(cb, int(val)).toolTip())


    def set_discretize(self, val, index):
        ui = self.ui.numerics
        key = 'discretize'
        self.update_keyword(key, val, args=[index])
        cb = ui.tablewidget_discretization.cellWidget(index-1, COL_SCHEME)
        cb.setToolTip(get_combobox_item(cb, val).toolTip())
        self.setup_numerics() # update checkboxes etc


    def set_leq_method(self, val, index):
        ui = self.ui.numerics
        cb = ui.tablewidget_linear_solver.cellWidget(index-1, COL_SOLVER)
        cb.setToolTip(get_combobox_item(cb, val).toolTip())
        key = 'leq_method'
        self.update_keyword(key, LEQ_METHODS[val], args=[index])
        cb.setToolTip(get_combobox_item(cb, val).toolTip())
        #self.setup_numerics() # not needed


    def set_preconditioner(self, val, index):
        ui = self.ui.numerics
        #cb = ui.tablewidget_preconditioner.cellWidget(index-1, COL_PRECON)
        #cb.setToolTip(get_combobox_item(cb, val).toolTip()) # No useful per-item values
        key = 'leq_pc'
        pc_type = PRECON_TYPES[val]
        if pc_type != self.project.get_value(key, default='LINE', args=[index]):
            self.update_keyword(key, pc_type, args=[index])
            self.setup_numerics()


    def set_sweep(self, val, index):
        ui = self.ui.numerics
        cb = ui.tablewidget_preconditioner.cellWidget(index-1, COL_SWEEP)
        cb.setToolTip(get_combobox_item(cb, val).toolTip())
        key = 'leq_sweep'
        self.update_keyword(key, SWEEP_TYPES[val], args=[index])
        cb.setToolTip(get_combobox_item(cb, val).toolTip())
        self.setup_numerics()


    # Numerics sub-pane navigation
    def numerics_change_tab(self, tabnum, to_btn):
        ui = self.ui.numerics
        self.numerics_current_tab = tabnum
        self.animate_stacked_widget(
            ui.stackedwidget_numerics,
            ui.stackedwidget_numerics.currentIndex(),
            tabnum,
            direction='horizontal',
            line=ui.line_numerics,
            to_btn=to_btn,
            btn_layout=ui.gridlayout_tab_btns)
        self.setup_numerics_tab(tabnum)
        for btn in self.numerics_pushbuttons:
            btn.setChecked(btn==to_btn)
            font = btn.font()
            font.setBold(btn==to_btn)
            btn.setFont(font)


    def fixup_numerics_table(self, tw, stretch_column=1):
        # TODO catch resize & call fixup
        ui = self.ui.numerics
        hv = QHeaderView
        resize = tw.horizontalHeader().setSectionResizeMode
        ncols = tw.columnCount()
        for n in range(0, ncols):
            resize(n, hv.Stretch if n==stretch_column else hv.ResizeToContents)

        # trim excess vertical space - can't figure out how to do this in designer
        header_height = tw.horizontalHeader().height()
        scrollbar_height = tw.horizontalScrollBar().isVisible() * (4+tw.horizontalScrollBar().height())
        nrows = tw.rowCount()

        height =  (header_height+scrollbar_height
                   + nrows*tw.rowHeight(0)) # extra to avoid unneeded scrollbar
        tw.setMaximumHeight(height)
        tw.setMinimumHeight(height)
        if tw == ui.tablewidget_discretization:
            icon_height = sub_icon_height() + 8
            ui.groupbox_discretization.setMaximumHeight(height+icon_height)
        tw.updateGeometry() #? needed?


    def setup_numerics(self):
        self.setup_numerics_tab(self.numerics_current_tab)


    def setup_numerics_tab(self, tabnum):
        if tabnum == TAB_RESIDUALS:
            self.numerics_setup_residuals_tab()
        elif tabnum == TAB_DISCRETIZATION:
            self.numerics_setup_discretization_tab()
        elif tabnum == TAB_LINEAR_SOLVER:
            self.numerics_setup_linear_solver_tab()
        elif tabnum == TAB_PRECONDITIONER:
            self.numerics_setup_preconditioner_tab()
        elif tabnum == TAB_ADVANCED:
            self.numerics_setup_advanced_tab()
        else:
            raise ValueError(tabnum)


    def reset_numerics(self):
        pass
        # Set all numeric-related state back to default


    def numerics_setup_residuals_tab(self):
        """Residuals (tab)"""
        #Specify residual for continuity plus momentum equations
        #    Specification always available
        #    Sets keyword TOL_RESID
        #    DEFAULT 1.0e-3
        #Specify residual for energy equations
        #    Specification always available
        #    Sets keyword TOL_RESID_T
        #    DEFAULT 1.0e-4
        #Specify residual for species equations
        #    Specification always available
        #    Sets keyword TOL_RESID_X
        #    DEFAULT 1.0e-4
        #Specify residual for granular energy equations
        #    Specification always available
        #    Sets keyword TOL_RESID_TH
        #    DEFAULT 1.0e-4
        #Specify residual for scalar/K-Epsilon
        #    Specification always available
        #    Sets keyword TOL_RESID_SCALAR
        #    DEFAULT 1.0e-4
        # handled by keyword widgets
        pass

    def numerics_setup_discretization_tab(self):
        """Discretization (tab)"""
        ui = self.ui.numerics
        #Select temporal discretization scheme
        #    Selection always available
        #    Available selections:
        #  Implicit Euler [DEFAULT]
        #    Sets keyword CN_ON to .FALSE.
        #  Crank-Nicolson
        #    Sets keyword CN_ON to .TRUE.
        cb = ui.combobox_cn_on
        idx = 1 if self.project.get_value('cn_on') else 0
        cb.setCurrentIndex(idx)
        cb.setToolTip(get_combobox_item(cb, idx).toolTip())

        tw = ui.tablewidget_discretization
        self.fixup_numerics_table(tw)

        #  Available selections
        #    First-order upwind [DEFAULT for all equations]
        # Sets keyword DISCRETIZE(#) to 0
        #    First-order upwind (dwf)
        # Sets keyword DISCRETIZE(#) to 1
        #    Superbee
        # Sets keyword DISCRETIZE(#) to 2
        #    SMART
        # Sets keyword DISCRETIZE(#) to 3
        #    ULTRA-QUICK
        # Sets keyword DISCRETIZE(#) to 4
        #    QUICKEST
        # Sets keyword DISCRETIZE(#) to 5
        #    MUSCL
        # Sets keyword DISCRETIZE(#) to 6
        #    van Leer
        # Sets keyword DISCRETIZE(#) to 7
        #    minmod
        # Sets keyword DISCRETIZE(#) to 8
        #    Central
        # Sets keyword DISCRETIZE(#) to 9
        # see "make_combobox"

        #    Column 3: Specify under relaxation factors
        #  Specification always available
        #  Sets keyword UR_FAC for each equation #
        #  DEFAULTS are equation type specific
        defaults = [None,#    0 - unused
                    0.8, #    1 - gas pressure: 0.8
                    0.5, #    2 - volume fraction: 0.5
                    0.5, #    3 - u-momentum: 0.5
                    0.5, #    4 - v-momentum: 0.5
                    0.5, #    5 - w-momentum: 0.5
                    1.0, #    6 - energy: 1.0
                    1.0, #    7 - species: 1.0
                    0.5, #    8 - granular energy: 0.5
                    0.8, #    9 - user-scalar/k-epsilon: 0.8
                    1.0] #    10 - DES diffusion: 1.0

        for i in range(1, 1+DIM_EQS):
            row = i-1
            key = 'discretize'
            default = 0
            cb = tw.cellWidget(row, COL_SCHEME)
            val = self.project.get_value(key, args=[i])
            if val is None:
                val = default
                #self.update_keyword(key, val, args=[i]) #?
            cb.setCurrentIndex(val)

            key = 'ur_fac'
            le = tw.cellWidget(row, COL_RELAX)
            val = self.project.get_value(key, args=[i])
            if val is None:
                val = defaults[i]
                #self.update_keyword(key, val, args=[i]) #?
            le.updateValue(key, val) # initialize lineedit, since it's not registered

        #Enable deferred correction
        #    Selection only available if minval(discretize) > 0
        #    Sets keyword DEF_COR
        #    DEFAULT value .FALSE.
        key = 'discretize'
        enabled = min(self.project.get_value(key, args=[i], default=0)
                      for i in range(1, 1+DIM_EQS)) > 0
        key = 'def_cor'
        cb = ui.checkbox_keyword_def_cor
        cb.setEnabled(enabled)
        self.add_tooltip(cb, key)
        if not enabled:
            cb.setToolTip(cb.toolTip() + " Disabled for first-order upwinding.")
            cb.setChecked(False)
            self.unset_keyword(key)

        #Enable chi-scheme correction
        #    Selection only available if the species equation spatial discretization is SMART or
        #MUSCL (DISCRETIZE(7) = 3 or 6)
        #    Sets keyword CHI_SCHEME
        #    DEFAULT value .FALSE.
        enabled = self.project.get_value('discretize', args=[7]) in (3,6)
        key = 'chi_scheme'
        cb = ui.checkbox_keyword_chi_scheme
        cb.setEnabled(enabled)
        if not enabled:
            cb.setChecked(False)
            self.unset_keyword(key)


    def numerics_setup_linear_solver_tab(self):
        """Linear Solver (tab)"""
        ui = self.ui.numerics
        tw = ui.tablewidget_linear_solver
        #Specify linear solver, number of iterations, and convergence tolerance (table format)
        #    Specification always available
        #    Column 1: List of equations
        #    Column 2: Select linear equation solver method for equation #
        #  Available selections
        #    BiCGSTAB [DEFAULT for all equations]
        # Sets keyword LEQ_METHOD(#) to 2
        #    GMRES
        #  Sets keyword LEQ_METHOD(#) to 3
        #    Column 3: Specify number of iterations
        #  Specification always available
        #  Sets keyword LEQ_IT for each equation #
        #  DEFAULTS are equation type specific
        defaults = [None, #    0 - unused
                    20,   #    1 - gas pressure: 20
                    20,   #    2 - volume fraction: 20
                    5,    #    3 - u-momentum: 5
                    5,    #    4 - v-momentum: 5
                    5,    #    5 - w-momentum: 5
                    15,   #    6 - energy: 15
                    15,   #    7 - species: 15
                    15,   #    8 - granular energy: 15
                    15,   #    9 - user-scalar/k-epsilon: 15
                    10]   #    10 - DES diffusion: 5
        #    Column 4: Specify convergence tolerance
        #  Specification always available
        #  Sets keyword LEQ_TOL
        #  DEFAULT 1.0E-4 for all equations
        self.fixup_numerics_table(tw)
        for i in range(1, 1+DIM_EQS):
            row = i-1
            key = 'leq_method'
            val = self.project.get_value(key, args=[i])
            if val not in LEQ_METHODS:
                if val is not None:
                    self.warning("Invalid %s(%s)=%s" % (key, i, val))
                    val = BICGSTAB # Default
                    self.update_keyword(key, val, args=[i])
                else:
                    val = BICGSTAB # Default
            cb = tw.cellWidget(row, COL_SOLVER)
            cb.setCurrentIndex(LEQ_METHODS.index(val))

            key = 'leq_it'
            val= self.project.get_value(key, args=[i])
            if val is None:
                val = defaults[i]
                #self.update_keyword(key, val, args=[i]) #?
            le = tw.cellWidget(row, COL_ITER)
            le.updateValue(key, val)

            key=  'leq_tol'
            val= self.project.get_value(key, args=[i])
            if val is None:
                val = 1e-4 #FloatExp('1.0e-4')
                #self.update_keyword(key, val, args=[i]) #?
            le = tw.cellWidget(row, COL_TOL)
            le.updateValue(key, val)


    def numerics_setup_preconditioner_tab(self):
        """Preconditioner (tab)"""

        #Specify linear solver, number of preconditioner and sweep direction (table format)
        #    Column 1: List of equations
        #    Specification only available for equations using BiCGSTAB solver

        #    Column 2: Preconditioner for equation #
        #  Available selections
        #    None
        # Sets keyword LEQ_PC(#) to 'NONE'
        #    Line Relaxation [DEFAULT for all equations]
        # Sets keyword LEQ_PC(#) to 'LINE'
        #    Diagonal Scaling
        # Sets keyword LEQ_PC(#) to 'DIAG'

        #    Column 3: Preconditioner sweep direction for equation #
        #  Selection only available for equations with LINE preconditioner
        #  Available selections
        #    'Red-black sweep' [DEFAULT for all equations]
        # Sets keyword LEQ_SWEEP(#) to 'RSRS'
        #    All sweep
        # Sets keyword LEQ_SWEEP(#) to 'ASAS'
        #    I-sweep
        # Sets keyword LEQ_SWEEP(#) to 'ISIS'
        #    J-sweep
        # Sets keyword LEQ_SWEEP(#) to 'JSJS'
        #    K-sweep
        # Sets keyword LEQ_SWEEP(#) to 'KSKS'

        ui = self.ui.numerics
        tw = ui.tablewidget_preconditioner
        self.fixup_numerics_table(tw)
        for i in range(1, 1+DIM_EQS):
            row = i-1
            key = 'leq_pc'
            default = 'LINE'
            cb = tw.cellWidget(row, COL_PRECON)
            enabled = self.project.get_value('leq_method', args=[i], default=BICGSTAB)==BICGSTAB
            cb.setEnabled(enabled)
            if enabled:
                val = self.project.get_value(key, args=[i])
                if val not in PRECON_TYPES:
                    if val is not None:
                        self.warning("Invalid %s(%s)=%s" % (key, i, val))
                        val = default
                        self.update_keyword(key, val, args=[i])
                    else:
                        val = default
                cb.setCurrentIndex(PRECON_TYPES.index(val))
            else:
                cb.currentIndexChanged.disconnect()
                cb.setCurrentIndex(1) # default: LINE
                cb.currentIndexChanged.connect(lambda val, i=i: self.set_preconditioner(val, i))
                self.unset_keyword(key, args=[i])
                self.add_tooltip(cb, key) #TODO explain why it's disabled

            key = 'leq_sweep'
            default = 'RSRS'
            cb = tw.cellWidget(row, COL_SWEEP)
            enabled = enabled and self.project.get_value('leq_pc', args=[i], default='LINE')=='LINE'
            cb.setEnabled(enabled)
            if enabled:
                val = self.project.get_value(key, args=[i])
                if val not in SWEEP_TYPES:
                    if val is not None:
                        self.warning("Invalid %s(%s)=%s" % (key, i, val))
                        val = default
                        self.update_keyword(key, val, args=[i])
                    else:
                        val = default
                cb.setCurrentIndex(SWEEP_TYPES.index(val))
            else:
                cb.currentIndexChanged.disconnect()
                cb.setCurrentIndex(0)
                cb.currentIndexChanged.connect(lambda val, i=i: self.set_sweep(val, i))
                self.unset_keyword(key, args=[i])
                self.add_tooltip(cb, key) #TODO: explain why it's disabled


    def numerics_setup_advanced_tab(self):
        """Advanced (tab)"""
        ui = self.ui.numerics
        #Specify maximum inlet velocity factor
        #    Specification always available
        #    Sets keyword MAX_INLET_VEL_FAC
        #    DEFAULT 1.0
        #    Error check: Value greater than or equal to 1.0
        #Specify drag under relation factor
        #    Specification only available with MFiX-TFM and MFIX-Hybrid solvers
        #    Sets keyword UR_F_GS
        #    DEFAULT 1.0
        #    Error check: Value bounded between 0 and 1
        key = 'ur_f_gs'
        enabled = self.project.solver in (TFM, HYBRID)
        for item in (ui.label_ur_f_gs, ui.lineedit_keyword_ur_f_gs):
            item.setEnabled(enabled)
            self.add_tooltip(item, key)
            if not enabled:
                item.setToolTip(item.toolTip()+"<br>&bull;Only available for TFM and Hybrid solvers")

        #Specify IA theory conductivity under relation factor
        #    Specification only available with KT_TYPE = 'IA_NONEP'
        #    Sets keyword UR_KTH_SML
        #    DEFAULT 1.0
        #    Error check: value bounded between 0 and 1
        key = 'ur_kth_sml'
        enabled = self.project.get_value('kt_type') == 'IA_NONEP'
        for item in (ui.label_ur_kth_sml, ui.lineedit_keyword_ur_kth_sml):
            item.setEnabled(enabled)
            self.add_tooltip(item, key)
            if not enabled:
                item.setToolTip(item.toolTip() + '<br>&bull;Only available with kt_type=IA_NONEP')
