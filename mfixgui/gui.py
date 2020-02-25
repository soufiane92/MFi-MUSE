#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Qt-based user interface for MFiX fluid dynamics engine"""

import argparse
import copy
import datetime
import glob
import json
import logging
import os
import pprint
import re
import shutil
import signal
import sys
import tempfile
import time
import traceback

from collections import OrderedDict

from qtpy import QtGui, QtCore, QtWidgets
from qtpy.QtCore import Qt, Signal

from mfixgui.constants import *

from mfixgui.tools import (SCRIPT_DIRECTORY, convert_string_to_python,
                           get_username, plural,
                           parse_key_with_args, format_key_with_args,
                           rround, case_insensitive)
from mfixgui.tools.qt import (get_icon, get_image_path, get_pixmap,
                              main_icon_size, sub_icon_size,
                              set_main_icon_size, set_sub_icon_size,
                              widget_iter, get_ui)

from mfixgui.tools.keyword_args import keyword_args
from mfixgui.namelistparser import getKeywordDoc
from mfixgui.regexes import re_valid_run_name_qt
from mfixgui.tools.thumbnail import create_thumbnail
from mfixgui.tools.monitor_reader import MonitorReader
from mfixgui import default_values, stylesheets

from mfixgui.bcs import BCS
from mfixgui.ics import ICS
from mfixgui.iss import ISS
from mfixgui.pss import PSS
from mfixgui.chemistry import Chemistry
from mfixgui.dashboard import Dashboard

from mfixgui.fluid_handler import FluidHandler
from mfixgui.solids_handler import SolidsHandler
from mfixgui.scalar_handler import ScalarHandler

from mfixgui.graphic_tabs import GraphicTabs

from mfixgui.interpreter import Interpreter

from mfixgui.job import JobManager
from mfixgui.file_menu import FileMenu
from mfixgui.mesh import Mesh
from mfixgui.model_setup import ModelSetup
from mfixgui.monitors import Monitors
from mfixgui.numerics import Numerics
from mfixgui.output import Output
from mfixgui.project_manager import ProjectManager


from mfixgui.widgets.base import (BaseWidget, CheckBox, ComboBox,
                                  DoubleSpinBox, LineEdit, SpinBox, Table)

#from mfixgui.widgets.help_dialog import HelpDialog
from mfixgui.widgets.new_popup import NewProjectDialog
from mfixgui.widgets.output_selection_popup import OutputSelectionPopup
from mfixgui.widgets.parameter_dialog import ParameterDialog
from mfixgui.widgets.regions import RegionsWidget
from mfixgui.widgets.regions_popup import RegionsPopup
from mfixgui.widgets.run_popup import RunPopup
from mfixgui.widgets.species_popup import SpeciesPopup
from mfixgui.widgets.build_popup import make_build_popup
from mfixgui.widgets.resource_monitor import ResourceMonitor

from mfixgui.version import __version__

support_email = 'mfix-help@mfix.netl.doe.gov'

# Initialize logger early
log = logging.getLogger('mfix-gui' if __name__ == '__main__' else __name__)

SETTINGS = QtCore.QSettings('MFIX', 'MFIX')
ANIMATION_SPEED = int(SETTINGS.value('animation_speed', 400))

# --- Main Gui ---
class MfixGui(QtWidgets.QMainWindow,
              ModelSetup,
              Mesh,
              FluidHandler,
              SolidsHandler,
              ScalarHandler,
              ICS, BCS, PSS, ISS,
              Chemistry,
              Numerics,
              Monitors,
              Output,
              Dashboard,
              GraphicTabs,
              FileMenu,
              Interpreter):

    # Main window class for MFIX-GUI
    settings = SETTINGS
    project_file = None
    stdout_signal = Signal(str)
    stderr_signal = Signal(str)
    signal_update_runbuttons = Signal(str)

    # Allow LineEdit widgets to report out-of-bounds values.
    def popup_value_error(self, exc):
        text = str(exc)
        text = text.replace('>', '&gt;')
        text = text.replace('<', '&lt;')
        self.message(title='Error', text=text)

    # Allow LineEdit widgets to report REQUIRED values.
    def popup_value_required(self, key):
        text = "A value is required for %s\nRestoring previous/default value" % key
        self.message(title='Warning', text=text)

    def error(self, msg, popup=False, print_console=True):
        # Show the user a warning & log it - use this instead of log.error

        if self.message_box and self.message_box.isVisible():
            popup=False # #Avoid cascading error popups
            print_console=True # Don't drop message!
            # FIXME, when printing to console HTML tags show up

        if print_console and not popup:
            self.print_internal('Error: %s' % msg)
        else:
            self.message(title='Error',
                         icon='error',
                         text=msg,
                         buttons=['ok'],
                         default='ok',
                         print_console=print_console)

    def warn(self, msg, popup=False, print_console=True):
        # Show the user a warning & log it - use instead of log.warn
        if self.message_box and self.message_box.isVisible():
            popup=False # Avoid multiple popups
            print_console=True  # But print message, so it's not lost
        if print_console and not popup:
            #print_internal will call log.warn if message starts with "Warning"
            # avoid double-warning for "Project has been converted" message
            if not any(msg.startswith(x) for x in ("Warning:",
                                    "Project has been converted")):
                msg = "Warning: " + msg
            self.print_internal(msg)
        else:
            self.message(text=msg, print_console=print_console)
            # Will also print-internal and log
    warning = warn

    def __init__(self, app, parent=None, project_file=None, loadnodeworks=True,
                 loadvtk=True, set_splash_text=None):
        self.app = app
        QtWidgets.QMainWindow.__init__(self, parent)
        self.setObjectName('mfixgui')
        if project_file is not None:
            self.set_project_file(project_file)

        LineEdit.report_value_error = self.popup_value_error
        LineEdit.report_value_required = self.popup_value_required

        self.setWindowIcon(get_icon('mfix.png'))

        ss = stylesheets.DEFAULT_STYLESHEET
        try:
            s = app.style().objectName()
            if 'macintosh' in s.lower():
                ss += stylesheets.OSX_STYLESHEET
        except:
            pass
        app.setStyleSheet(ss)

        self.message_box = None # for tests to access
        # Initialize data members - make sure these values match 'reset'!
        self.solver_name = None
        self.fluid_solver_disabled = False # infer at load time
        self.mfix_exe = None
        self.mfix_exe_flags = {}
        self.commandline_option_exe = None
        self.run_cmd = None # job launched by run_popup
        self.mfix_available = False
        self.open_succeeded = False
        self.unsaved_flag = False
        self.mtime = 0.0
        self.last_run_msg_time = 0.0
        self.last_run_msg = ''
        self.run_dialog = None
        self.mfix_process = None
        self.pic_enabled = False # Issues/440
        if set_splash_text is not None:
            self.set_splash_text = set_splash_text
        else:
            def noop(text): return
            self.set_splash_text = noop
        self.cpu_ram_timer = None

        # Hack -remove these once better 'restore value' framework exists
        self.saved_ro_g0 = default_values.ro_g0

        # load UI

        self.customWidgets = {
            'LineEdit':      LineEdit,
            'CheckBox':      CheckBox,
            'ComboBox':      ComboBox,
            'DoubleSpinBox': DoubleSpinBox,
            'SpinBox':       SpinBox,
            'Table':         Table}

        self.ui = get_ui('gui.ui')
        self.ui.panes = []
        self.setCentralWidget(self.ui)

        names = ('model_setup',
                 'geometry',
                 'mesh',
                 'regions',
                 'fluid',
                 'solids',
                 'initial_conditions',
                 'boundary_conditions',
                 'point_sources',
                 'internal_surfaces',
                 'chemistry',
                 'numerics',
                 'output',
                 'monitors',
                 'run',
                 'dashboard',
                 #'post-processing',
        )

        for name in names:
            if name == 'regions':  # not loaded from .ui file
                widget = RegionsWidget(parent=self)
            else:
                widget = QtWidgets.QWidget()
                try:
                    get_ui(name+'.ui', widget)
                except Exception as e:
                    # report which ui file it was, otherwise stack trace
                    # is too generic to be helpful.
                    print("Error loading", name+'.ui')
                    # Exception is re-raised don't need to print it here
                    raise

            # assign 'self.ui.general', etc
            setattr(self.ui, name, widget)
            self.ui.stackedWidgetTaskPane.addWidget(widget)
            widget.input_enabled = True
            self.ui.panes.append(widget)
            # set text on splash
            self.set_splash_text('Creating %s widgets'%name)
        # end of ui loading

        # add and hide a progress bar in the status bar
        # this is a hack show text on top of a QtWidgets.QProgressBar
        # add bar first, then label
        self.progress_bar = QtWidgets.QProgressBar()
        self.progress_bar.setTextVisible(False)
        self.ui.horizontallayout_mode_bar.addWidget(self.progress_bar, 0, 5)
        self.progress_bar.hide()
        self.ui.label_status = QtWidgets.QLabel('Ready')
        self.ui.label_status.setAlignment(Qt.AlignRight|Qt.AlignVCenter)
        self.ui.horizontallayout_mode_bar.addWidget(self.ui.label_status, 0, 5)

        # create cpu and ram resource progress bars
        self.resource_monitor = ResourceMonitor(self.settings)
        self.ui.horizontallayout_mode_bar.addWidget(self.resource_monitor, 0, 6)
        self.resource_monitor.show(int(self.settings.value('show_resources', 0)))

        # build keyword documentation from namelist docstrings
        self.keyword_doc = getKeywordDoc()

        # Fixes
        doc = self.keyword_doc['turbulence_model']
        doc['valids'] = OrderedDict((('NONE', {'note': 'No turbulence model'}),
                                     (TURBULENCE_MODELS[1], doc['valids'][TURBULENCE_MODELS[1]]),
                                     (TURBULENCE_MODELS[2], doc['valids'][TURBULENCE_MODELS[2]])))

        # We're disabling hybrid solver, so don't mention it
        doc = self.keyword_doc['solids_model']
        doc['description'] = doc['description'].split(" For TFM/DEM")[0]

        # A few more ranges etc not mentioned in namelist doc
        self.add_extra_keyword_doc()

        # Setup the navigation tree widget
        tw = self.ui.treewidget_navigation
        self.max_label_len = tw.fontMetrics().width('Boundary Conditions') + 10

        self.nav_labels = [("Model Setup", "Model"),
                           ("Post-processing", "Post"),
                           ("Boundary Conditions", "BCs"),
                           ("Initial Conditions", "ICs"),
                           ("Point Sources", "PSs"),
                           ("Internal Surfaces", "ISs")]

        # Set tooltips for nav tree
        root = tw.invisibleRootItem()
        for i in range(root.childCount()):
            item = root.child(i)
            item.setToolTip(0, item.text(0))
            for j in range(item.childCount()):
                subitem = item.child(j)
                subitem.setToolTip(0, subitem.text(0))

        # Intercept the resize event
        tw.resizeEvent = (lambda old_method:
                          (lambda event:
                           (self._on_resized(event),
                            old_method(event))[-1]))(tw.resizeEvent) # pylint: disable=undefined-variable

        # Initialize popup dialogs
        self.set_splash_text('Initializing Dialogs')
        self.species_popup = SpeciesPopup(QtWidgets.QDialog(), self)
        #self.species_popup.setModal(True) # ?
        self.regions_popup = RegionsPopup(QtWidgets.QDialog(), self)

        # Create project manager
        # NOTE.  it's a ProjectManager, not a Project.  But
        # ProjectManager is a subclass of Project.  Please
        # do not "fix" the code by renaming self.project to
        # self.project_manager
        self.project = ProjectManager(self, self.keyword_doc)

        # Extra setup for fluid & solids panes.  Needs to happen
        # after we create ProjectManager, because widgets get registered
        self.set_splash_text('Creating panes')
        self.init_model_setup()
        self.init_fluid_handler()
        self.init_solids_handler()
        self.init_scalar_handler()
        self.init_mesh()
        self.init_ics()
        self.init_bcs()
        self.init_pss()
        self.init_iss()
        self.init_chemistry()
        self.init_numerics()
        self.init_output()
        self.init_monitors()
        self.init_dashboard()
        self.init_graphics_tabs(loadvtk)
        self.init_keyboard_shortcuts()

        # In-process REPL (for development, should we enable this for users?)
        self.init_interpreter()

        # delete popup
        self.output_selection_popup = OutputSelectionPopup(self)

        # --- animation data
        self.modebuttondict = {'modeler':   self.ui.pushButtonModeler,
                               'nodeworks':  self.ui.pushButtonNodeworks,
                               'developer': self.ui.pushButtonDeveloper,
                               'interpreter': self.ui.pushButtonInterpreter}
        self.animating = False
        self.stack_animation = None

        # --- icons ---
        # loop through all widgets & set icons for any ToolButton with
        # add/delete/copy in the name
        # for mac, remove blue border from QListWidget and QTreeWidget
        # Have to do this after instantiating a QApp
        dpi = self.logicalDpiX()

        if dpi > 96:
            set_main_icon_size(24*dpi/96)
            set_sub_icon_size(16*dpi/96)

        self.set_splash_text('Loading icons')
        for widget in widget_iter(self):
            if isinstance(widget, QtWidgets.QToolButton):
                name = str(widget.objectName())
                if 'add' in name:
                    widget.setIcon(get_icon('add.svg'))
                    widget.setIconSize(sub_icon_size())
                elif 'delete' in name or 'remove' in name:
                    widget.setIcon(get_icon('remove.svg'))
                    widget.setIconSize(sub_icon_size())
                elif 'copy' in name:
                    widget.setIcon(get_icon('copy.svg'))
                    widget.setIconSize(sub_icon_size())
                elif 'up' in name:  # Could match 'update', etc
                    widget.setArrowType(Qt.UpArrow)
                    widget.setIconSize(sub_icon_size())
                elif 'down' in name:
                    widget.setArrowType(Qt.DownArrow)
                    widget.setIconSize(sub_icon_size())
                elif 'left' in name:
                    widget.setArrowType(Qt.LeftArrow)
                    widget.setIconSize(sub_icon_size())
                elif 'right' in name:
                    widget.setArrowType(Qt.RightArrow)
                    widget.setIconSize(sub_icon_size())
            elif isinstance(widget, (QtWidgets.QListWidget, QtWidgets.QTreeWidget)):
                # remove blue outline focus on mac
                widget.setAttribute(Qt.WA_MacShowFocusRect, 0)

            if isinstance(widget, QtWidgets.QAbstractButton):
                # Make sure lineedits lose focus so updates stick!!
                widget.setFocusPolicy(Qt.ClickFocus)

            if isinstance(widget, QtWidgets.QComboBox):
                # Allow mousewheel scrolling, don't disrupt combobox settings
                widget.wheelEvent = lambda ev: None

        # Toolbuttons at top of frame
        ui = self.ui
        ui.toolbutton_help.hide()
        for (button, icon_name, function) in (
                (ui.toolbutton_file, 'menu', self.handle_file_menu),
                (ui.toolbutton_save, 'save', self.handle_save),
                (ui.toolbutton_run_mfix, 'play', self.handle_run),
                (ui.toolbutton_pause_mfix, 'pause', self.handle_pause),
                (ui.toolbutton_stop_mfix, 'stop', self.handle_stop),
                (ui.toolbutton_reset_mfix, 'restore_delete', self.remove_output_files),
                (ui.toolbutton_compile, 'build', self.handle_compile),
                (ui.toolbutton_parameters, 'sliders', self.handle_parameters),
                #(ui.toolbutton_help, 'question_mark', self.handle_help),
                ):

            button.setIcon(get_icon(icon_name+'.svg'))
            button.setIconSize(main_icon_size())
            button.clicked.connect(function)

        ui.toolbutton_stop_mfix.mouseDoubleClickEvent = self.handle_force_stop

        # disable compile until a project is opened
        ui.toolbutton_compile.setEnabled(False)

        # Make sure lineedits lose focus so keywords update before save/run !!
        for button in (ui.toolbutton_run_mfix, ui.toolbutton_save):
            button.setFocusPolicy(Qt.ClickFocus)

        # Geometry toolbuttons
        geo = self.ui.geometry
        for btn, icon in [('toolbutton_add_geometry', 'geometry.svg'),
                          ('toolbutton_add_filter', 'filter.svg'),
                          ('toolbutton_wizard', 'wand.svg'),
                          ('toolbutton_geometry_union', 'union.svg'),
                          ('toolbutton_geometry_intersect', 'intersect.svg'),
                          ('toolbutton_geometry_difference', 'difference.svg')]:
            b = getattr(geo, btn)
            b.setIcon(get_icon(icon))
            b.setIconSize(sub_icon_size())

        # mode (modeler, nodeworks, developer)
        for mode, btn in self.modebuttondict.items():
            btn.pressed.connect(lambda mode=mode: self.change_mode(mode))
            btn.released.connect(lambda btn=btn:  btn.setChecked(True)) # Force button to remain in 'checked' state

        # navigation tree
        ui.toolbutton_collapse_navigation.clicked.connect(self.toggle_nav_menu)
        icon = self.icon_collapse_right = QtGui.QIcon()
        icon.addFile(get_image_path('right_light.svg'), QtCore.QSize(), icon.Normal, icon.Off)
        icon.addFile(get_image_path('right.svg'), QtCore.QSize(), icon.Active, icon.Off)
        icon = self.icon_collapse_left = QtGui.QIcon()
        icon.addFile(get_image_path('left_light.svg'), QtCore.QSize(), icon.Normal, icon.Off)
        icon.addFile(get_image_path('left.svg'), QtCore.QSize(), icon.Active, icon.Off)
        ui.toolbutton_collapse_navigation.setIcon(icon)

        ui.treewidget_navigation.itemSelectionChanged.connect(
            self.navigation_changed)

        # Make tree fully open & non-closable
        # We expect "rootIsDecorated" has been set False in the .ui file
        tree = ui.treewidget_navigation
        tree.expandAll()
        tree.setExpandsOnDoubleClick(False)
        tree.setMaximumWidth(tree.fontMetrics().width('Boundary Conditions') + 10)
        tree.setMinimumWidth(tree.fontMetrics().width('Chemistry') + 10)

        # Make splitters non-collapsing
        collapse = int(self.settings.value('collapse_qsplitter', 0))
        for widget in widget_iter(self):
            if isinstance(widget, QtWidgets.QSplitter):
                widget.setChildrenCollapsible(collapse)

        # Job manager / monitor
        self.set_splash_text('Creating job manager')
        self.job_manager = JobManager(self)
        self.job_manager.sig_change_run_state.connect(self.slot_update_runbuttons)

        self.job_manager.sig_update_job_status.connect(self.slot_update_status)
        self.watch_run_dir_timer = QtCore.QTimer()
        self.watch_run_dir_timer.timeout.connect(self.slot_rundir_timer)
        self.watch_run_dir_timer.start(100)

        # monitor reader
        self.monitor_reader = MonitorReader('csv', self)
        self.project.register_widget(self.monitor_reader, keys=['monitor_name'], args=['*'], force=True)

        # Print welcome message.  Do this early so it appears before any
        # other messages that may occur during this __init__
        self.print_welcome()

        ## Run signals
        self.stdout_signal.connect(self.handle_stdout)
        self.stderr_signal.connect(self.handle_stderr)
        self.signal_update_runbuttons.connect(self.slot_update_runbuttons)

        # --- Register widgets ---
        self.set_splash_text('Enabling controls')
        self.register_keyword_widgets()

        # --- Create file menu ---
        self.set_splash_text('Setting up menu')
        self.init_file_menu()

        # --- vtk setup ---
        self.set_splash_text('Loading vtk')
        self.init_vtk_widget(loadvtk)

        # --- nodeworks setup ---
        self.set_splash_text('Loading nodeworks')
        self.init_nodeworks_widget(loadnodeworks)

        # --  instantiate dialogs
        #self.help_dialog = HelpDialog(self)
        self.parameter_dialog = ParameterDialog(self)

        # --- default --- # do we need this?  note this gets reset in main anyhow
        self.change_mode('modeler')
        self.change_pane('model setup')

        # Update run options
        self.slot_update_runbuttons()

        # Issues/440 PIC and hybrid are not available yet
        self.disable_pic()

        # Set input validator for run_name
        le = self.ui.run.lineedit_keyword_run_name
        le.setValidator(QtGui.QRegExpValidator(
            QtCore.QRegExp(re_valid_run_name_qt)))
        le.required = True

        # Reset everything to default values
        # This is done in 'load_project'.  so why do it now?
        #self.reset() # Clear command_output too?

    def init_keyboard_shortcuts(self):

        shortcuts = [('Ctrl+O', self.handle_open_shortcut),
                     ('Ctrl+N', self.handle_new_shortcut),
                     ('Ctrl+S', self.handle_save),
                     ('Ctrl+R', self.handle_run),
                     ('Ctrl+B', self.handle_compile)]

        for key, callback in shortcuts:
            q = QtWidgets.QShortcut(QtGui.QKeySequence(key), self)
            q.activated.connect(callback)

    def add_extra_keyword_doc(self):
        # Add a little extra knowledge ...
        # TODO , move all of this to *.f
        # These are all fractions, must be btwn 0 and 1, not documented as such
        for key in ('des_em',
                    'eps_f_min',
                    'bc_xw_g',
                    'phip0'):
            self.keyword_doc[key]['validrange'] = {'min':0.0, 'max':1.0}

        # Diameter must be > 0, setting 'min:0' doesn't enforce this, hence this hack
        self.keyword_doc['d_p0']['validrange'] = {'min': 0, 'exclude_endpoints':True}
        # Density also?
        self.keyword_doc['ro_s0']['validrange'] = {'min':0, 'exclude_endpoints':True}

        # ro_g0 must be >0 if set
        self.keyword_doc['ro_g0']['validrange'] = {'min':0} #, 'exclude_endpoints':True}

        # MAX_INLET_VEL_FAC
        # DEFAULT 1.0
        # Error check: Value greater than or equal to 1.0
        self.keyword_doc['max_inlet_vel_fac']['validrange'] = {'min': 1.0}
        # Error check: value bounded between 0 and 1
        self.keyword_doc['ur_kth_sml']['validrange'] = {'min':0.0, 'max':1.0}

        # Number of particles must be non-negative
        self.keyword_doc['particles']['validrange'] = {'min':0.0}

        # Times must be nonnegative
        self.keyword_doc['res_dt']['validrange'] = {'min':0.0}
        self.keyword_doc['res_backups']['validrange'] = {'min': 0}
        self.keyword_doc['nrr']['validrange'] = {'min': 0}

        # Remove mention of 'cylindrical' since we don't support it
        self.keyword_doc['no_k']['description'] = 'Flag to disable the third dimension (i.e., 2D simulation).'

        # Remove this docstring completely - it refers to cylindrical coordinates (annular region)
        del self.keyword_doc['xmin']['description']

        # Don't allow 0 for number of cells
        for key in 'imax', 'jmax', 'kmax':
            self.keyword_doc[key]['validrange']['min'] = 1

        # MW_AVG (issues/402)
        self.keyword_doc['mw_avg']['validrange']['min'] = 0.0

        # pressure must be >= 0
        self.keyword_doc['p_ref']['validrange']['min'] = 0.0

        self.keyword_doc['ro_g0']['description'] = """Specified constant gas density [kg/m^3 in SI]. An equation of
state -the ideal gas law by default- is used to calculate the gas
density if this parameter is undefined. To set this value to zero
and simulate granular flow in a vacuum, select 'Disable fluid solver'
in the 'Model' pane"""

        valids = self.keyword_doc['is_type']['valids']
        for axis in 'XYZ':
            for is_type in 'IMPERMEABLE', 'SEMIPERMEABLE':
                val = "%s_%s" % (axis, is_type)
                valids[val] = valids[is_type].copy()
                valids[val]['value'] = val
                valids[val]['note'] = valids[val]['note'].replace(
                    'through the surface', 'in the %s direction'%axis)

        # Remove details about Carnahan-Starling from rdf_type doc
        key = 'rdf_type'
        doc = self.keyword_doc[key]
        descr =  doc['description']
        doc['description'] = descr.split('.', 1)[0]+'.'
        doc['valids']['CARNAHAN_STARLING'] = {'value': None,
                                              'alias': None,
                                              'note': descr.split('.', 1)[1].strip()}

        # Should we enforce all temperatures > 0 ?


    def set_no_project(self):
        """setup mode when no project is open"""
        self.open_succeeded = False
        self.set_solver(None)
        self.set_project_file(None)
        self.clear_unsaved_flag() # sets save button
        self.update_window_title()
        self.enable_input(False)
        self.fluid_solver_disabled = False
        self.disable_fluid_solver(False)
        self.ui.toolbutton_compile.setEnabled(False) # disable compile
        # This gets set by guess_solver if we're loading a project, otherwise
        # we need to set the default.  (Do other defaults need to be set here?)
        self.status_message("No project - open existing MFiX project or create a new one")
        self.change_pane("model setup")

    def reset(self):
        """Reset all widgets to default values and set GUI to blank-slate"""

        self.change_pane("model setup") # Default pane

        # clean out job
        self.job_manager.disconnect()
        # disconnect stdout/stderror from mfixprocess
        p = self.mfix_process
        if p is not None:
            for sig in [p.error, p.readyReadStandardError, p.started,
                        p.readyReadStandardOutput, p.finished]:
                sig.disconnect()

            self.mfix_process = None

        # Defaults - see __init__
        self.solver_name = None
        self.fluid_solver_disabled = False
        self.saved_ro_g0 = default_values.ro_g0

        self.project.reset() # Clears all keywords & collections

        self.slot_rundir_timer()

        self.reset_model_setup()
        self.reset_fluid()
        self.reset_solids()
        self.reset_scalars()
        self.ui.regions.reset_regions()
        self.reset_ics()
        self.reset_bcs()
        self.reset_iss()
        self.reset_pss()
        self.reset_chemistry()
        self.reset_dashboard()
        self.reset_output()
        self.reset_monitors()
        self.reset_graphics_tabs()
        self.monitor_reader.reset()

        self.saved_ro_g0 = default_values.ro_g0
        self.fluid_solver_disabled = False
        self.disable_fluid_solver(False)

        # Set all custom widgets to default
        for w in widget_iter(self):
            if isinstance(w, BaseWidget):
                w.default()
            elif hasattr(w, 'default'):
                w.default()
            else:
                pass # What to do for rest of widgets?

        # reset parameters
        base_parameters = OrderedDict([(key, 0.0) for key in PARAMETER_DICT])
        PARAMETER_DICT.clear()
        PARAMETER_DICT.update(base_parameters)
        PARAMETER_DICT.update(copy.deepcopy(CONVERSION_TO_METERS))

        self.update_nav_tree()
        self.clear_unsaved_flag()
        self.disable_pic() # Issues/440
        #Workaround for ro_g0 bug
        le = self.ui.fluid.lineedit_keyword_ro_g0
        le.required = False
        le.minimum = 0.0
        #self.set_project_file(None)  - do we want to do this?


    def confirm_close(self):
        """before closing, ask user whether to end job and save project"""

        if self.job_manager.job:
            confirm = self.message(text="Currently running job. Are you sure you want to quit?",
                                   buttons=['ok', 'cancel'],
                                   default='cancel')
            if confirm == 'cancel':
                return False

        if self.unsaved_flag:
            confirm = self.message(text="Save project before quitting?",
                                   buttons=['yes', 'no', 'cancel'],
                                   default='Cancel')
            if confirm == 'yes':
                self.save_project()
            return confirm != 'cancel'
        else:
            return True


    def update_keyword(self, key, value, args=None):
        """like set_keyword but no action if value already set"""
        if isinstance(args, int):
            args = [args]
        elif args is None:
            args = []
        args = self.project.expand_args(args)
        expected_args = keyword_args.get(key)
        if expected_args is None:
            self.error("Unknown keyword %s" % key, popup=True)
        if expected_args is not None:
            if len(args) != len(expected_args):
                self.error("keyword %s: argument mismatch, expected %s, got %s" %
                           (key, str(expected_args), str(args)))
                return
        # Can't do this unless we expand args out first ('BC', etc)
        # if args:
        #     for arg in args:
        #         if not isinstance(arg, int):
        #             self.error("keyword %s: indices must be integer, not %s" %
        #                        (key, repr(arg)))
        #             return
        if value is None or value=='':
            self.unset_keyword(key, args)
            return

        #check whether it's actually changed
        v = self.project.get_value(key, args=args)

        if type(v) == type(value) and str(v) == str(value):
                return
        self.set_unsaved_flag()
        self.project.submit_change(None, {key:value}, args)


    def set_keyword_default(self, key, value, args=None):
        """Set key to default value if not already set"""
        prev = self.project.get_value(key, args=args)
        if prev is None:
            self.update_keyword(key, value, args=args)

    def unset_keyword(self, key, args=None):
        """Undefine keyword.  Report to user, also catch and report any errors"""
        #  Note - keyword is still registered!  This method does not deregister
        #   keywords with project manager
        if args is None:
            args = []
        elif isinstance(args, int):
            args = [args]
        args = self.project.expand_args(args)

        # If any element of 'args' is itself a list, iterate over all values
        # This does not handle more than one list/tuple
        # See similar code in project_manager.change
        if any(isinstance(arg, (list,tuple)) for arg in args):
            copy_args = list(args)
            for (i, arg) in enumerate(args):
                if isinstance(arg, (list,tuple)):
                    for a in arg:
                        copy_args[i] = a
                        self.unset_keyword(key, args=copy_args)
                    break
            return

        try:
            success = self.project.removeKeyword(key, args, warn=False)
            if success:
                self.set_unsaved_flag()
                self.print_internal("%s" % format_key_with_args(key, args),
                                    font='strikeout')
        except Exception as e:
            msg = 'Warning: Failed to unset %s: %s: %s' % (format_key_with_args(key, args),
                                                           e.__class__.__name__, e)
            self.print_internal(msg, color='red')
            traceback.print_exception(*sys.exc_info())

    def _on_resized(self, ev):
        w = ev.size().width()
        if w < self.max_label_len:
            self.short_labels()
        else:
            self.long_labels()

    def short_labels(self):
        tree = self.ui.treewidget_navigation
        flags = Qt.MatchFixedString | Qt.MatchRecursive
        for (long, short) in self.nav_labels:
            items = tree.findItems(long, flags, 0)
            for item in items:
                item.setText(0, short)

    def long_labels(self):
        tree = self.ui.treewidget_navigation
        flags = Qt.MatchFixedString | Qt.MatchRecursive
        for (long, short) in self.nav_labels:
            items = tree.findItems(short, flags, 0)
            for item in items:
                item.setText(0, long)

    def unimplemented(self):
        self.message(title='Unimplemented',
                     text='Feature not implemented')

    def update_nav_tree(self):
        self.ics_update_enabled()
        self.bcs_update_enabled()
        self.pss_update_enabled()
        self.iss_update_enabled()
        self.chemistry_update_enabled()

    def update_region(self, name, data):
        for update in (self.ics_update_region,
                       self.bcs_update_region,
                       self.iss_update_region,
                       self.pss_update_region,
                       self.output_update_region):
            update(name, data)

    def get_region_users(self, name):
        """Return a list of object types referring to region.  Always return a list, even if empty"""
        return [t for (t, check) in (('ICs', self.ics_check_region_in_use),
                                     ('BCs', self.bcs_check_region_in_use),
                                     ('PSs', self.pss_check_region_in_use),
                                     ('ISs', self.iss_check_region_in_use),
                                     ('Output', self.output_check_region_in_use),
                                     ('Monitors', self.monitors_check_region_in_use))
                                     # any more places region can be used?
                if check(name)]

    def change_region_name(self, name, new_name):
        self.bcs_change_region_name(name, new_name)
        self.ics_change_region_name(name, new_name)
        self.iss_change_region_name(name, new_name)
        self.pss_change_region_name(name, new_name)
        self.output_change_region_name(name, new_name)
        self.monitors_change_region_name(name, new_name)
        # any more places region can be used?

    def toggle_nav_menu(self):
        nav_menu = self.ui.treewidget_navigation
        if self.mode != 'modeler':
            self.change_mode('modeler')
            self.change_pane('model setup')
            nav_menu.setVisible(True)
        else:
            nav_menu.setVisible(not nav_menu.isVisible())

        if nav_menu.isVisible():
            self.ui.toolbutton_collapse_navigation.setIcon(self.icon_collapse_left)
        else:
            self.ui.toolbutton_collapse_navigation.setIcon(self.icon_collapse_right)

    def status_message(self, message='', print_in_console=True):
        '''set the status text'''
        if message.strip() == self.ui.label_status.text().strip():
            return
        # pad text (why?)
        message += '  '
        self.ui.label_status.setText(message)

        now = time.time()
        if print_in_console and self.last_run_msg_time:
            # Avoid flooding
            if (  #now - self.last_run_msg_time < 60.0 and
                message.split(':', 1)[0] == self.last_run_msg.split(':', 1)[0]):
                print_in_console = False

        if print_in_console:
            self.last_run_msg_time = now
            self.last_run_msg = message
            self.print_internal(message, color='blue')


    def slot_rundir_timer(self):
        project_dir = self.get_project_dir()
        if project_dir and not self.job_manager.job:
            run_name = self.project.get_value('run_name')
            if run_name:
                pidfile = os.path.join(project_dir, run_name+'.pid')
                self.job_manager.try_to_connect(pidfile)

    def set_run_button(self, text=None, enabled=None):
        b = self.ui.toolbutton_run_mfix
        if text is not None:
            b.setToolTip('Resume previous MFiX run' if text == 'Resume'
                         else text+' MFiX')
        if enabled is not None:
            b.setEnabled(enabled)
            # disable reset (delete) button while a simulation is running (issues/403)
            self.set_reset_button(enabled)

    def set_pause_button(self, text=None, enabled=None):
        b = self.ui.toolbutton_pause_mfix
        if enabled is not None:
            b.setEnabled(enabled)
        if text is not None:
            b.setToolTip(text + ' MFIX')

    def set_stop_button(self, enabled):
        b = self.ui.toolbutton_stop_mfix
        b.setEnabled(enabled)
        # tooltip?

    def set_reset_button(self, enabled):
        files = False
        if enabled:
            files = self.get_output_files()
            if files is None:
                files = False
            else:
                files = len(files) > 0
        b = self.ui.toolbutton_reset_mfix
        b.setEnabled(enabled and files)


    def enable_input(self, enabled=True, partial=False):
        # Enable/disable all inputs (while job running, etc)
        # enable some inputs if partial==True (paused, resumable)
        # TODO clean this up, it knows too much about internals
        # of each pane. Move to bcs.enable, ics.enable, etc
        for pane in self.ui.panes:
            pane.input_enabled = enabled
            name = pane.objectName()
            if name in ['dashboard']:
                continue
            targets = [pane]
            if hasattr(pane, 'detail_pane'):
                targets = [pane.detail_pane]
            elif name == 'numerics':
                targets = [pane.stackedwidget_numerics]
            elif name == 'output':
                targets = [pane.bottom_frame, # bottom of VTK pane, leave table enabledpane.page_basic,
                           pane.page_spx,
                           pane.page_netcdf,
                           pane.page_basic]
            for w in widget_iter(pane):
                if w.objectName().startswith('frame_add_'):
                    targets.append(w)
            if name == 'solids':
                targets.append(self.ui.solids.scrollarea_solids_detail)
                targets.append(self.ui.solids.TFM)
                #targets.append(self.ui.solids.DEM) # Prevents scrolling
                targets.append(self.ui.solids.detail_pane_dem)
                targets.append(self.ui.solids.PIC)

            if name == 'regions':
                self.ui.regions.update_region_parameters()
                targets = []

            for target in targets:
                enable_target = False
                if enabled:
                    enable_target = True
                elif partial:
                    enable_target = (name in {'monitors', 'output'} or
                                     (name in {'boundary_conditions', 'point_sources',
                                               'internal_surfaces', 'numerics', 'run'}
                                      and not target.objectName().startswith('frame_add')))
                target.setEnabled(enable_target)


        # Don't allow changing type of BC.  FIXME, there are a bunch of other
        # items that should be disabled on a per-item basis, should use the 'locked'
        # metadata in keyword_doc
        # Move this to bcs.py ?
        bcs = self.ui.boundary_conditions
        for item in (bcs.combobox_bc_type, # disallow changing type
                     bcs.label_bc_type,
                     bcs.toolbutton_add, # but these are managed by bcs_handle_selection!
                     bcs.toolbutton_delete, #see comment above
                     bcs.toolbutton_up,
                     bcs.toolbutton_down):
            item.setEnabled(enabled)

        # Don't allow changing MONITOR_TYPE
        item = self.ui.monitors.combobox_monitor_type
        item.setEnabled(enabled)

        # Don't allow changing RUN_NAME while paused.
        item = self.ui.run.lineedit_keyword_run_name
        item.setEnabled(enabled)

        # Don't allow enable/disable reactions
        tw = self.ui.chemistry.tablewidget_reactions
        for i in range(0, tw.rowCount()):
            tw.cellWidget(i,0).setEnabled(enabled) # checkbox


    def slot_update_status(self):
        """Get job status from JobManager and update residuals pane and status message"""
        if self.job_manager and self.job_manager.job:
            status_text = pprint.pformat(
                rround(self.job_manager.job.status, 6))
            header = '<html><body><pre>'
            footer = '</pre></body></html>'
            status_text = '%s%s%s' % (header, status_text, footer)
            self.ui.residuals.setText(status_text)

            response_text = self.ui.responses.toPlainText().lower()
            if not response_text:
                self.ui.responses.setText('ok')

            # update plot data
            status = self.job_manager.job.status
            if not self.job_manager.job.is_paused():
                self.update_plots(status)

            # update dashboard
            self.update_dashboard(status)

            finished = status.get('finished')
            running = status.get('running')

            if running:
                # update progress bar
                p = None

                if finished:
                    p = 1.0
                else:
                    t = status.get('time', None)
                    ts = status.get('tstop', None)
                    if t is not None and ts is not None:
                        try:
                            p = t/float(ts)
                        except:
                            pass
                if p is not None:
                    self.progress_bar.setValue(int(p*100))
                self.progress_bar.show()
            else:
                self.progress_bar.hide()

            # update status message
            paused = self.job_manager.job and self.job_manager.job.is_paused()
            if paused:
                pausing = self.job_manager.pausing = False
                stopping = self.job_manager.stopping = False
            else:
                pausing = self.job_manager.pausing
                stopping = self.job_manager.stopping

            w = status.get('walltime_elapsed', 0)
            h, m, s = 0, 0, 0
            try:
                w = float(w)
                m, s = divmod(w, 60)
                h, m = divmod(m, 60)
            except:
                pass

            self.status_message('MFiX %s: elapsed time %d:%02d:%02d' %
                                ('stopping' if stopping
                                 else 'pausing' if pausing
                                 else 'running' if not paused
                                 else 'paused',
                                 h, m, s), print_in_console=True)

            # If the job has finished, collect final output from the webserver
            #  and then tell the server it can go away

            if finished is True and running is False:
                # Collect any stdout/stderr
                if not self.job_manager.is_output_pending():
                    self.job_manager.exit_mfix()

        else:
            log.debug('no Job object (update_status)')
        self.slot_update_runbuttons()


    # This is called by several different signals for different reasons
    # 1) executables changed
    # 2) project directory changed
    # 3) process started
    # 4) process stopped
    def slot_update_runbuttons(self, message=None):
        # This is the main state-transition handler

        if message is not None:
            if not 'running' in message:
                # highlight for visibility, this is an important state change
                self.print_internal(message, color='blue')

        # TODO: set this in __init__ or another early setup method
        # assemble list of available executables

        ui = self.ui
        project_file = os.path.basename(self.get_project_file() or '')
        project_open = bool(project_file and self.open_succeeded)
        pending = self.job_manager.is_job_pending()
        # why both paused and unpaused states?
        paused = self.job_manager.job and self.job_manager.job.is_paused()
        pausing = self.job_manager.pausing
        if paused:
            pausing = False

        unpaused = self.job_manager.job and not paused
        resumable = bool(self.get_res_files()) and not self.job_manager.job
        stopping = self.job_manager.stopping
        self.update_window_title() # put run state in window titlebar

        self.enable_input(enabled=project_open and not (pending or
                                                        unpaused or
                                                        paused or
                                                        resumable),
                          partial=project_open and (paused or resumable))

        #handle buttons in order:  RESET RUN PAUSE STOP

        if pending:
            # also disable spinboxes for dt, tstop unless interactive
            self.set_reset_button(enabled=False)
            self.set_run_button(enabled=False)
            self.set_pause_button(text="Pause", enabled=False)
            self.set_stop_button(enabled=True)

        elif unpaused:
            # also disable spinboxes for dt, tstop unless interactive
            self.set_reset_button(enabled=False)
            self.set_run_button(enabled=False)
            self.set_pause_button(text="Pause", enabled=not(stopping or pausing))
            self.set_stop_button(enabled=True)

        elif paused:
            if not 'paused' in self.ui.label_status.text().lower():
                self.status_message("MFiX paused", print_in_console=False)

            self.set_reset_button(enabled=False)
            self.set_run_button(text="Unpause", enabled=True)
            self.set_pause_button(text="Pause", enabled=False)
            self.set_stop_button(enabled=True)

        elif resumable:
            self.status_message("Previous MFiX run is resumable.  Reset job to edit model")
            self.set_reset_button(enabled=True)
            self.set_run_button(text='Resume', enabled=True)
            self.set_pause_button(text="Pause", enabled=False)
            self.set_stop_button(enabled=False)

        else: # Not running, ready for input
            self.status_message("Ready")
            self.set_reset_button(enabled=False)
            self.set_run_button(text="Run", enabled=project_open)
            self.set_pause_button(text="Pause", enabled=False)
            self.set_stop_button(enabled=False)


    def print_welcome(self):
        self.print_internal("Welcome to MFiX - https://mfix.netl.doe.gov",
                            color='blue')
        self.print_internal("MFiX-GUI version %s" % __version__,
                            color='blue')

    def resizeEvent(self, event):
        '''over-ride of qt resize event'''
        if self.file_menu.isVisible():
            self.file_menu.setGeometry(0, 0, self.width(), self.height())

        QtWidgets.QMainWindow.resizeEvent(self, event)

    def closeEvent(self, event):
        '''over-ride of qt close event'''
        if not self.confirm_close():
            event.ignore()
            return
        # save the state
        self.settings.setValue('geometry', self.saveGeometry())
        self.settings.setValue('splitter_left_right', self.ui.splitter_left_right.sizes())
        self.settings.setValue('splitter_graphics_cmd_output', self.ui.splitter_graphics_cmd_output.sizes())
        self.settings.setValue('mode', self.mode)

        name = self.curr_nav_label
        for (long, short) in self.nav_labels:
            if name == short:
                name = long
                break

        self.settings.setValue('navigation', name)

        # clean up
        self.file_menu.close()

        # stop threads
        self.monitor_reader.stop_threads()

        event.accept()

    def find_navigation_tree_item(self, item_name):
        tree = self.ui.treewidget_navigation
        flags = Qt.MatchFixedString | Qt.MatchRecursive
        items = tree.findItems(item_name, flags, 0)
        if len(items) == 1:
            return items[0]
        else:
            for (long, short) in self.nav_labels:
                if item_name == long:
                    items = tree.findItems(short, flags, 0)
                    if len(items) == 1:
                        return items[0]


    def register_keyword_widgets(self):
        """Look for and connect keyword widgets to the project manager.
        Keyword information from the namelist doc strings is added to each
        keyword widget. The widget must be named: *_keyword_<keyword> where
        <keyword> is the actual keyword.
        Args are also supported via widgetname_keyword_KEY_args_ARGS"""

        def try_int(str):
            try:
                return int(str)
            except ValueError:
                return str

        # loop through all widgets looking for *_keyword_<keyword>
        for widget in widget_iter(self):
            name_list = str(widget.objectName()).split('_')

            if name_list[0] == 'label': # Note no 'keyword'
                if 'args' in name_list:
                    args_idx = name_list.index('args')
                    args = [try_int(name) for name in name_list[args_idx+1:]]
                    key = '_'.join(name_list[1:args_idx])
                else:
                    if name_list[-1].isdigit(): # strip suffix
                        name_list = name_list[:-1]

                    args = None
                    key = '_'.join(name_list[1:])
                if not widget.toolTip():
                    if args:
                        self.add_tooltip(widget, key, value=args[0])
                    else:
                        self.add_tooltip(widget, key)

            elif 'keyword' in name_list:
                key_idx = name_list.index('keyword')
                args = None
                # Look for _args_ following <keyword>
                if 'args' in name_list:
                    args_idx = name_list.index('args')
                    args = [try_int(name) for name in name_list[args_idx+1:]]
                    key = '_'.join(name_list[key_idx+1:args_idx])
                else:
                    key = '_'.join(name_list[key_idx+1:])

                # sometimes multiple widgets point to the same key ...
                # name them widget_keyword_KW_1, _2, etc
                if key not in self.keyword_doc:
                    name_list = key.split('_')
                    if name_list[-1].isdigit():
                        base_key = '_'.join(name_list[:-1])
                        if base_key not in self.keyword_doc:
                            log.error("UNKNOWN KEYWORD %s: not registering %s (also tried %s)" %
                                      (key, widget.objectName(), base_key))
                            continue
                        key = base_key

                if len(keyword_args.get(key, [])) != len(args or []):
                    self.error("keyword args mismatch: key=%s: expected %s, got %s" %
                               (key, keyword_args.get(key), args))

                # set the key attribute to the keyword
                widget.key = key
                widget.args = args

                # add info from keyword documentation
                if key in self.keyword_doc:
                    doc = self.keyword_doc[key]
                    try:
                        widget.setdtype(doc['dtype'])
                    except:
                        print(widget, widget.objectName())
                        raise
                    vr = doc.get('validrange', {})

                    # We're going to ignore the 'required' from keyword doc since
                    # it does not match the SRS.  We will set 'required' fields
                    # manually, in accordance with the SRS
                    #required=doc.get("required"))
                    if vr:
                        widget.setValInfo(min=vr.get('min'), max=vr.get('max'),
                                          exclude_endpoints=vr.get('exclude_endpoints'))


                    # Set widget defaults from 'initpython' keys
                    # Maybe we shoudn't do this, because it leads to the GUI
                    # showing values which are not actually set in the datafile - cgw
                    default = doc.get('initpython') # "Initial Python Value"
                    if default is not None:
                        # Special-case a few vector keys because the initpython is scalar
                        if key == 'tol_snap' and args[0] > 1:
                            continue
                        widget.default(default)
                    if not widget.toolTip():
                        self.add_tooltip(widget, key)

                    # NB not all widgets get set up this way
                    if isinstance(widget, QtWidgets.QLineEdit) and widget.dtype in (int, float):
                        widget.allow_parameters = True
                    elif isinstance(widget, QtWidgets.QComboBox) and widget.count() < 1:
                        widget.addItems(list(doc['valids'].keys()))

                    elif isinstance(widget, QtWidgets.QCheckBox):
                        # TODO: infer which value is true/false?
                        values = sorted([convert_string_to_python(v) for v in doc['valids'].keys()])
                        if len(values) == 2:
                            widget.false_value = values[0]
                            widget.true_value = values[1]

                else:
                    log.error("UNKNOWN KEYWORD %s: not registering %s" % (key, widget.objectName()))
                    continue

                # register the widget with the project manager
                self.project.register_widget(widget, keys=[key], args=args)

    def disable_pic(self):
        # Issues/440 hide PIC and hybrid models by default
        self.pic_enabled = False
        self.ui.solids.pushbutton_solids_pic.hide()
        cb = self.ui.model_setup.combobox_solver
        if len(cb) != 3:
            cb.removeItem(4) #Hybrid
            cb.removeItem(3) #PIC
        cb = self.ui.solids.combobox_solids_model
        if len(cb) != 2:
            cb.removeItem(2) # PIC
        ui = self.ui.initial_conditions
        for item in (ui.label_ic_pic_const_statwt,
                     ui.lineedit_keyword_ic_pic_const_statwt_args_IC_P):
            item.hide()

    def enable_pic(self):
        # Issues/440 hide PIC and hybrid models by default
        self.pic_enabled = True
        self.ui.solids.pushbutton_solids_pic.show()
        cb = self.ui.model_setup.combobox_solver
        # These strings match the .ui files
        if len(cb) != 5:
            cb.addItem("Particle-in-Cell (MFiX-PIC)")
            cb.addItem("MFiX-Hybrid")
        cb = self.ui.solids.combobox_solids_model
        if len(cb) != 3:
            cb.addItem("Multiphase Particle-in-Cell (MFiX-PIC)")
        ui = self.ui.initial_conditions
        for item in (ui.label_ic_pic_const_statwt,
                     ui.lineedit_keyword_ic_pic_const_statwt_args_IC_P):
            item.hide()


    def init_vtk_widget(self, load_vtk=True):
        #initialize the vtk widget
        disable_vtk = False
        if load_vtk and 'MFIX_NO_VTK' not in os.environ: # Avoid importing vtkwidget if MFIX_NO_VTK set
            from mfixgui.widgets.vtkwidget import VTK_AVAILABLE, VTK_IMPORT_INFO
            disable_vtk = not VTK_AVAILABLE
        else: # env var set
            disable_vtk = True
            VTK_IMPORT_INFO = None

        if disable_vtk:
            log.info("MFIX_NO_VTK set or vtk not importable, creating fake VTK")
            # Create a dummy object, so we don't have to test for 'if use_vtk' all over
            class FakeVtk:
                def noop(self, *args, **kwargs):
                    return None
                def __getattr__(self, key):
                    return self if key == 'vtkiren' else self.noop

            self.vtkwidget = FakeVtk()
            self.ui.regions.vtkwidget = self.vtkwidget

            # build widgets to display/copy message
            spacer = QtWidgets.QSpacerItem(10, 100,
                                           QtWidgets.QSizePolicy.Maximum,
                                           QtWidgets.QSizePolicy.Expanding)
            vtk_message = QtWidgets.QWidget()
            vtk_message_l = QtWidgets.QVBoxLayout(vtk_message)
            vtk_message_l.addItem(spacer)
            message = '\n'.join(VTK_IMPORT_INFO) if VTK_IMPORT_INFO is not None else 'VTK disabled.'
            label = QtWidgets.QLabel(message)
            label.setAlignment(Qt.AlignLeft)
            vtk_message_l.addWidget(label)

            if VTK_IMPORT_INFO is not None:
                def copy():
                    cp = self.app.clipboard()
                    cp.setText(message)
                cpy_btn = QtWidgets.QPushButton('Copy Error')
                cpy_btn.pressed.connect(copy)
                vtk_message_l.addWidget(cpy_btn)

            vtk_message_l.addItem(spacer)

            self.ui.horizontalLayoutModelGraphics.addWidget(vtk_message)
            return

        from mfixgui.widgets.vtkwidget import VtkWidget
        self.vtkwidget = VtkWidget(parent=self)
        self.ui.horizontalLayoutModelGraphics.addWidget(self.vtkwidget)

        # remove close btn
        tw = self.ui.tabWidgetGraphics
        rb = tw.tabBar().tabButton(0, QtWidgets.QTabBar.RightSide)
        if rb:
            rb.resize(0, 0)
        lb = tw.tabBar().tabButton(0, QtWidgets.QTabBar.LeftSide)
        if lb:
            lb.resize(0, 0)

        # register with project manager
        self.project.register_widget(
            self.vtkwidget, ['x_min', 'x_max', 'y_min', 'y_max', 'z_min',
                             'Z_max', 'imax', 'jmax', 'kmax', 'no_k',
            ])

        # add reference to other widgets
        self.ui.regions.vtkwidget = self.vtkwidget

    def init_nodeworks_widget(self, load_nodeworks=True):
        # initialize the nodeworks widgets if nodeworks is available
        if load_nodeworks:
            from mfixgui.widgets.nodeworks import NodeworksWidget, NODEWORKS_AVAILABLE
            if NODEWORKS_AVAILABLE:
                self.ui.nodeworks_widget = NodeworksWidget(self.project, self)
                self.ui.nodeworks_widget.NODEWORKS_AVAILABLE = True
                self.ui.verticallayout_nodeworks_mode.addWidget(
                    self.ui.nodeworks_widget)
            else:
                self.make_fake_nodeworks()
        else:
            self.make_fake_nodeworks()

    def make_fake_nodeworks(self):
        class FakeNodeworks:
            NODEWORKS_AVAILABLE = False
            def noop(self, *args, **kwargs):
                return None
            def __getattr__(self, key):
                return self if key == 'nodeChart' else self.noop

        self.ui.nodeworks_widget = FakeNodeworks()
        self.ui.pushButtonNodeworks.setEnabled(False)
        self.ui.pushButtonNodeworks.setToolTip(
            "Nodeworks disabled, can't import nodeworks")

    @classmethod
    def get_project_file(cls):
        """get the project filename, including full path"""
        return cls.project_file if cls.project_file else None

    @classmethod
    def set_project_file(cls, value):
        cls.project_file = value = os.path.normpath(value) if value else None
        cls.settings.setValue('project_file', value)

    def get_project_dir(self):
        """get the current project directory"""
        project_file = self.get_project_file()
        project_file = os.path.normpath(project_file) if project_file else None
        return os.path.dirname(project_file) if project_file else None

    def change_mode(self, mode):
        """change the Modeler, Nodeworks, Developer tab"""
        self.mode = mode
        to_index = None
        if mode == 'interpreter':
            self.capture_output(True)
            self.setup_interpreter()
        else:
            self.capture_output(False)
        for i in range(self.ui.stackedwidget_mode.count()):
            widget = self.ui.stackedwidget_mode.widget(i)
            if mode == str(widget.objectName()):
                to_index = i
                break
        if to_index is None:
            self.error("Invalid mode %s" % mode)
            return

        for key, btn in self.modebuttondict.items():
            btn.setChecked(mode == key)

        nodeworks = (mode == 'nodeworks')
        if self.ui.nodeworks_widget is None:
            self.ui.nodeworks_widget.NODEWORKS_AVAILABLE = False

        if self.ui.nodeworks_widget.NODEWORKS_AVAILABLE:
            nc = self.ui.nodeworks_widget.nodeChart
            for btn in nc.enable_disable_btns:
                btn.setVisible(nodeworks)

        ui = self.ui
        for btn in (ui.toolbutton_reset_mfix, ui.toolbutton_run_mfix,
                    ui.toolbutton_pause_mfix, ui.toolbutton_stop_mfix):
            btn.setVisible(not nodeworks)

        self.animate_stacked_widget(self.ui.stackedwidget_mode,
                                    self.ui.stackedwidget_mode.currentIndex(),
                                    to_index,
                                    'horizontal')

        if mode == 'modeler': # open navigation menu whenever we go back to Modeler mode
            ui.treewidget_navigation.setVisible(True)
            self.ui.toolbutton_collapse_navigation.setIcon(self.icon_collapse_left)

    # --- modeler pane navigation ---
    def change_pane(self, name):
        name = name.replace('_', ' ')
        """set current pane to the one matching 'name'.  Must be the long
        (non-abbreviated) navigation label.  Case-insensitive"""
        items = self.ui.treewidget_navigation.findItems(
            name,
            Qt.MatchFixedString | Qt.MatchRecursive, 0)
        if not items: # Nav menu may be in abbreviated mode.  Might be better
            # to identify navigation items by something other than text, since
            # that can change (long/short) and is possibly non-unique (eg "points")
            for (long, short) in self.nav_labels:
                if name.lower() == long.lower():
                    items = self.ui.treewidget_navigation.findItems(
                        short,
                        Qt.MatchFixedString | Qt.MatchRecursive, 0)
                    if items:
                        break

        if len(items) == 1:
            self.ui.treewidget_navigation.setCurrentItem(items[0])
        else:
            self.error("Cannot find pane %s" % name)
        #self.navigation_changed() # not needed, since setCurrentItem triggers callback

    def navigation_changed(self):
        """an item in the tree was selected, change panes"""
        current_selection = self.ui.treewidget_navigation.selectedItems()
        if not current_selection:
            return None
        self.curr_nav_label = name = str(current_selection[0].text(0))
        # Translate from short to long name
        for (long, short) in self.nav_labels:
            if name == short:
                name = long
                break

        name = '_'.join(name.lower().split(' '))

        matches = [i
                   for i in range(self.ui.stackedWidgetTaskPane.count())
                   if self.ui.stackedWidgetTaskPane.widget(i).objectName() == name]

        assert len(matches) == 1

        to_index = matches[0]

        self.animate_stacked_widget(
            self.ui.stackedWidgetTaskPane,
            self.ui.stackedWidgetTaskPane.currentIndex(),
            to_index)

        self.setup_current_pane()

    def setup_current_pane(self):
        # Force any open popup to close
        # (if dialog is modal we don't need this)
        self.species_popup.done(0)
        self.regions_popup.done(0)
        current_selection = self.ui.treewidget_navigation.selectedItems()

        if not current_selection:
            return
        text = str(current_selection[-1].text(0))

        # Translate from short to long name
        for (long, short) in self.nav_labels:
            if text == short:
                text = long
                break

        text = '_'.join(text.lower().split(' '))
        # Make sure panes are properly initialized as we change to them.  This way
        # we do not have to worry so much about inter-pane state dependencies

        setup_pane = { 'regions': self.ui.regions.setup_regions,
                       'model_setup': self.setup_model_setup,
                       'fluid': self.setup_fluid,
                       'solids': self.setup_solids,
                       'initial_conditions': self.setup_ics,
                       'boundary_conditions': self.setup_bcs,
                       'point_sources': self.setup_pss,
                       'internal_surfaces': self.setup_iss,
                       'chemistry': self.setup_chemistry,
                       'numerics': self.setup_numerics,
                       'output': self.setup_output,
                       'monitors': self.setup_monitors}.get(text)
        if setup_pane:
            setup_pane()

    # --- animation methods ---
    def animate_stacked_widget(self, stackedwidget, from_, to,
                               direction='vertical', line=None, to_btn=None,
                               btn_layout=None):
        """animate changing of qstackedwidget"""

        # check to see if already animating
        if self.animating and self.stack_animation is not None:
            self.stack_animation.stop()

        from_widget = stackedwidget.widget(from_)
        to_widget = stackedwidget.widget(to)

        # get from geometry
        width = from_widget.frameGeometry().width()
        height = from_widget.frameGeometry().height()

        # offset
        # bottom to top
        if direction == 'vertical' and from_ < to:
            offsetx = 0
            offsety = height

        # top to bottom
        elif direction == 'vertical' and from_ > to:
            offsetx = 0
            offsety = -height
        elif direction == 'horizontal' and from_ < to:
            offsetx = width
            offsety = 0
        elif direction == 'horizontal' and from_ > to:
            offsetx = -width
            offsety = 0
        else:
            return #?

        self.stack_animation = QtCore.QParallelAnimationGroup()

        if from_ != to:
            # move to widget and show
            # set the geometry of the next widget
            to_widget.setGeometry(0 + offsetx, 0 + offsety, width, height)
            to_widget.show()
            to_widget.raise_()
            #to_widget.activateWindow() ? needed?


            # animate
            # from widget
            self.animation_setup(from_widget, 0, 0, -offsetx, -offsety)

            # to widget
            self.animation_setup(to_widget, offsetx, offsety, 0, 0)

        # line
        line_to = None
        if line is not None and to_btn is not None:
            self.animation_setup(line, line.geometry().x(), line.geometry().y(),
                                 to_btn.geometry().x(), line.geometry().y(),
                                 to_btn.geometry().width())
            if btn_layout is not None:
                for i in range(0, btn_layout.columnCount()):
                    if btn_layout.itemAtPosition(0, i) == to_btn:
                        line_to = i
                        break

        # animation group
        # call back for when the animation is finsished/canceled.
        self.stack_animation.stateChanged.connect(lambda: self.animate_stacked_widget_finished(
            stackedwidget, from_, to, btn_layout, to_btn, line, line_to))

        self.animating = True
        self.stack_animation.start()

    def animation_setup(self, target, x_start, y_start, x_end, y_end, width_end=None):
        """setup an animation widget"""
        # TODO: this could probably be done better.
        animation = QtCore.QPropertyAnimation(target, "pos".encode('utf-8'))
        animation.setDuration(ANIMATION_SPEED)
        animation.setEasingCurve(QtCore.QEasingCurve.InOutQuint)
        animation.setStartValue(QtCore.QPoint(x_start, y_start))
        animation.setEndValue(QtCore.QPoint(x_end, y_end))
        self.stack_animation.addAnimation(animation)

        # resize line width
        if width_end is not None:
            animation = QtCore.QPropertyAnimation(target, "size".encode('utf-8'))
            animation.setDuration(ANIMATION_SPEED)
            animation.setEasingCurve(QtCore.QEasingCurve.InOutQuint)
            size = target.size()
            animation.setStartValue(size)
            animation.setEndValue(QtCore.QSize(width_end, size.height()))
            self.stack_animation.addAnimation(animation)

    def animate_stacked_widget_finished(self, widget, from_, to,
                                        btn_layout=None, to_btn=None, line=None, line_to=None):
        """cleanup after animation"""
        try:
            if self.stack_animation.state() == QtCore.QAbstractAnimation.Stopped:
                widget.setCurrentIndex(to)
                if from_ != to:
                    from_widget = widget.widget(from_)
                    from_widget.hide()
                    from_widget.move(0, 0) #why?
                if btn_layout is not None and line is not None:
                    btn_layout.addItem(btn_layout.takeAt(
                        btn_layout.indexOf(line)), 1, line_to or to)
        except AttributeError: # Happens during shutdown. TODO: remove this hack
            pass
        finally:
            self.animating = False

    # --- helper methods ---
    def message(self,
                title='Warning',
                icon='warning',
                text='',
                buttons=['ok'],
                default='ok',
                info_text=None,
                detailed_text=None,
                traceback_text=None,
                print_console=True):

        """Create and display a modal message box, also prints message in console
        title:  displayed in frame of message box
        text:  string, displayed in flowed format, variable width font, HTML
        icon:  'warning' or 'question', anything else is displayed as 'information'
        buttons: list of values, 'ok', 'yes', 'no', 'cancel','discard'
        default: 'ok' the default selected button
        info_text:  shown in message box as QtInformativeText (see QT docs)
        detailed_text:  shown in message box, requires "view details" button click
        traceback_text: string, displayed in monospace font, unflowed, no HTML

        Return the pressed button."""

        # Print messages in console
        if print_console:
            if traceback_text: # Avoid printing HTML tags in console window
                # TODO:  enable HTML for console?  Or format HTML to text?
                #self.print_internal(title, color='red')
                # This just prints 'Application error', so why bother?
                pass

            elif not any(title.lower().startswith(x)
                         for x in ('save', 'delete')):
                self.print_internal(title + ": " + text)

            for t in (info_text, detailed_text, traceback_text):
                if t:
                    self.print_internal(t)

        message_box = QtWidgets.QMessageBox(self)
        self.message_box = message_box # Make it accessible to tests
        message_box.setWindowTitle(title)
        message_box.setTextFormat(Qt.RichText) # support HTML

        # Icon
        if icon == 'warning':
            icon = QtWidgets.QMessageBox.Warning
        elif icon == 'question':
            icon = QtWidgets.QMessageBox.Question
        else:
            icon = QtWidgets.QMessageBox.Information

        message_box.setIcon(icon)

        # Text
        text = text.replace('\n', '<br>')# the joys of HTML
        if traceback_text:
            message_box.setText(text + "<pre>"+traceback_text+"</pre>")
        else:
            message_box.setText(text)

        if info_text:
            message_box.setInformativeText(info_text)

        if detailed_text:
            message_box.setDetailedText(detailed_text)

        qbuttonDict = {'ok':      QtWidgets.QMessageBox.Ok,
                       'yes':     QtWidgets.QMessageBox.Yes,
                       'no':      QtWidgets.QMessageBox.No,
                       'cancel':  QtWidgets.QMessageBox.Cancel,
                       'discard': QtWidgets.QMessageBox.Discard}

        for b in buttons:
            button = qbuttonDict.get(b)
            #if not button:
            #    button = QtWidgets.QPushButton(b)
            #    role = QtWidgets.QMessageBox.AcceptRole # Seems to be OK to use for all buttonsrole)
            #message_box.addButton(button, role)
            #    qbuttonDict[b] = button
            #else:
            message_box.addButton(button)

            if b == default:
                message_box.setDefaultButton(button)

        ret = message_box.exec_()

        for (k, v) in qbuttonDict.items():
            if v == ret:
                return k

    scan_errors_linebuf = []
    scan_errors_timer = False
    def scan_errors(self, lines):
        # Wait a little while for more lines, so we don't  make too many popups
        # But don't wait to set the mtime on a failed reinit (see comments in handle_reinit)
        if self.job_manager.job:
            reinit_fail = any(x in line.lower()
                              for x in ("reinit fail", "reinitialization fail")
                              for line in lines)
            if reinit_fail:
                self.job_manager.job.mtime = 0

        self.scan_errors_linebuf.extend(lines)
        if len(self.scan_errors_linebuf) > 20:
            self.scan_errors_inner()
        else: # Allow a more lines to come in
            if not self.scan_errors_timer:
                self.scan_errors_timer = True
                QtCore.QTimer.singleShot(100, self.scan_errors_inner)

    re_err_1000_bad_key = re.compile("Error 1000: A keyword pair on line (\d+)")
    re_err_1000_missing_key = re.compile("Error 1000: Required input not specified: (.*)")
    re_err_1001_illegal_key = re.compile("Error 1001: Illegal or unknown input: (.*)")
    re_err_2000_bad_line = re.compile("Error 2000: Unable to process line (\d+)")
    re_err_numbered = re.compile("Error [0-9]{4}: (.*)")

    def scan_errors_inner(self):
        # Check for reinit failure.  This is in place of full reporting of reinit status
        # (see comments in handle_reinit)
        self.scan_errors_timer = False
        if not self.scan_errors_linebuf:
            return

        lines = self.scan_errors_linebuf[:]
        if sys.version_info[0] == 2:
            del self.scan_errors_linebuf[:]
        else:
            self.scan_errors_linebuf.clear()
        reinit_fail = any(x in line.lower()
                          for x in ("reinit fail", "reinitialization fail")
                          for line in lines)
        if reinit_fail:
            msg = ['Reinitialization failed']
            for line in lines:
                line = line.strip()
                if any(line.lower().startswith(x)
                       for x in ("error", "warning")):
                    if ':' in line:
                        line = line.split(':',1)[1].strip()
                    msg.append(line)
            self.error('\n'.join(msg),
                           popup=True,
                           print_console=False)#error already in console from solver!
            return


        lineno = bad_line = None
        for (i, line) in enumerate(lines):
            for re_err in (self.re_err_1000_bad_key,
                           self.re_err_1001_illegal_key,
                           self.re_err_2000_bad_line):
                match = re_err.search(line)
                if match:
                    break
            if match:
                if re_err == self.re_err_1001_illegal_key: # No line number, we have to go find it
                    try:
                        text = match.group(1).lower()
                        if '=' in text:
                            text = text.split('=', 1)[0]
                        for (i, l) in enumerate(self.datfile_lines, 1):
                            if isinstance(l, type(text)):
                                if text in l.lower():
                                    bad_line = l
                                    lineno = i
                    except:
                        return
                else:
                    try:
                        lineno = int(match.group(1))
                        bad_line = self.datfile_lines[lineno-1]
                    except:
                        return
                break

        # TODO:  colorize errors in source view (red)
        # Note, we only report the first bad line, since there may be multiple errors
        if bad_line:
            try:
                key = bad_line.split("=", 1)[0].strip()
                self.report_keyword_error(key, bad_line)
            except:
                pass # Don't introduce additional errors in error handler
            return

        # Now check for missing required inputs.
        msg = []
        required_keys = []
        for (i, line) in enumerate(lines):
            match = self.re_err_1000_missing_key.search(line)
            if match:
                text = match.group(1)
                try:
                    key, args = parse_key_with_args(text)
                    key = key.lower()
                    # make sure it's still unset
                    if self.project.get_value(key, args=args) is not None:
                        continue
                    required_keys.append((key, args))
                except:
                    pass # avoid cascading errors
        if required_keys:
            msg = ['Required input%s not specified:' % ('s' if len(required_keys)>1 else '')]

            for (key, args) in required_keys:
                kw_doc = self.keyword_doc.get(key, {}).get('description', 'unknown')
                kw_doc = kw_doc.split('.', 1)[0] # Only keep first sentence
                kw_doc = kw_doc.split('\n',1)[0] # Remove existing formatting
                kw_doc = re.sub('\[.*\]', '', kw_doc) # Remove [units], [default]

                msg.append('<b>%s</b>: %s' % (format_key_with_args(key, args), kw_doc))

        # Other errors
        if msg:
            msg.append('') # Add blank line
        for line in lines:
            match = self.re_err_numbered.search(line)
            if match and not self.re_err_1000_missing_key.search(line): #Missing keys already done
                text = match.group(1)
                text = text.replace('<', '&lt;').replace('>', '&gt;')
                msg.append(text)

        if msg:
            self.error('\n'.join(msg),
                       popup=True,
                       print_console=False)#error already in console from solver!

    def report_keyword_error(self, key, line):
        """Give the user a chance to omit or edit deprecated keys"""
        #  This is a first implementation.  There's a lot more
        #  we could do here - suggest fixes, link to documentation,
        #  attempt to retain order in dat_file_list, etc.
        key = key.lower()
        line = line.strip()

        message_box = QtWidgets.QMessageBox(self)
        self.message_box = message_box
        message_box.setWindowTitle("Keyword error")
        message_box.setIcon(QtWidgets.QMessageBox.Warning)
        text = line
        message_box.setText(text)
        buttons = ['Drop Key', 'Edit', 'Ignore']
        for b in buttons:
            role = QtWidgets.QMessageBox.AcceptRole # Seems to be OK to use for all buttons
            message_box.addButton(QtWidgets.QPushButton(b), role)

        def drop(key):
            if '(' in key: # This is a little crude, use parser instead?
                k, a = key.split('(', 1)
                a = a.split(')')[0]
                a = [int(x) for x in a.split(',')]
            else:
                k, a = key, None

            self.unset_keyword(k, a)

        resp = buttons[message_box.exec_()].lower()
        if not resp or resp == 'ignore': # User bailed out
            return

        elif resp == 'drop key':
            drop(key)

        elif resp == 'edit':
            q = QtWidgets.QInputDialog(self)#,
            text, ok = q.getText(self, 'Edit keyword', "Keyword error: %s\n\nEnter replacement text"%line, text=line)
            if not ok:
                return
            text = text.strip()
            if not text:
                drop(key) #User replaced it with blank
            for (new_key, new_args, new_value) in self.project.parseKeywordLine(text):
                if new_key:
                    drop(key) # Only drop the old one once we have a valid replacemnent
                    self.update_keyword(new_key, new_value, args=new_args) # validate key?
                else:
                    self.print_internal("Error:  cannot parse %s" % text)

    def can_skip(self, line,
                 boilerplate=set(['Program Terminated.',
                                  'Fatal error reported on one or more processes. The .LOG file',
                                  'may contain additional information about the failure.',
                                  'ERROR STOP 1',
                 ])):

        # These are routine messages that we are not going to trouble the user with
        # Note, the set is only constructed once (at load time)
        # Also skip lines containing only '*'
        stripped = line.strip()
        lower = stripped.lower()

        return (lower=='' or
                lower.startswith('please correct') or
                lower.startswith('please update') or
                lower.startswith('please consult') or
                all(c == '*' or c=='_' for c in lower) or
                stripped in boilerplate)


    def handle_stdout(self, text):
        """collect stderr from mfix/pymfix process, format and display
        to user.  Note that some errors are printed to stdout"""
        lines = text.splitlines(True)
        # Scanning for errors may trigger popups, etc, so get the output to
        # the user first.
        for line in lines:
            if self.can_skip(line):
                continue
            advance = line.endswith(os.linesep)
            self.print_internal(line, font='Courier', advance=advance)

        self.scan_errors(lines)

    def handle_stderr(self, text):
        """collect stderr from mfix/pymfix process, format and display
        to user."""
        # Scanning for errors may trigger popups, etc, so get the output to
        # the user first.
        lines = text.splitlines(True)
        for line in lines:
            if self.can_skip(line):
                continue
            advance = line.endswith(os.linesep)
            self.print_internal(line, color='red', font='Courier', # Bold?
                                advance=advance)

        self.scan_errors(lines)

    highlight_error_re = re.compile('(error[: ])|(warning[: ])|fail', re.IGNORECASE)

    def print_internal(self, line, color=None, font=None, advance=True):
        # avoid printing HTML tags
        line = line.replace('&gt;', '>')
        line = line.replace('&lt;', '<')
        line = line.replace('<br>', os.linesep)

        qtextbrowser = self.ui.command_output
        stripped = line.strip()
        if not stripped:
            # Let's just skip blank lines completely
            return
        lower = line.lower()
        logmsg = stripped
        # hack. TODO: real msg types, map to font/color
        strikeout = font and font.lower() == 'strikeout'
        if strikeout:
            logmsg = "unset " + logmsg
        if lower.startswith("error:"):
            log.error(logmsg[6:])
        elif lower.startswith("warning:"):
            log.warning(logmsg[8:])
        elif lower.startswith("info:"):
            logmsg = logmsg[5:].strip()
            line = line[5:].strip() # Supress 'Info:' in console window (?)
            log.info(logmsg)
            color = 'blue'
        else:
            log.info(logmsg)

        cursor = qtextbrowser.textCursor()
        cursor.movePosition(cursor.End)
        char_format = QtGui.QTextCharFormat()

        # HACK is this going too far?  we should define message types, not infer from messages
        if self.highlight_error_re.search(line):
            color = 'red'

        if color:
            char_format.setForeground(QtGui.QColor(color))

        if font:
            if strikeout: # hack
                char_format.setFontFamily("Monospace")
                char_format.setFontStrikeOut(True)
            else:
                char_format.setFontFamily(font)

        #char_format.setFontFixedPitch(True) # ?
        cursor.setCharFormat(char_format)
        scrollbar = qtextbrowser.verticalScrollBar()
        scrolled_to_end = (scrollbar.value() == scrollbar.maximum())

        if advance and not line.endswith(os.linesep):
            line += os.linesep

        cursor.insertText(line)

        if scrolled_to_end:
            scrollbar.setValue(scrollbar.maximum())

    def remove_output_files(self, output_files=None, message_text=None, force_remove=False):
        """ remove MFIX output files from current project directory

        output_files: List of patterns to be matched for file removal
        message_text: Text to be displayed on the popup
        force_remove: Don't allow user to uncheck categories
        return: True for success, False for user cancel"""

        if not output_files:
            output_files = self.get_output_files()

        if message_text is None:
            message_text = 'Reset the project by deleting the following files.'

        ok_to_delete = self.output_selection_popup.exec_(
            output_files, force_remove, message_text)

        if not ok_to_delete:
            return False

        # stop the monitor_reader threads
        self.monitor_reader.stop_threads()

        for path in self.output_selection_popup.get_output_files():
            try:
                os.remove(path)
            except OSError as err:
                msg = 'Error: cannot delete %s: %s' % (path, err.strerror)
                self.message(title='File error',
                             icon='error',
                             text=msg,
                             buttons=['ok'],
                             default=['ok'])
                break

        self.slot_update_runbuttons()

        # resume the monitor_reader threads
        self.monitor_reader.resume_threads()

        return True

    def get_output_files(self, patterns=None):
        """ get the output files of an MFIX solver job """
        project_dir = self.get_project_dir()
        if not project_dir:
            return
        if not patterns:
            patterns = RESTART_FILES + SPX_FILES + VTK_FILES + MONITOR_FILES + OTHER_FILES

        vtu_dir = self.project.get_value('vtu_dir', default=None)

        outputs = []
        for p in patterns:
            pi = case_insensitive(p)
            outputs.extend(glob.glob(os.path.join(project_dir, pi)))
            if vtu_dir not in (None, '', '.'):
                outputs.extend(glob.glob(os.path.join(project_dir, case_insensitive(vtu_dir), pi)))

        return outputs

    def get_res_files(self):
        return self.get_output_files(patterns=['*.res'])

    def handle_run(self):
        self.ui.responses.setText("")  # Clear old messages
        paused = self.job_manager.is_job_paused()
        word = 'resuming' if paused else 'starting'

        if paused:
            if self.unsaved_flag:
                self.confirm_save(title="Save?",
                                  text="Must save project before %s run. Save?" % word,
                                  autostart=True)
                # Do not need to unpause with autostart.  Either user canceled save,
                # or job started ...
                return
            elif self.mtime > self.job_manager.job.mtime:
                self.handle_reinit(autostart=True)
                return
            else:
                self.print_internal("Unpausing...", color='blue')
                self.job_manager.job.unpause()
        else:
            if not self.confirm_save(title="Save?",
                                     text="Must save project before %s run. Save?" % word):
                return
            # Job is started from run dialog
            self.open_run_dialog()

        self.slot_update_runbuttons()


    def handle_pause(self):
        self.print_internal("Pausing MFiX...", color='blue')
        #self.set_stop_button(enabled=False) # Leave enabled so we can force-kill
        self.job_manager.pause_job()
        self.slot_update_status() # force status update
        self.update_window_title()


    def handle_reinit(self, autostart=False):
        # The "job is not None" checks should be delegated to the
        #  job_manager
        warning = None
        if not self.job_manager.job:
            warning = "No job"
        elif not self.job_manager.job.is_paused():
            warning = "Job is not paused"
        elif self.job_manager.is_job_pending():
            warning = "Job is pending"
        if warning:
            self.warning("reinit: " + warning)
            return

        # issues/414 filter out keyword DT
        #  FIXME, there are a bunch of "locked"/"unlocked" keys, we should
        #  treat them all the same, not just DT
        # TODO use 'locked/unlocked' attributes of keyword
        dt_pat = re.compile(r'\s*dt\s*=', re.IGNORECASE)
        def ok_for_reinit(line):  # No need to send comments, blank lines, etc
            line = line.strip()
            return not (
                line == '' or
                line.startswith('!') or
                line.startswith('#') or
                dt_pat.match(line))

        mfix_dat = ''.join(
            filter(ok_for_reinit, self.project.to_string()))
        # TODO strip trailing comments (being careful of quotes)
        self.job_manager.job.reinit(mfix_dat, autostart=autostart)
        # TODO FIXME better success/failure reporting for reinit!
        # Since everything is async (both HTTP, and the reinit_flag handling in
        #  the solver), this requires some work
        # For now, assume that the reinit succeeded, and if we get a
        #  reinit failed message, we reset the job's mtime
        # Setting job.mtime=self.mtime on 'reinit succeeded' would not work b/c
        #  self.mtime might have advanced before that message came back
        # It would be good to include the mtime in the payload, that's the only
        #  way to track it reliably
        self.job_manager.job.mtime = self.mtime #XXX should only do this for successful reinit!

    def handle_stop(self):
        self.print_internal("STOP", color='red')
        self.set_pause_button(enabled=False)
        self.job_manager.stop_mfix()


    def handle_force_stop(self, *args):
        self.print_internal("FORCE STOP", color='red')
        self.set_pause_button(enabled=False)
        self.job_manager.force_stop_mfix()


    def confirm_save(self, title="Save?", text="Save project?", autostart=False):
        if self.unsaved_flag:
            response = self.message(title=title,
                                    icon="question",
                                    text=text,
                                    buttons=['ok', 'cancel'])
            if response != 'ok':
                return False
            self.handle_save(autostart=autostart)
        return True

    def open_run_dialog(self):
        project_dir = self.get_project_dir()
        udfs = glob.glob(os.path.join(project_dir, "*.f"))
        mfixsolver = glob.glob(os.path.join(project_dir, "mfixsolver*"))
        if udfs and not mfixsolver:
            udf_msg = ("Warning: Fortran source files exist for this project,"
                       " but no project-specific mfixsolver was found. This"
                       " case probably won't run correctly unless mfixsolver"
                       " is (re)built. See the user manual for instructions on"
                       " building mfixsolver. Proceed anyway?")

            response = self.message(title='Warning',
                                    icon='question',
                                    text=udf_msg,
                                    buttons=['yes', 'no'],
                                    default='no')
            if response != 'yes':
                return

        self.run_dialog = RunPopup(self.commandline_option_exe, self)
        self.run_dialog.setModal(True)
        self.run_dialog.popup()

    def export_project(self):
        """Copy project files to new directory, but do not switch to new project"""
        self.confirm_save(text='Save project before exporting?')
        project_file = self.get_project_file()
        if not project_file:
            self.message(text="Nothing to export",
                         buttons=['ok'])
            return

        export_file = self.get_save_filename()
        if not export_file: # User bailed out
            return
        export_dir = os.path.dirname(export_file)
        if not self.check_writable(export_dir):
            return

        # new project name
        new_project_file = os.path.splitext(os.path.basename(export_file))[0]

        # Write the project file
        exported_project = copy.deepcopy(self.project)
        exported_project.updateKeyword('run_name', new_project_file)
        self.print_internal("Info: Exporting %s" % new_project_file)
        # FIXME: Note calling writeDatFile instead of gui.save_project skips a lot...
        #  regions, BC, etc.  Better to just copy the .mfx file.
        exported_project.writeDatFile(os.path.join(export_dir, new_project_file+'.mfx'))

        # collect files to copy
        files_to_copy = []
        # output files *.RES, *.SP?, *.vtu, etc...
        output_files = self.get_output_files()
        ok_to_copy = self.output_selection_popup.exec_(output_files, heading='Copy the following files?', title='Copy files?')
        if not ok_to_copy: # user bailed out
            return
        files_to_copy.extend(self.output_selection_popup.get_output_files())

        # stl files
        files_to_copy.extend(self.get_output_files(["*.stl"]))

        #copy project files into new_project_directory
        for f in files_to_copy:
            try:
                shutil.copyfile(f, os.path.join(export_dir, os.path.basename(f)))
            except Exception as e:
                self.message(title='File error',
                             text="Error copying file:\n%s" % e,
                             buttons=['ok'])

        self.handle_file_menu_hide()

    def navigate_all(self):
        """iterate over all navigation panes, selecting each
        row of each table, to make sure associated keywords
        are all set"""
        tw = self.ui.treewidget_navigation
        r = tw.invisibleRootItem()
        for i in range(r.childCount()):
            c = r.child(i)
            gui.change_pane(c.text(0))
            # we should cycle through all the subtabs, too
            if gui.project.solver == 'DEM':
                gui.setup_solids_dem_tab()
            elif gui.project.solver == 'TFM':
                gui.setup_solids_tfm_tab()
            elif gui.project.solver == 'PIC':
                gui.setup_solids_pic_tab()

        for t in (0,2,4,5):
            gui.setup_bcs_tab(t)

        for w in widget_iter(self):
            if isinstance(w, QtWidgets.QTableWidget):
                for r in range(0, w.rowCount()):
                    w.setCurrentCell(r, 0)

    def save_project(self, filename=None):
        """save project, optionally as a new project.

        project_file: string, full filename of project (including path)
        return: None"""

        if filename:
            project_dir = os.path.dirname(filename)
            project_file = filename
        else:
            project_dir = self.get_project_dir()
            project_file = self.get_project_file()

        if project_dir is None or project_file is None:
            return

        # save version
        v = self.project.mfix_gui_comments.get('project_version', 0)
        self.project.mfix_gui_comments['project_version'] = str(int(v) + 1)
        self.project.mfix_gui_comments['gui_version'] = __version__

        self.project.mfix_gui_comments['project_notes'] = json.dumps(self.ui.file_menu_info_widget.project_notes.toPlainText())

        # save users
        users = self.project.mfix_gui_comments.get('modified_by', [])
        if not isinstance(users, list):
            users = users.split('|')

        users.append(get_username())
        users = set(users)
        self.project.mfix_gui_comments['modified_by'] = '|'.join(users)

        # save geometry
        self.vtkwidget.export_stl(os.path.join(project_dir, 'geometry.stl'))
        self.project.mfix_gui_comments['geometry'] = self.vtkwidget.geometry_to_str()
        self.project.mfix_gui_comments['visual_props'] = self.vtkwidget.visual_props_to_str()

        # save regions
        self.project.mfix_gui_comments['regions_dict'] = self.ui.regions.regions_to_str()

        for (data, key) in ((self.bc_regions_to_str(), 'bc_regions'),
                            (self.ic_regions_to_str(), 'ic_regions'),
                            (self.is_regions_to_str(), 'is_regions'),
                            (self.ps_regions_to_str(), 'ps_regions'),
                            (self.vtk_regions_to_str(), 'vtk_regions'),
                            (self.monitor_regions_to_str(), 'monitor_regions'),
                            (self.chemistry_to_str(), 'chemistry'),
                            (self.graphics_to_str(), 'graphics')):
            if data:
                self.project.mfix_gui_comments[key] = data
            else:
                self.project.mfix_gui_comments.pop(key, None)

        self.create_project_thumbnail()

        # Trim unused species from THERMO_DATA
        for key in list(self.project.thermo_data.keys()):
            if not (self.chemistry_check_species_in_use(key)
                    or key in self.fluid_species
                    or any(key in species_per_phase
                           for species_per_phase in self.solids_species.values())):
                del self.project.thermo_data[key]

        project_base = os.path.basename(project_file)
        self.print_internal("Info: Saving %s\n" % project_file)
        self.project.writeDatFile(project_file)
        mtime = os.path.getmtime(project_file)
        dt = datetime.datetime.fromtimestamp(mtime)
        self.project.modified_time = datetime.datetime.strftime(dt, '%Y-%m-%d %H:%M')

        # save nodeworks
        if self.ui.nodeworks_widget.NODEWORKS_AVAILABLE:
            self.ui.nodeworks_widget.save(
                os.path.abspath(os.path.join(project_dir, 'workflow.nc')))

        self.clear_unsaved_flag()
        self.update_source_view()
        self.save_recent_project_list()

    def save_as(self):
        """Prompt user for new filename, save project to that file and make
        it the active project"""

        ### TODO what if there is a paused job?

        new_file = self.get_save_filename()
        if not new_file:
            return
        new_dir = os.path.dirname(new_file)
        if not self.check_writable(new_dir):
            return

        ok_to_write = self.check_writable(new_dir)
        if not ok_to_write:
            return

        if os.path.exists(new_file) and not self.confirm_clobber(new_file):
            return

        # Force run name to file name.  Is this a good idea?
        basename = os.path.basename(new_file)
        run_name = os.path.splitext(basename)[0].replace(' ', '_')
        # See re_valid_run_name_qt
        # Don't allow '-' at start
        while run_name and run_name.startswith('-'):
            run_name = run_name[1:]
        banned = r"""!#$&*?/<>{}[]()|~`'":;\\\n\t """
        for c in banned:
            run_name = run_name.replace(c, '_')

        self.set_project_file(new_file)
        self.update_keyword('run_name', run_name)
        self.save_project()

        # change file watcher
        self.slot_rundir_timer()
        self.signal_update_runbuttons.emit('')
        self.handle_file_menu_hide()

    def get_save_filename(self, message=None):
        """wrapper for call to getSaveFileName, override in unit tests"""
        if message is None:
            message = 'Save Project As'

        run_name = self.project.get_value('run_name', default='new_file')
        default = os.path.join(self.get_project_dir(), run_name+'.mfx')
        # TODO set a validator on file name?  setDefaultSuffix?
        filename, ignore = QtWidgets.QFileDialog.getSaveFileName(self, message, default, "*.mfx")
        if filename and '.' not in filename: # Force .mfx suffix?
            filename += '.mfx'
        return filename

    def handle_save(self, autostart=False):
        # Save project, also update/reinit any paused job
        project_file = self.get_project_file()
        try:
            # TODO:  save backup copy, revert if reinit failed
            self.save_project()
            if self.job_manager.is_job_paused():
                if self.mtime > self.job_manager.job.mtime:
                    self.print_internal("Updating paused job", color='blue')
                    self.handle_reinit(autostart=autostart)

        except Exception as e:
            msg = 'Failed to save %s: %s: %s' % (project_file, e.__class__.__name__, e)
            self.print_internal("Error: %s" % msg, color='red')
            self.message(title='Error',
                         icon='error',
                         text=msg,
                         buttons=['ok'],
                         default='ok')
            traceback.print_exception(*sys.exc_info())
            return
        self.handle_file_menu_hide()

    def handle_export(self):
        self.export_project()

    def handle_save_as(self):
        self.save_as()

    def handle_parameters(self):
        """add/change parameters"""
        changed_params = self.parameter_dialog.get_parameters()
        if changed_params:
            self.set_unsaved_flag()
        self.update_parameters(changed_params)

    def update_parameters(self, changed_params):
        """update the changed parameters"""
        self.ui.regions.update_parameters(changed_params)
        self.vtkwidget.update_parameters(changed_params, render=self.open_succeeded)
        self.project.update_parameters(changed_params)

    def handle_compile(self):
        """compiling tool"""
        self.ui.toolbutton_compile.setEnabled(False)
        popup = make_build_popup(self, self.get_project_dir())
        popup.finished.connect(self.handle_compile_finished)
        popup.show()

    def handle_compile_finished(self):
        self.ui.toolbutton_compile.setEnabled(True)

    def update_window_title(self):
        title = self.solver_name or 'MFiX'
        project_file = self.get_project_file()
        if project_file:
            # add entire path to title, abbreviate user dir
            title += " - " + project_file.replace(os.path.expanduser('~'), '~')
            if self.unsaved_flag:
                title += '*'

            if self.job_manager.job:
                if not self.job_manager.job.is_paused():
                    if self.job_manager.stopping:
                        title += ', STOPPING'
                    elif self.job_manager.pausing:
                        title += ', PAUSING'
                    else:
                        title += ', RUNNING'
                    if self.job_manager.job.job_id is not None:
                        title += ', job %s'% self.job_manager.job.job_id
                    elif self.job_manager.job.mfix_pid is not None:
                        title += ', process %s'% self.job_manager.job.mfix_pid

                elif self.job_manager.job.is_paused():
                    title += ', PAUSED'
                elif self.get_res_files():
                    title += ', STOPPED, resumable' # Do we ever see this?

        self.setWindowTitle(title)

    def set_save_button(self, enabled):
        self.ui.toolbutton_save.setEnabled(enabled)
        # set the one in the file menu too!
        self.disable_file_menu_items([] if enabled else ['save'])

    def set_unsaved_flag(self):
        self.mtime = time.time()
        if not self.unsaved_flag:
            # For debugging problems where flag gets set during load
            #import traceback
            #traceback.print_stack()
            log.info("Project is not saved")

        self.unsaved_flag = True
        self.update_window_title()
        self.set_save_button(enabled=True)


    def clear_unsaved_flag(self):
        if self.unsaved_flag:
            log.info("Project is saved")

        self.unsaved_flag = False
        self.set_save_button(enabled=False)
        # reinit support
        self.slot_update_runbuttons()

    def check_writable(self, directory):
        """check whether directory is writable """
        try:
            testfile = tempfile.TemporaryFile(dir=directory)
            testfile.close()
            return True

        except Exception as e:
            self.message(
                title='Warning',
                icon='warning',
                text="The directory %s is not writable" % directory,
                buttons=['ok'],
                default='ok')

            return False

    def get_new_project_info(self, filename):
        """Queries user for with NewProjectDialog,
        returns project directory and run_name """

        run_name = self.get_run_name_from_file(filename)
        ok, run_name, project_loc = NewProjectDialog(self).get(run_name=run_name)
        run_name = run_name.strip().replace(' ', '_')
        if not all([ok, run_name, project_loc, self.check_writable(project_loc)]):
            return None

        project_dir = os.path.join(project_loc, run_name)
        if os.path.exists(project_dir):
            if not self.confirm_clobber(project_dir):
                return None
        else:
            os.makedirs(project_dir)

        return project_dir, run_name

    def open_new_from_template(self, template):
        """Copy files from template directory to user-specified location,
        set run_name and modify comments,
        then open the new project"""

        info = self.get_new_project_info(template)
        if info is None:
            return
        project_dir, run_name = info

        extra_files = [extra_file for pattern in ["*.msh", "*.f", "*.stl", "*.dat", "*.inc"]
                       for extra_file in glob.glob(os.path.join(os.path.dirname(template), pattern))
                       if "mfix.dat" not in extra_file]

        project_file = os.path.join(project_dir, run_name + ".mfx")

        # Start from template
        creator = get_username()
        creation_time = time.strftime('%Y-%m-%d %H:%M')
        try:
            with open(template, encoding='utf-8', errors='replace') as infile:
                with open(project_file, 'w', encoding='utf-8', errors='replace') as outfile:
                    for line in infile:
                        if line.startswith('# Generic'):
                            # Put something better in the comment field, otherwise
                            # "generic template" lingers there forever
                            line = '# Created by %s on %s\n' % (creator, creation_time)

                        outfile.write(line)

            for extra_file in extra_files:
                shutil.copyfile(extra_file, os.path.join(project_dir, os.path.basename(extra_file)))

        except Exception as e:
            self.message(text="Error %s creating new project" % e,
                         buttons=['ok'],
                         default=['ok'])
            self.set_no_project()
            return

        self.open_project(project_file, run_name)

        # add some info
        self.project.mfix_gui_comments['author'] = creator
        self.project.mfix_gui_comments['created_date'] = creation_time
        self.update_keyword('run_name', run_name)
        self.save_project()

    def get_open_filename(self):
        """wrapper for call to getOpenFileName, override in for unit tests"""
        project_dir = self.get_project_dir()
        filename, ignore = QtWidgets.QFileDialog.getOpenFileName(
            self, 'Open Project Directory', project_dir,
            'MFiX Project (*.mfx *.dat);; All Files (*)')
        return filename

    def handle_open(self):
        """handler for toolbar Open button"""
        if self.unsaved_flag:
            confirm = self.message(text="Project not saved\nData will be lost!\nProceed?",
                                   buttons=['yes', 'no'],
                                   default='no')
            if confirm != 'yes':
                return
            self.clear_unsaved_flag()

        project_path = self.get_open_filename()
        if not project_path:
            return # user pressed Cancel
        self.open_project(project_path)

    def update_source_view(self, number_lines=True):
        project_file = self.get_project_file()
        if not project_file:
            src = ''
        else:
            try:
                with open(project_file, encoding='utf-8', errors='replace') as f:
                    src = f.read()
            except Exception as e:
                log.error("error opening %s: %s" % (project_file, e))
                src = ''

        self.datfile_lines = src.split(os.linesep)

        if number_lines:
            lines = src.split(os.linesep)
            # Avoid extra blank lines at end
            while lines and lines[-1] == '':
                lines.pop(-1)

            src = os.linesep.join('%4d:%s'%(lineno, line)
                                  for (lineno, line) in enumerate(lines, 1))

        self.ui.mfix_dat_source.setPlainText(src)

    def force_default_settings(self):
        # Should these just be in the template file? TODO
        self.update_keyword('chk_batchq_end', True)


    def get_run_name_from_file(self, filename):
        """Get run name from file without loading it.
        For a loaded project, use get_value('run_name')"""

        run_name = ''
        for line in open(filename, encoding='utf-8', errors='replace'):
            line = line.strip()
            if line.lower().startswith('run_name') and '=' in line:
                tok = line.split('=', 1)[1].strip()
                # Remove quotes if present.
                # NB: Due to a bug, in some files, run_name may lack quotes
                if tok.startswith('"') or tok.startswith("'"):
                    tok = tok[1:]
                if tok.endswith('"') or tok.endswith("'"):
                    tok = tok[:-1]
                run_name = tok.strip().replace(' ', '_')
                break
        return run_name


    def save_recent_project_list(self):
        rec_projects = self.settings.value('recent_projects')
        if rec_projects is None:
            rec_projects = []
        elif isinstance(rec_projects, str):
            rec_projects = rec_projects.split('|')

        # remove deleted projects
        clean_proj = []
        for proj in rec_projects:
            if os.path.exists(proj):
                clean_proj.append(proj)

        proj = self.get_project_file()
        if proj in clean_proj:
            clean_proj.remove(proj)

        new_rec_projects = ([proj] + clean_proj)[:MAX_RECENT_PROJECTS]
        self.settings.setValue('recent_projects', '|'.join(new_rec_projects))

    def open_project(self, project_path, run_name=None, interactive=True):
        """Open MFiX project"""
        # Too much going on in this method, maybe split some of this out

        if self.file_menu.isVisible():
            self.handle_file_menu_hide()

        #self.change_pane('model setup')

        # see also project_manager.load_project_file

        # Make sure path is absolute
        if not os.path.isabs(project_path):
            project_path = os.path.abspath(project_path)

        # "path" may be a directory or a file
        if os.path.isdir(project_path):
            project_dir = project_path
            project_file = os.path.abspath(os.path.join(project_path, 'mfix.dat'))
        else:
            project_dir = os.path.dirname(project_path)
            project_file = project_path

        if not os.path.exists(project_file):
            self.message(title='Error',
                         icon='error',
                         text=('%s does not exist' % project_file),
                         buttons=['ok'],
                         default='ok')
            self.set_no_project()
            return

        os.chdir(project_dir) # Make project dir CWD to match solver

        # --- read the mfix.dat or *.mfx file

        self.reset() # resets gui, keywords, file system watchers, etc

        basename, pathname = os.path.split(project_file)
        self.print_internal("Loading %s from %s" % (pathname, basename), color='blue')
        try:
            (units_converted, excs, ws) = self.project.load_project_file(project_file)
        except Exception as e:
            msg = 'Failed to load %s: %s: %s' % (os.path.basename(project_file),
                                                 e.__class__.__name__, e)
            self.error(msg, popup=interactive) # don't popup in -t mode

            traceback.print_exception(*sys.exc_info())
            # Should we stick this in the output window?  no, for now.

            self.set_no_project()
            return

        run_name = self.get_run_name_from_file(project_file)
        pidfile = os.path.join(project_dir, run_name +'.pid') if run_name else None
        self.do_open(project_file, pidfile)
        # report any errors
        for (prefix, errlist) in (('Error', excs), ('Warning', ws)):
            for e in errlist:
                if hasattr(e, 'message'):
                    msg = e.message
                else:
                    msg = e
                self.print_internal("%s: %s" % (prefix, msg),
                                        color='red') # Different color for err/warn?

        if excs: # Errors occurred
            msg = plural(len(excs), 'error')
            self.print_internal("Warning: %s loading %s" %
                                    (msg , project_file),
                                    color='red')
        else:
            if ws: # No errors, but some warnings
                msg = plural(len(ws), 'warning')
                self.print_internal("Loaded %s with %s" %
                                        (os.path.basename(project_file), msg),
                                        color='red')
            else: # Success
                self.print_internal("Loaded %s" % os.path.basename(project_file),
                                        color='blue')


        if units_converted:
            msg = ["Project has been converted to SI.", "Please check result of conversion."]
            udfs = glob.glob(os.path.join(os.path.dirname(project_file), '*.f'))
            if udfs:
                msg += ["", "The following Fortran source files have not been converted:"]
                msg += ['  '+os.path.basename(x) for x in udfs]
                msg = '\n'.join(msg)
            self.warn(msg, popup=True)


    def do_open(self, project_file, pidfile):
        project_dir = os.path.dirname(project_file)
        self.set_project_file(project_file) # set project file early

        # make sure the file_menu is closed
        self.handle_file_menu_hide()

        # previously started job may be running, try to reconnect
        self.job_manager.try_to_connect(pidfile)

        self.open_succeeded = False  # set to true on success
        self.vtkwidget.defer_render = True # defer rendering vtk until load finished

        self.setup_current_pane() # update vals in any open tabs
        self.update_source_view()

        ### Geometry
        # Look for geometry.stl and load automatically
        geometry_file = os.path.abspath(os.path.join(project_dir, 'geometry.stl'))
        if os.path.exists(geometry_file) and 'geometry' not in self.project.mfix_gui_comments:
            self.vtkwidget.add_stl(None, filename=geometry_file)
        else:
            # order needs to be visual_props -> geometry -> regions (loaded below)
            # load props first
            props = self.project.mfix_gui_comments.get('visual_props')
            if props:
                self.vtkwidget.visual_props_from_str(props)

            geo = self.project.mfix_gui_comments.get('geometry')

            if geo:
                self.vtkwidget.geometry_from_str(geo)
            else:
                # extract quadrics
                self.vtkwidget.process_quadrics(self.project)
                # read other geometry?

        #  Non-keyword params stored as !#MFIX-GUI comments
        solids_phase_names = {}
        try:
            for (key, val) in self.project.mfix_gui_comments.items():
                if key == 'fluid_phase_name':
                    self.set_fluid_phase_name(val)
                elif key.startswith('solids_phase_name('):
                    n = int(key.split('(')[1][:-1])
                    solids_phase_names[n] = val
                elif key == 'regions_dict':
                    self.ui.regions.regions_from_str(val)
                elif key == 'ic_regions':
                    self.ics_regions_from_str(val)
                elif key == 'bc_regions':
                    self.bcs_regions_from_str(val)
                elif key == 'is_regions':
                    self.iss_regions_from_str(val)
                elif key == 'ps_regions':
                    self.pss_regions_from_str(val)
                elif key == 'vtk_regions':
                    self.vtk_regions_from_str(val)
                elif key == 'monitor_regions':
                    self.monitors_regions_from_str(val)
                elif key == 'chemistry':
                    self.chemistry_from_str(val)
                elif key == 'graphics':
                    self.graphics_from_str(val)
                elif key == 'geometry':
                    pass # handled in 'geometry' section above
                elif key == 'visual_props':
                    pass # handled in 'geometry' section above
                elif key == 'parameters':
                    pass # handled in project
                # Add more here
                #else:  # Too many warnings!
                #    self.warn("Unknown mfix-gui setting '%s'" % key)

        except Exception as e:
            self.error("%s: %s" % (key, e))

        # Copy ordered dict to modify keys w/o losing order
        if solids_phase_names:
            s = OrderedDict()
            for (i, (k, v)) in enumerate(self.solids.items(), 1):
                s[solids_phase_names.get(i, k)] = v

            self.solids = s

        #### Fluid phase
        # TODO move this stuff to setup_fluid.
        self.fluid_solver_disabled = (self.project.get_value('ro_g0') == 0.0)
        self.disable_fluid_solver(self.fluid_solver_disabled)
        self.update_fluid_species_table()

        # fluid momentum and species eq. handled by _keyword_ widget

        # Scalar equations
        pass

        #Move to fluid_handler!
        # handle a bunch of items which are essentially the same
        for (setter, name) in ((self.set_fluid_density_model, 'ro'),
                               (self.set_fluid_viscosity_model, 'mu'),
                               (self.set_fluid_specific_heat_model, 'c_p'), # inconsistent
                               (self.set_fluid_conductivity_model, 'k'),
                               (self.set_fluid_diffusion_model, 'dif')):
            name_g0 = 'c_pg0' if name=='c_p' else name+'_g0'
            name_usr = 'usr_cpg' if name=='c_p' else 'usr_'+name+'g'
            val_g0 = self.project.get_value(name_g0)
            val_usr = self.project.get_value(name_usr)

            if val_usr is not None and val_g0 is not None:
                self.print_internal('Warning: %s and %s are both set' % (name_g0, name_usr))
                # this is getting printed after error count ... should be included in # of errs

            # XXX FIXME conflicts with default fluid models (?)
            setter(CONSTANT if val_g0 is not None
                   else UDF if val_usr is not None
                   else 1)

        # molecular weight model is the odd one (only 2 settings)
        if self.project.get_value('mw_avg') is not None:
            self.set_fluid_mol_weight_model(CONSTANT)
        else:
            # requires molecular weights for all species components, when should we validate?
            self.set_fluid_mol_weight_model(1)


        ### Solids
        # Needed?  will this get done when we switch to solids tab?
        self.update_solids_table()
        self.solids_update_tabs()
        self.update_solids_detail_pane()

        ### Regions
        # Look for regions in IC, BC, PS, etc.
        self.ui.regions.extract_regions(self.project, project_dir)

        # background mesh
        self.init_background_mesh()

        # "Extract" pulls out info from non-GUI project files, which don't have MFIX_GUI section
        # Initial conditions
        self.ics_extract_regions()
        # Boundary conditions
        self.bcs_extract_regions()
        # Point sources
        self.pss_extract_regions()
        # Internal surfaces
        self.iss_extract_regions()
        # VTK output regions
        self.vtk_extract_regions()
        # Monitors
        self.monitors_extract_regions()
        # Chemistry
        self.chemistry_extract_info()

        # Scalars
        #self.scalar_extract_info() Nothing to do

        # monitor reader
        for mon in self.project.get_key_indices('monitor_name'):
            name = self.project.get_value('monitor_name', args=mon)
            if name is not None:
                self.monitor_reader.add_file(os.path.join(project_dir, name))

        ### Nodeworks
        if self.ui.nodeworks_widget.NODEWORKS_AVAILABLE:
            nodeworks_file = os.path.abspath(os.path.join(project_dir, 'workflow.nc'))
            if os.path.exists(nodeworks_file):
                self.ui.nodeworks_widget.clear()
                self.ui.nodeworks_widget.load(nodeworks_file)

            self.ui.nodeworks_widget.look_for_projects(project_dir)

        self.vtkwidget.reset_view()
        self.vtkwidget.clear_offscreen_render()
        self.vtkwidget.render(defer_render=False)
        self.ui.tabWidgetGraphics.setCurrentIndex(0)
        self.open_succeeded = True
        self.slot_update_runbuttons()
        self.update_nav_tree()
        self.ui.toolbutton_compile.setEnabled(True)

        #if self.unsaved_flag: #
        # Settings changed during loading
        #    self.save_project()- let the user do this

        self.save_recent_project_list()
        self.setup_current_pane()

        #### ***** PUBLIC SERVICE ANNOUNCEMENT
        ##PLEASE do not call 'clear_unsaved_flag' here
        #  if it got set during file-load, either there's
        #  a bug (erroneous call to set_unsaved_flag)
        #  or a keyword got modified during loading (this
        #  happens!).  Clearing the flag is unsafe
        #  because there may be real changes that need
        #  to be saved


    def add_tooltip(self, widget, key, description=None, value=None):
        # Why not just use widget.key ?
        if description is None:
            doc = self.keyword_doc.get(key)
            if not doc:
                return
            description = doc.get('description')
            if description is None:
                return
            if value is not None and 'valids' in doc or key=='rdf_type':
                vkey = ('.FALSE.' if value is False
                        else '.TRUE.' if value is True
                        else str(value))
                for (k, v) in doc['valids'].items():
                    if k==vkey or (vkey and v.get('alias')==vkey):
                        description = v.get('note', str(value))
                        break
        # Clean it up a little
        description = description.strip()
        # Remove default values in brackets (these don't make sense
        #  once the value has been set)
        if description.endswith(']') or description.endswith('].'):
            dot = description.endswith('.')
            description = description.rsplit('[', 1)[0]
            description = description.strip()
            if dot and not description.endswith('.'): # Put it back
                description += '.'
        # '>' and '<' will mess up HTML
        description = description.replace('<', '&lt;')
        description = description.replace('>', '&gt;')

        ### epsilon
        description = description.replace('epsilon', 'ε')

        # Don't split diff. eq's over multiple lines
        pat = re.compile('boundary condition: *([^,]*, )')
        match = pat.search(description)
        if match:
            text = match.group(1)
            description = description.replace(text, '<br/>%s<br/>'%text[:-2].replace(' ', '&nbsp;'))

        # Default
        #pat = re.compile(r'\[[^]]+\]')
        #while True:
        #    match = pat.search(description)
        #    if not match:
        #        break
        #    text = match.group(0)
        #    description = description.replace(text, '<i>Default: %s</i>'%text[1:-1])

        for phrase in ('Variable Index List', ):
            if phrase in description:
                description = description.replace(phrase, '<br/>'+phrase)

        # Bullets
        description = re.sub(r'^\s*-', '<br/>&bull;', description, flags=re.MULTILINE)

        # Issues/590 (workaround)
        description = re.sub(r'\.\s*-', '<br/>&bull;', description, flags=re.MULTILINE)

        if 'list:' in description.lower():
            for n in range(1, 20):
                s = ' %d: ' % n
                description = description.replace(s, '<br/>&bull;%d: '%n)

        # non-breaking hyphen
        description = description.replace('-', '&#8209;')

        args = widget.args if hasattr(widget, 'args') else None
        if args is None:
            # this really only applies to label widgets, which don't have '.args'
            args = keyword_args.get(key)
            if args: # translate to consistent terms
                replace = {'phase':'P',
                           'species': 'S',
                           'scalar': 'N'}
                args = [replace.get(a, a.upper()) for a in args]

        if isinstance(args, int):
            key = '%s(%s)' % (key, args)
        elif args:
            args = ('Phase' if arg=='P'
                    else 'Species' if arg=='S'
                    else arg for arg in args)
            key = '%s(%s)' % (key, ','.join(map(
                lambda x: str(x[0] if isinstance(x, (tuple, list)) else str(x)), args)))
        if value is not None:
            key = '%s=%s' % (key, value)
        if key is None:
            msg = '<b></b>%s</br>' % description
        else:
            msg = '<b>%s</b>: %s</br>' % (key, description)

        widget.setToolTip(msg)
        widget.help_text = msg # would be nice do something more useful with help_text

    def create_project_thumbnail(self, save_gui_info=False, test=False):
        '''create a thumbnail for the project'''

        path = os.path.join(self.get_project_dir(), '.thumbnail')

        cur_widget = self.ui.tabWidgetGraphics.currentWidget()
        # if vtk 7+ and the widget is not visible, the screenshot will be empty
        # don't bother saving the thumbnail
        if not cur_widget.isVisible() and not test:
            return

        # collect meta data
        solver = self.project.solver
        solver_dict = {SINGLE:'single', TFM:'tfm', DEM:'dem', PIC:'pic', HYBRID:'hybrid'}
        s = solver_dict.get(solver, 'single')

        geo = self.project.get_value('cartesian_grid', False)
        chem = bool(self.project.reactions)
        des = self.project.get_value('description')

        # try to get image from vtk
        temp = os.path.join(self.get_project_dir(), 'temp.png')
        # find the currently visible vtk widget
        current_index = self.ui.tabWidgetGraphics.currentIndex()
        if hasattr(cur_widget, 'vtk_widget'):
            cur_widget = cur_widget.vtk_widget
        elif not hasattr(cur_widget, 'screenshot'):
            self.ui.tabWidgetGraphics.setCurrentWidget(self.ui.widgetModelGraphics)
            cur_widget = self.vtkwidget
        cur_widget.screenshot(True, temp, size=[400, 400], offscreen=True)

        # create the thumbnail
        create_thumbnail(path, s, geo, chem, temp)
        if os.path.exists(temp):
            os.remove(temp)

        # save the model types too!
        if save_gui_info:
            path = os.path.join(self.get_project_dir(), '.mfixguiinfo')
            with open(path, 'w', encoding='utf-8', errors='replace') as f:
                f.write(','.join(str(v) for v in [s, geo, chem, des]))

        # reset current tab
        self.ui.tabWidgetGraphics.setCurrentIndex(current_index)

    # Following functions are overrideable for test runner
    def confirm_clobber(self, renamed_project_file):
        clobber_msg = '%s exists, replace?' % renamed_project_file
        response = self.message(title='Warning',
                                icon='question',
                                text=clobber_msg,
                                buttons=['yes', 'no'],
                                default='no')
        return response == 'yes'

    def confirm_delete_files(self, message_text):
        response = self.message(title="Info",
                                icon="info",
                                text=message_text,
                                buttons=['ok', 'cancel'],
                                default='cancel')
        return response == 'ok'

    def find_keyword(self, key, args=None):
        if args is None:
            args = []
        elif isinstance(args, int):
            args = [args]
        key = key.lower()
        found = False
        for pane in self.ui.panes:
            if found:
                break
            for w in widget_iter(pane):
                wkey = getattr(w, 'key', None)
                if wkey == key:
                    found = True
                    self.change_pane(pane.objectName())
                    w.setStyleSheet('background: green')
                    break
        return found

    #def handle_help(self):
    #    self.help_dialog.exec_()


def main():
    main_args(sys.argv[1:])

def main_args(sys_argv):
    global gui
    gui = None
    # 'gui' is initialized here instead of at module-level so that interpreter.py can 'import gui'

    # handle command-line arguments
    av_styles = [s.lower() for s in QtWidgets.QStyleFactory.keys()]
    parser = argparse.ArgumentParser(description='MFiX GUI')
    ARG = parser.add_argument
    ARG('project', action='store', nargs='?', default=None,
        help='open mfix.dat or <RUN_NAME>.mfx project file or search a specified directory for project files')
    ARG('-e', '--exe', metavar='EXE', action='store', default=None,
        help='specify MFiX executable (full path)')
    ARG('-l', '--log', metavar='LOG', action='store', default='WARN',
        choices=['error', 'warning', 'info', 'debug'],
        help='set logging level (error, warning, info, debug)')
    ARG('-s', '--style', metavar='STYLE', action='store', default=None,
        choices=av_styles,
        help='specify app style (windowsvista, fusion, cleanlooks,...)')
    ARG('-n', '--noload', action='store_true',
        help='do not autoload previous project')
    ARG('-w', '--nonodeworks', action='store_false',
        help='do not load the nodeworks environment')
    ARG('-k', '--novtk', action='store_false',
        help='do not load vtk')
    ARG('-g', '--default_geo', action='store_true',
        help="Use default geometry, don't restore previous state.")
    ARG('-d', '--developer', action='store_true',
        help="Enable developer mode.")
    ARG('-c', '--clear', action='store_true',
        help="Clear all saved settings.")
    ARG('-t', '--test', action='store_true',
        help="Enable test mode.")
    ARG('-ct', '--thumbnails', action='store_true',
        help="Create thumbnails in test mode.")
    ARG('-v', '--version', action='version', version=__version__)

    args = parser.parse_args(sys_argv)

    if args.clear:
        print("Clearing all MFIX settings from ", SETTINGS.fileName())
        SETTINGS.clear()
        SETTINGS.sync()
        return

    # Force decimal point instead of comma for users who have set
    #   non-US locale, e.g.  de_DE.utf8
    # Need to do this early, before VTK is set up.
    # This prevents VTK from writing STL files which can't be loaded
    os.environ['LC_NUMERIC'] = 'C'

    # setup logging
    logging.basicConfig(stream=sys.stdout,
                        filemode='w', level=getattr(logging, args.log.upper()),
                        format='%(name)s - %(levelname)s - %(message)s')

    project_file = args.project
    if project_file and os.path.isdir(project_file):
        mfx_files = glob.glob(os.path.join(project_file, '*.mfx'))
        if mfx_files:
            project_file = mfx_files[0]
        else:
            dat_files = glob.glob(os.path.join(project_file, 'mfix.dat'))
            if dat_files:
                project_file = dat_files[0]
            else:
                print("Can't find *.mfx or mfix.dat in directory: %s" % project_file)
                parser.print_help()
                return

    elif project_file and not os.path.isfile(project_file):
        print("%s: is not a file " % project_file)
        parser.print_help()
        return

    # Set exception handler early, so we catch any errors in initialization
    def excepthook(etype, exc, tb):
        if args.developer or gui is None:
            traceback.print_exception(etype, exc, tb)
        if gui is None:
            return
        # TODO allow a way to quit!
        info = ("Please report this error to <a href=mailto:%s>%s</a>"
                "<p>If you continue running, the application may "
                "become unstable.  Consider saving your work now."
                "<p>Please include the following details in your bug report:"
        ) % (support_email, support_email)

        try:
            err_str = "Error: %s\n" % exc
        except Exception as ex: # unlikely
            err_str = "Error: %s\n" % etype
            log.exception(ex)

        # Clean up the traceback a little.
        # Remove some leading whitespace
        tb_list = [line[2:] if line.startswith('  ') else line for line in traceback.format_tb(tb)]
        # Don't let the traceback be too long (e.g. "recursion too deep")
        tb_list = tb_list[-20:]
        # Shorten long pathnames
        tb_list = [line.replace(SCRIPT_DIRECTORY, '...') for line in tb_list]
        details = [err_str] + tb_list

        if gui.message_box and gui.message_box.isVisible():
            # Avoid cascading dialog boxes
            return
        try:
            button = gui.message("Application error",
                                 text=info,
                                 traceback_text=''.join(details))
            #Note, there's no quit button.  message does not (easily) support this
            if button == 'quit':
                sys.exit(1)

        except Exception as e:
            log.exception("Application error")
            log.exception(''.join(details))
            log.exception("Error displaying popup: %s", e)

    if not args.test:
        sys.excepthook = excepthook

    # create the QApplication
    qapp = QtWidgets.QApplication([]) # TODO pass args to qt

    # splash screen
    splash = None
    if not args.test:
        splash = QtWidgets.QSplashScreen(get_pixmap('splash.png', 600, 338))
        splash.setWindowFlags(Qt.SplashScreen | Qt.WindowStaysOnTopHint)
        splash.show()

    def set_splash_text(text):
        if splash is None:
            return
        v = 'Version: ' + __version__
        splash.showMessage('\n'.join([v, text]), Qt.AlignHCenter|Qt.AlignBottom)
        qapp.processEvents()

    set_splash_text('Starting...')

    # set style
    app_style_arg = args.style
    app_style_settings = SETTINGS.value('app_style')
    if app_style_arg is not None:
        qapp.setStyle(app_style_arg.lower())
        SETTINGS.setValue('app_style', app_style_arg)
    elif app_style_settings is not None:
        qapp.setStyle(app_style_settings.lower())

    # create the gui
    set_splash_text('Creating application')
    gui = MfixGui(qapp, project_file=project_file, loadnodeworks=args.nonodeworks,
                  loadvtk=args.novtk, set_splash_text=set_splash_text)

    geo = SETTINGS.value('geometry')
    if geo is not None and not args.default_geo:
        # set previous geometry
        gui.restoreGeometry(geo)
        left_right = SETTINGS.value('splitter_left_right')
        if left_right is not None:
            gui.ui.splitter_left_right.setSizes([int(num) for num in left_right])

        cmd_output = SETTINGS.value('splitter_graphics_cmd_output')
        if cmd_output is not None:
            gui.ui.splitter_graphics_cmd_output.setSizes([int(num) for num in cmd_output])
    else:
        # default geometry
        geo = gui.frameGeometry()
        screen = qapp.desktop().screenNumber(qapp.desktop().cursor().pos())
        centerPoint = qapp.desktop().screenGeometry(screen).center()
        geo.moveCenter(centerPoint)
        gui.move(geo.topLeft())

    # set developer mode
    gui.handle_enable_developer_mode(int(SETTINGS.value('developer_mode', 0)) or args.developer)

    # show the gui, unless disabled
    if not args.test:
        gui.show()

    # close the splash
    if splash is not None:
        splash.close()

    if args.exe:
        #print('exe option passed: %s' % mfix_exe_option)
        gui.commandline_option_exe = args.exe

    last_project = None
    if project_file is None and not args.noload:
        # autoload last project
        last = SETTINGS.value('project_file')
        last_project = project_file = os.path.normpath(last) if last else None

    if project_file and not args.noload and os.path.exists(project_file):
        set_splash_text('Loading project')
        gui.open_project(project_file, interactive=(not args.test))
        # change mode and navigation if loaded last project
        if last_project == project_file:
            m = SETTINGS.value('mode')
            if m is not None:
                gui.change_mode(m)

            n = SETTINGS.value('navigation')
            if n is not None:
                gui.change_pane(n)
    else:
        gui.set_no_project()
        gui.handle_file_menu()

    # have to initialize vtk after the widget is visible!
    if not (args.novtk or args.test):
        gui.vtkwidget.vtkiren.Initialize()

    # exit with Ctrl-C at the terminal
    signal.signal(signal.SIGINT, signal.SIG_DFL)

    if not args.test:
        qapp.exec_()

    else:  # Run internal test suite
        gui.navigate_all()
        # create thumbnails
        if args.thumbnails:
            gui.create_project_thumbnail(save_gui_info=True, test=True)

        print("That's all folks")

if __name__ == '__main__':
    main()
