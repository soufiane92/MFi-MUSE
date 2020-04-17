#!/usr/bin/env python

import logging
import os
import sys
from distutils.version import StrictVersion
from distutils import spawn

from io import StringIO

import errno
import multiprocessing
import json

from collections import OrderedDict
from glob import glob

import configparser

from qtpy import QT_VERSION
from qtpy.QtCore import Qt, QProcess, QProcessEnvironment

from qtpy.QtWidgets import (QCheckBox, QComboBox, QDialog, QDoubleSpinBox,
                            QFileDialog, QLabel, QSpinBox, QListWidget)

from mfixgui.tools import (extract_config, replace_with_dict,
                           get_mfix_home, get_mfix_templates)
from mfixgui.tools.qt import (clear_layout, get_icon, sub_icon_size, get_ui)

from mfixgui.widgets.base import BASE_WIDGETS
from mfixgui.constants import *

log = logging.getLogger('mfix-gui' if __name__ == '__main__' else __name__)

RECENT_EXE_LIMIT = 5
MFIXSOLVER_GLOB_NAMES = ['mfixsolver', 'mfixsolver-crow', 'mfixsolver.exe', 'mfixsolver-crow.exe', 'mfixsolver.bat']
SPX_GLOB = ['*.sp?']
RESTART_2_REMOVE_GLOB = ['*.sp?', "*.pvd", "*.vtp", "*.vtu"]
RESTART_TYPES = OrderedDict([
    ('Resume', 'restart_1'),
    ('Use as initial Condition', 'restart_2'),
])
RESTART_TYPES_INVS = {v: k for k, v in RESTART_TYPES.items()}

OK, LOOKING, ERROR = [0, 1, 2]


def safe_int(s, default=1):
    try:
        return int(s)
    except:
        return default

def plural(n, word):
    fmt = "%d %s" if n == 1 else "%d %ss"
    return fmt % (n, word)

class RunPopup(QDialog):
    def __init__(self, solver, parent):
        super(RunPopup, self).__init__(parent)
        self.commandline_option_exe = solver if solver else None
        self.mfix_available = False
        self.mfix_exe_cache = {}
        self.solver_list = {}
        self.template_values = {}
        self.cmdline = []  # List of strings
        self.parent = parent
        self.project = parent.project
        self.settings = parent.settings
        self.project_dir = parent.get_project_dir()
        self.gui_comments = self.project.mfix_gui_comments
        self.flag_processes = {}
        self.title = 'Run Solver'

        # load ui
        ui = self.ui = get_ui('run_popup.ui', self)
        ui.layout.setSizeConstraint(ui.layout.SetFixedSize)

        ui.toolbutton_browse.clicked.connect(self.handle_browse_exe)
        ui.toolbutton_browse.setIcon(get_icon('add.svg'))
        ui.toolbutton_browse.setIconSize(sub_icon_size())

        ui.toolbutton_remove.clicked.connect(self.handle_remove_exe)
        ui.toolbutton_remove.setIcon(get_icon('remove.svg'))
        ui.toolbutton_remove.setIconSize(sub_icon_size())

        ui.toolbutton_view_error.clicked.connect(self.show_solver_error)

        ui.listwidget_solver_list.itemSelectionChanged.connect(self.update_dialog_options)
        ui.combobox_restart.addItems(RESTART_TYPES.keys())
        ui.combobox_restart.hide()

        ui.button_run.clicked.connect(self.handle_run)
        ui.button_cancel.clicked.connect(self.handle_abort)
        ui.pushbutton_browse_template.clicked.connect(self.handle_browse_template)

        n_cpus = multiprocessing.cpu_count()
        ui.groupbox_smp_options.setTitle("SMP Options (%s available locally)"
                                         % plural(n_cpus, "core"))
        ui.groupbox_queue.toggled.connect(self.toggle_run_btn_text)
        ui.widget_queue_options.hide()

        self.initialize_ui()
        self.init_templates()

    @property
    def solver(self):
        """The currently selected solver"""
        item = self.ui.listwidget_solver_list.currentItem()
        if item is None:
            solver = None
        else:
            solver = item.text()

        if not solver:
            solver = None

        return solver

    def toggle_run_btn_text(self):
        ui = self.ui
        ui.button_run.setText('Submit' if ui.groupbox_queue.isChecked() else 'Run')

    def update_gui_comment(self, key, val):
        if self.gui_comments.get(key) == val:
            return
        self.gui_comments[key] = val
        self.parent.set_unsaved_flag()

    # UI update functions
    def initialize_ui(self):
        ui = self.ui
        self.setWindowTitle(self.title)
        get_value = self.parent.project.get_value
        update_keyword = self.parent.update_keyword

        # restart
        spx_files = self.parent.get_output_files(SPX_GLOB)
        res = self.parent.get_res_files()
        enable = bool(spx_files) or bool(res)
        ui.groupbox_restart.setEnabled(enable)
        if not enable:
            ui.groupbox_restart.setChecked(False)
            ui.groupbox_restart.setTitle('Restart - no restart files found')

        restart_1 = bool(spx_files) and bool(res)
        if not restart_1:
            ui.combobox_restart.setCurrentIndex(1)

        self.enable_restart_item(RESTART_TYPES_INVS['restart_1'], restart_1)
        self.enable_restart_item(RESTART_TYPES_INVS['restart_2'], bool(res))

        # set OMP_NUM_THREADS
        project_threads = self.gui_comments.get('OMP_NUM_THREADS', '1')
        env_threads = os.environ.get('OMP_NUM_THREADS', None)
        if env_threads:
            project_threads = env_threads

        ui.spinbox_threads.setValue(safe_int(project_threads, default=1))

        # migrate from comments back to keywords, undo issues 149
        gui_comment_nodes = self.gui_comments.pop('OMP_NODES', None)
        if gui_comment_nodes:
            self.parent.set_unsaved_flag() # modified project
            nodes = gui_comment_nodes.split(os.path.pathsep)
            if len(nodes) == 3:
                nodes = (nodesi, nodesj, nodesk) = [safe_int(n, default=1) for n in nodes]
            else:
                nodes = (nodesi, nodesj, nodesk) = 1, 1, 1
            update_keyword('nodesi', nodesi)
            update_keyword('nodesj', nodesj)
            update_keyword('nodesk', nodesk)
        else:
            nodes = (nodesi, nodesj, nodesk) = [get_value('nodes'+c, default=1) for c in 'ijk']

        for val, spin in zip(nodes, [ui.spinbox_nodesi, ui.spinbox_nodesj, ui.spinbox_nodesk]):
            spin.setValue(val)

        # local/queue
        ui.groupbox_queue.setChecked(int(self.gui_comments.get('submit_to_queue', 0)))

        # create initial executable list
        self.get_solver_list()
        if self.solver_list:
            self.mfix_available = True
            self.populate_combobox_solver()
            for exe in self.solver_list.keys():
                self.get_exe_flags(exe)

            # select solver
            self.ui.listwidget_solver_list.setCurrentRow(0)

        self.update_dialog_options()

    def init_templates(self):
        # look for templates in MFIX_HOME/queue_templates
        search_p = os.path.join(get_mfix_templates(), 'queue_templates')
        self.templates = {}
        for root, _, files in os.walk(search_p):
            for f in files:
                p = os.path.join(root, f)
                self.add_queue_template(p)

        # look for recent templates
        temp_paths = self.settings.value('queue_templates')
        if temp_paths:
            for temp_path in temp_paths.split('|'):
                if os.path.exists(temp_path):
                    self.add_queue_template(temp_path)

        self.ui.combobox_template.currentIndexChanged.connect(self.update_queue_widgets)

        temp = self.gui_comments.get('queue_template')
        if temp:
            self.template_values = json.loads(temp)
            t_name = self.template_values.get('template')
            if t_name:
                self.set_current_template(t_name)

        self.update_queue_widgets()

    def save_template(self):
        '''Save the current template data'''
        self.collect_template_values()
        template_txt = self.ui.combobox_template.currentText()
        self.template_values['template'] = template_txt
        self.update_gui_comment('queue_template', json.dumps(self.template_values))

    def collect_template_values(self):
        template_txt = self.ui.combobox_template.currentText()
        template = self.templates.get(template_txt, {})
        replace_dict = {}
        for name, wid in template.items():
            if not isinstance(wid, dict):
                continue

            if 'widget_obj' in wid:
                wid_obj = wid['widget_obj']
                if isinstance(wid_obj, (QSpinBox, QDoubleSpinBox)):
                    self.template_values[name] = v = wid_obj.value()
                elif isinstance(wid_obj, QCheckBox):
                    self.template_values[name] = v = wid_obj.value
                    if v:
                        v = wid.get('true', '')
                    else:
                        v = wid.get('false', '')
                elif isinstance(wid_obj, QListWidget):
                    self.template_values[name] = v = wid_obj.value
                    v = ' '.join(v)
                else:
                    self.template_values[name] = v = wid_obj.value

                replace_dict[name] = v
        return replace_dict

    def set_current_template(self, name):
        '''set the template file combobox'''
        cb = self.ui.combobox_template
        for itm in range(cb.count()):
            if str(name).lower() == str(cb.itemText(itm)).lower():
                cb.setCurrentIndex(itm)
                break

    def add_queue_template(self, path, select=False):
        config, script = extract_config(path)
        c = configparser.ConfigParser()
        c.readfp(StringIO(config))

        d = OrderedDict([(s, dict(c.items(s))) for s in c.sections()])
        d['path'] = path
        d['script'] = script

        name = os.path.basename(path)
        if 'options' in d:
            name = d['options'].get('name', name)

        self.templates[name] = d

        self.ui.combobox_template.clear()
        self.ui.combobox_template.addItems(list(self.templates.keys()))

        if select:
            self.set_current_template(name)

    def update_queue_widgets(self):
        l = self.ui.groupbox_queue_options_gridlayout
        clear_layout(l)
        tp = self.ui.combobox_template.currentText()

        wids_data = self.templates.get(tp, None)
        if wids_data is None:
            return

        # check to see if qsub command present
        cmd = wids_data.get('options', {}).get('submit', None)
        if cmd is not None:
            cmd = cmd.strip().split()[0]
            if spawn.find_executable(cmd) is None:
                label = QLabel('The submission command "{}" does not exist in '
                               'the current environment. Please select another '
                               'template, edit the template, and/or check your '
                               'environment.'.format(cmd))
                label.setStyleSheet('color:red')
                label.setWordWrap(True)
                l.addWidget(label, 0, 0)
                return

        # add the widgets
        for i, wid in enumerate(list(wids_data.keys())):
            wd = wids_data[wid]
            if not isinstance(wd, dict) or wid == 'options':
                continue

            label = QLabel(wd.get('label', wid))
            l.addWidget(label, i, 0)
            widget = BASE_WIDGETS.get(wd.get('widget', 'lineedit'), BASE_WIDGETS['lineedit'])()
            items = [it.strip() for it in wd.get('items', '').split('|')]
            v = self.template_values.get(wid)
            if not v or self.template_values.get('template') != tp:
                v = wd.get('value')
            if isinstance(widget, QComboBox) and items:
                widget.addItems(items)
                if v not in items:
                    v = items[0]
            elif isinstance(widget, QListWidget) and items:
                widget.add_items(items)
                widget.setMaximumHeight(100)

            widget.updateValue('', v)
            widget.help_text = wd.get('help', 'No help available.')
            l.addWidget(widget, i, 1)
            wd['widget_obj'] = widget

    def populate_combobox_solver(self):
        """ Add items from self.solver_list to combobox,
        select the first item """
        ui = self.ui
        ui.listwidget_solver_list.clear()
        ui.listwidget_solver_list.addItems(self.solver_list.keys())

    def enable_restart_item(self, text, enable):
        cb = self.ui.combobox_restart
        model = cb.model()
        index = cb.findText(text)
        item = model.item(index)
        if not enable:
            flags = Qt.NoItemFlags
        else:
            flags = Qt.ItemIsSelectable | Qt.ItemIsEnabled

        item.setFlags(flags)

    def update_dialog_options(self):
        """ Enable or disable options based on self.solver features,
        local or remote settings """
        ui = self.ui
        status_text = ''
        get_value = self.parent.project.get_value

        # show status/errors
        error_msg = self.get_error()
        status = self.get_status()
        error = status == ERROR

        ui.toolbutton_view_error.setVisible(error)
        if error and error_msg:
            status_text = 'Solver Error:'

        # Enable/disable widgets
        enable = self.mfix_available and self.solver is not None and not error

        ui.button_run.setEnabled(enable)
        ui.widget_queue_options.setEnabled(enable)

        dmp = self.dmp_enabled()
        smp = self.smp_enabled()
        ui.groupbox_smp_options.setEnabled(enable and smp)
        ui.groupbox_dmp_options.setEnabled(enable and dmp)

        python = self.python_enabled()

        if status == LOOKING:
            status_text = 'Checking Solver.'
        elif not python and not error:
            status_text = 'Warning: Can not talk to selected solver (not python enabled).'

        ui.spinbox_nodesi.setEnabled(dmp)
        ui.spinbox_nodesj.setEnabled(dmp)
        ui.spinbox_nodesk.setEnabled(dmp and not get_value('no_k'))
        ui.spinbox_threads.setEnabled(smp)

        ui.label_solver_error.setText(status_text)

    def show_solver_error(self):
        error_msg = '\n'.join(self.get_error())
        self.parent.message(
            text='The solver test failed with the following error:',
            info_text=error_msg)

    def popup(self):
        self.show()
        self.raise_()
        self.activateWindow()

    def closeEvent(self, event):
        """save information on close"""
        ui = self.ui

        # save solver list
        self.save_selected_exe()

        # queue
        self.save_template()
        self.update_gui_comment('submit_to_queue', int(ui.groupbox_queue.isChecked()))
        self.update_gui_comment('OMP_NUM_THREADS', str(ui.spinbox_threads.value()))

        self.parent.save_project()

    # event handlers
    def handle_abort(self):
        self.close()

    def finish_with_dialog(self):
        """ save run options in project file, then emit run signal """
        ui = self.ui
        update_keyword = self.parent.update_keyword
        get_value = self.parent.project.get_value

        project_dir = self.parent.get_project_dir()
        udfs = glob(os.path.join(project_dir, "*.f"))
        udf_msg = ("Warning: Fortran source files exist for this project, but"
                   " the selected mfixsolver is not in the  project directory."
                   " This case probably won't run correctly unless this"
                   " project's custom mfixsolver is selected. Proceed anyway?")

        if udfs and "[project]" + os.sep + "mfixsolver" not in self.solver:
            response = self.parent.message(
                title='Warning',
                icon='question',
                text=udf_msg,
                buttons=['yes', 'no'],
                default='no')
            if response != 'yes':
                return

        thread_count = str(ui.spinbox_threads.value())
        # FIXME: should not pollute local env (see saved_env)
        os.environ['OMP_NUM_THREADS'] = thread_count
        log.info('SMP enabled with OMP_NUM_THREADS=%s', os.environ["OMP_NUM_THREADS"])
        self.update_gui_comment('OMP_NUM_THREADS', thread_count)

        # restart
        if ui.groupbox_restart.isChecked():
            restart_type = RESTART_TYPES.get(ui.combobox_restart.currentText(), 'restart_1')
            update_keyword('run_type', restart_type)

            # restart_2
            if restart_type == 'restart_2':
                spx_files = self.parent.get_output_files(RESTART_2_REMOVE_GLOB)
                if not self.parent.remove_output_files(spx_files, force_remove=True):
                    log.debug('SP* files exist and run was canceled')
                    return False

        # normal run
        else:
            update_keyword('run_type', 'new')
            output_files = self.parent.get_output_files()
            if output_files:
                message = 'Starting a new run requires the following files to be deleted from the run directory.'
                if not self.parent.remove_output_files(output_files, message_text=message, force_remove=True):
                    log.info('output files exist and run was canceled')
                    return False

        # collect nodes[ijk]
        nodesi = ui.spinbox_nodesi.value()
        nodesj = ui.spinbox_nodesj.value()
        nodesk = ui.spinbox_nodesk.value()

        if not self.dmp_enabled():
            nodesi = nodesj = nodesk = 1

        # write the correct nodes[ijk] to project file
        update_keyword('nodesi', nodesi)
        update_keyword('nodesj', nodesj)
        if not get_value('no_k'):
            update_keyword('nodesk', nodesk)
        else:
            update_keyword('nodesk', 1)

        if self.parent.unsaved_flag:  # run_type keyword updated and/or nodesi/nodesj/nodesk
            self.parent.save_project()
            self.parent.update_source_view()
        else:
            stl = os.path.join(self.parent.get_project_dir(), 'geometry.stl') # is this needed?
            self.parent.vtkwidget.export_stl(stl)

        self.close()
        self.parent.signal_update_runbuttons.emit('')
        return True

    def handle_run(self):
        if not self.finish_with_dialog():
            self.parent.slot_update_runbuttons()
            return

        # reset plots
        self.parent.reset_plots()

        self.parent.job_manager.stopping = False
        self.parent.job_manager.pausing = False
        self.parent.last_run_msg_time = 0.0
        if self.ui.groupbox_queue.isChecked():
            self.submit()
        else:
            self.run()
            self.parent.slot_update_runbuttons()

    def run(self):
        self.run_cmd = self.get_run_command()
        msg = 'Starting %s' % ' '.join(self.run_cmd)
        self.parent.print_internal(msg, color='blue')
        self.start_command(
            cmd=self.run_cmd,
            cwd=self.parent.get_project_dir(),
            env=os.environ)

    def submit(self):
        msg = 'Submitting to queue'
        self.parent.print_internal(msg, color='blue')
        self.submit_command(*self.get_submit_command())

    def handle_remove_exe(self):
        ui = self.ui
        row = ui.listwidget_solver_list.currentRow()
        item = ui.listwidget_solver_list.currentItem()
        path = item.text()
        if not path.startswith('[default]') and not path.startswith('[project'):
            ui.listwidget_solver_list.takeItem(row)

    def handle_browse_exe(self):
        """ Handle file open dialog for user specified exe """
        new_exe, ignore = QFileDialog.getOpenFileName(
            self, "Select Executable",
            directory=self.project_dir,
            options=QFileDialog.DontResolveSymlinks)

        if not new_exe:
            return

        key = self.replace_solver_path(new_exe)
        lw = self.ui.listwidget_solver_list
        items = [lw.item(i).text() for i in range(0, lw.count())]
        if key in items:
            self.parent.message(text='The selected solver is already in the list of '
                                'available solvers.')
            return

        # check solver
        ok, message = self.check_exe(new_exe)
        if not ok:
            self.parent.message(text=message)
            return

        self.save_selected_exe(new_exe)
        self.mfix_available = True

        lw.insertItem(0, key)
        lw.setCurrentRow(0)
        log.debug('selected new exe %s', key)

    def handle_browse_template(self):
        """ Handle file open dialog for user specified exe """
        new_temp, ignore = QFileDialog.getOpenFileName(
            self, "Select a Template",
            directory=self.project_dir)
        if not new_temp:
            return

        self.add_queue_template(new_temp, select=True)

        # add it to the recent settings
        temp_paths = self.settings.value('queue_templates')
        good_paths = [os.path.realpath(new_temp)]
        if temp_paths:
            for temp_path in temp_paths.split('|'):
                if os.path.exists(temp_path):
                    good_paths.append(temp_path)

        self.settings.setValue('queue_templates',
                               '|'.join(list(set(good_paths))[:RECENT_EXE_LIMIT]))

    # utils
    def save_selected_exe(self, new_solver=None):
        """ add new executable to recent list, save in project file and config,
        send signal(s) """
        if new_solver is None:
            new_solver = self.solver
        if new_solver is None:
            self.parent.warn('No solver selected')
            return

        key = self.replace_solver_path(new_solver)

        self.settings.setValue('mfix_exe', key)
        self.update_gui_comment('mfix_exe', key)

        lw = self.ui.listwidget_solver_list
        recent_list = [lw.item(i).text() for i in range(0, lw.count())]

        # truncate to maximum
        recent_list = recent_list[:RECENT_EXE_LIMIT]

        # make new solver in front
        if new_solver is not None:
            if new_solver in recent_list:
                recent_list.pop(recent_list.index(new_solver))

            recent_list.insert(0, new_solver)

        # add to solver list
        nl = OrderedDict([(key, new_solver)])
        for key in recent_list:
            val = self.solver_list.get(key, None)
            if val is not None:
                nl[key] = val

        self.solver_list = nl
        # save
        self.settings.setValue(
            'recent_executables',
            str(os.pathsep).join(recent_list))

    def get_solver_list(self):
        """ assemble list of executables from:
        - command line
        - project file 'mfix_exe'
        - project dir
        - config item 'recent_executables'
        - default install location
        """

        def recently_used_executables():
            recent_list = self.settings.value('recent_executables')
            if recent_list:
                # limit recently used exes to RECENT_EXE_LIMIT
                recent_lim = recent_list.split(os.pathsep)[:RECENT_EXE_LIMIT]
                recent_list = [
                    exe for exe in recent_lim if os.path.exists(exe)
                ]
                for recent_exe in recent_list:
                    yield recent_exe

        def project_directory_executables():
            for name in MFIXSOLVER_GLOB_NAMES:
                for exe in glob(os.path.join(self.project_dir, name)):
                    yield os.path.realpath(exe)

        def project_file_executable():
            project_exe = self.gui_comments.get('mfix_exe', None)
            if project_exe:
                yield project_exe

        def python_path():
            for d in sys.path:
                # filter out empty strings and current directory from $PATH
                if d and d != os.path.curdir and os.path.isdir(d):
                    for name in MFIXSOLVER_GLOB_NAMES:
                        for exe in glob(os.path.join(d, name)):
                            yield exe

        def os_path():
            PATH = os.environ.get("PATH")
            if PATH:
                # using OrderedDict to preserve PATH order
                dirs = OrderedDict.fromkeys(PATH.split(os.pathsep))
            else:
                dirs = OrderedDict()
            for d in dirs.keys():
                # filter out empty strings and current directory from $PATH
                if d and d != os.path.curdir and os.path.isdir(d):
                    for name in MFIXSOLVER_GLOB_NAMES:
                        for exe in glob(os.path.join(d, name)):
                            yield exe

        def mfix_build_directories():
            for d in set([get_mfix_home()]):
                for name in MFIXSOLVER_GLOB_NAMES:
                    for exe in glob(os.path.join(d, name)):
                        yield exe

        def get_saved_exe():
            last_exe = self.settings.value('mfix_exe')
            if last_exe and os.path.exists(last_exe):
                yield last_exe

        def command_line_option():
            if self.commandline_option_exe and os.path.exists(self.commandline_option_exe):
                yield self.commandline_option_exe

        # Why this order?  Shouldn't command_line_option or proj* be first?
        exe_list_order = [
            command_line_option,
            project_file_executable,
            project_directory_executables,
            python_path,
            os_path,
            recently_used_executables,
            mfix_build_directories,
            get_saved_exe,
        ]

        # use an ordered dict because it acts like an ordered set
        self.solver_list = od = OrderedDict()

        # look for executables in the order listed in exe_list_order
        for exe_spec in exe_list_order:
            for exe in exe_spec():
                # expand short hand
                if '[project]' in exe:
                    exe = exe.replace('[project]', self.prj_dir())
                elif '[default]' in exe:
                    exe = exe.replace('[default]', self.def_dir())

                # make sure it is an abs path
                exe = os.path.realpath(exe)
                # simple checking
                ok, message = self.check_exe(exe)

                # truncate paths to [project]/mfixsolver etc.
                key = self.replace_solver_path(exe)
                if not ok:
                    self.parent.warn(message)
                elif key not in od:
                    od[key] = exe

    def prj_dir(self):
        return os.path.realpath(self.project_dir)

    def def_dir(self):
        return os.path.dirname(os.path.realpath(sys.executable))

    def replace_solver_path(self, path):
        if path.startswith(self.prj_dir()):
            path = path.replace(self.prj_dir(), '[project]')
        elif path.startswith(self.def_dir()):
            path = path.replace(self.def_dir(), '[default]')

        return path

    def check_exe(self, path):

        if not os.path.isfile(path):
            return False, '{} is not a file.'.format(path)

        # try executable
        if not os.access(path, os.X_OK):
            return False, '{} is not a executable.'.format(path)

        # windows, check extension
        if os.name == 'nt':
            ext = os.path.splitext(path)[-1]
            if not (ext.endswith('.exe') or ext.endswith('.bat')):
                return False, 'Extension {} is not recognized, must be .exe or .bat'.format(ext)

        return True, 'ok'

    def get_solver_key(self, solver):
        if solver is None:
            return None
        try:
            stat = os.stat(self.solver_list.get(solver))
        except OSError as e:
            log.debug(str(e))
            return None

        key = (stat, solver)
        return key

    def get_exe_flags(self, solver):
        """ get and cache (and update) executable features """
        key = self.get_solver_key(solver)
        if key is None:
            return None

        # stat will have changed if the exe has been modified since last check
        if key in self.mfix_exe_cache:
            info = self.mfix_exe_cache[key]
            return info.get('flags')
        # spawn process to get flags
        else:
            self.set_solver_icon(solver, LOOKING)
            self.mfix_exe_cache[key] = {
                'status': LOOKING,
                'flags': None,
                'stdout': [],
                'stderror': [],
                'error': [],
            }
            self.spawn_flag_process(*key)
            return LOOKING

    def spawn_flag_process(self, stat, solver):

        log.debug('Feature testing MFiX %s', solver)
        key = (stat, solver)

        proc = self.flag_processes.get(key, None)
        if proc is not None:
            proc.kill()

        exe = self.solver_list.get(solver)
        exe_dir = os.path.dirname(exe)
        proc = self.flag_processes[key] = QProcess()
        proc.setProcessEnvironment(QProcessEnvironment.systemEnvironment())
        proc.readyReadStandardOutput.connect(lambda k=key: self.flag_process_out(k))
        proc.readyReadStandardError.connect(lambda k=key: self.flag_process_error(k))
        proc.finished.connect(lambda ecode, estat, k=key: self.flag_process_finished(k, ecode, estat))
        proc.error.connect(lambda e, k=key: self.flag_process_error(k, e))
        proc.setWorkingDirectory(exe_dir)
        proc.start(exe, ["--print-flags"])

    def flag_process_error(self, key, error=None):
        info = self.mfix_exe_cache.get(key)
        self.set_solver_icon(key[1], ERROR)
        info['status'] = ERROR
        if error is None:
            error = bytes(self.flag_processes[key].readAllStandardError()).decode('utf-8', errors='ignore')
            info['stderror'].append(error)
        else:
            info['error'].append(error)

        log.debug("could not run {} --print-flags: {}".format(key[1], error))

    def flag_process_out(self, key):
        info = self.mfix_exe_cache.get(key)
        out = bytes(self.flag_processes[key].readAllStandardOutput()).decode('utf-8', errors='ignore')
        info['stdout'].append(out)
        info['flags'] = str('\n'.join(info['stdout'])).strip()
        log.debug("stdout: {} --print-flags: {}".format(key[1], out))

    def flag_process_finished(self, key, exit_code, exit_status):
        info = self.mfix_exe_cache.get(key, {})
        status = info.get('status', ERROR)
        if info.get('stderror', []):
            status = ERROR
        if status == LOOKING:
            status = OK

        info['status'] = status
        self.set_solver_icon(key[1], status)

        if self.solver == key[1]:
            self.update_dialog_options()

        log.debug("finished: {} --print-flags: {}, {}".format(key[1], exit_code, exit_status))

    def set_solver_icon(self, solver, status):
        items = self.ui.listwidget_solver_list.findItems(solver, Qt.MatchExactly)
        if not items:
            return
        item = items[0]

        icon = 'error_outline.svg'
        if status == LOOKING:
            icon = 'timelapse.svg'
        elif status == OK:
            icon = 'check_outline.svg'

        item.setIcon(get_icon(icon))

    def dmp_enabled(self):
        flags = self.get_exe_flags(self.solver)
        dmp = False
        if flags is not None:
            dmp = 'dmp' in str(flags)
        return dmp

    def smp_enabled(self):
        flags = self.get_exe_flags(self.solver)
        smp = False
        if flags is not None:
            smp = 'smp' in str(flags)
        return smp

    def python_enabled(self):
        flags = self.get_exe_flags(self.solver)
        python = False
        if flags is not None:
            python = 'python' in str(flags)
        return python

    def get_error(self):
        key = self.get_solver_key(self.solver)
        if key is None:
            return None
        info = self.mfix_exe_cache.get(key, {})
        return info.get('stderror', None)

    def get_status(self):
        key = self.get_solver_key(self.solver)
        if key is None:
            return None
        info = self.mfix_exe_cache.get(key, {})
        return info.get('status', None)

    def get_run_command(self):
        get_value = self.parent.project.get_value

        # collect nodes[ijk] from project to guarantee that mpirun matches
        nodesi = get_value('nodesi', 1)
        nodesj = get_value('nodesj', 1)
        nodesk = get_value('nodesk', 1)

        np = nodesi * nodesj * nodesk
        if self.dmp_enabled() and np > 1:
            dmp = ['mpirun',
                   # '-quiet',
                   # '-mca', 'orte_create_session_dirs', 'true',
                   '-mca', 'mpi_warn_on_fork', '0',
                   '-np', str(nodesi * nodesj * nodesk)]
        else:
            dmp = []

        #if self.dmp_enabled():
        #    run_cmd += ['nodesi=%s'%nodesi,
        #                'nodesj=%s'%nodesj]
        #    if not self.parent.project.get_value('no_k'):
        #        run_cmd += ['nodesk=%s'%nodesk]

        if self.smp_enabled():
            num_threads = str(self.ui.spinbox_threads.value())
            smp = ['env', 'OMP_NUM_THREADS=%s' % num_threads]
        else:
            smp = []

        run_cmd = smp + dmp + [self.solver_list.get(self.solver)]

        # Add 'server' flag to start HTTP server
        run_cmd += ['-s']

        # Specify project file
        project_filename = self.parent.get_project_file()
        run_cmd += ['-f', project_filename]

        return run_cmd

    def get_submit_command(self):
        cmd = self.get_run_command()

        template_txt = self.ui.combobox_template.currentText()
        template = self.templates[template_txt]

        # collect widget values
        replace_dict = self.collect_template_values()
        replace_dict.update({
            'PROJECT_NAME': self.parent.project.get_value('run_name', default=''),
            'COMMAND': ' '.join(cmd),
            'MFIX_HOME': get_mfix_home(),
        })

        # replace twice to make sure that any references added the first time
        # get replaced
        script = replace_with_dict(template['script'], replace_dict)
        script = replace_with_dict(script, replace_dict)

        sub_cmd = template['options'].get('submit', False)
        delete_cmd = template['options'].get('delete', False)  # XXX
        status_cmd = template['options'].get('status', False)
        job_id_regex = template['options'].get('job_id_regex', None)

        ## FIXME, return something nicer than this 6-tuple
        return script, sub_cmd, delete_cmd, status_cmd, job_id_regex, replace_dict

    def submit_command(self, script, sub_cmd, delete_cmd, status_cmd, job_id_regex, replace_dict):

        self.remove_mfix_stop()

        if not sub_cmd:
            template_txt = self.ui.combobox_template.currentText()
            self.parent.error(('The template file at: {}\n'
                               'does not have a submit_cmd defined').format(template_txt))
            return

        self.parent.job_manager.submit_command(script,
                                               sub_cmd,
                                               delete_cmd,
                                               status_cmd,
                                               job_id_regex,
                                               replace_dict)

    def remove_mfix_stop(self):
        mfix_stop_file = os.path.join(self.parent.get_project_dir(), 'MFIX.STOP')
        if os.path.exists(mfix_stop_file):
            try:
                os.remove(mfix_stop_file)
            except OSError:
                self.parent.warn("Cannot remove %s", mfix_stop_file)
                return

    def start_command(self, cmd, cwd, env):
        """Start MFIX in QProcess"""

        self.cmdline = cmd  # List of strings, same as psutil

        self.remove_mfix_stop()

        self.mfixproc = QProcess()
        if not self.mfixproc:
            log.warning("QProcess creation failed")
            return
        self.mfixproc.setWorkingDirectory(cwd)

        def slot_start():
            # processId was only added in qt 5.3
            if StrictVersion(QT_VERSION) > StrictVersion('5.3'):
                pid = self.mfixproc.processId()
            else:
                pid = self.mfixproc.pid()

            msg = "MFiX process %d is running" % pid
            self.parent.signal_update_runbuttons.emit(msg)

        def slot_read_out():
            # Why convert to bytes then decode?
            out_str = bytes(self.mfixproc.readAllStandardOutput()).decode('utf-8', errors='ignore')
            self.parent.stdout_signal.emit(out_str)

        def slot_read_err():
            err_str = bytes(self.mfixproc.readAllStandardError()).decode('utf-8', errors='ignore')
            self.parent.stderr_signal.emit(err_str)

        def slot_finish(status):
            # This should really be in the job manager
            if self.parent.job_manager.job:
                self.parent.job_manager.job.cleanup_and_exit()
                self.parent.job_manager.job = None
                msg = "MFiX process has stopped"
                self.parent.signal_update_runbuttons.emit(msg)

            if self.parent.job_manager.pidfile:
                try:
                    os.unlink(self.parent.job_manager.pidfile)
                    self.parent.job_manager.pidfile = None
                except OSError as e:
                    if e.errno != errno.ENOENT:
                        raise

        def slot_error(error):
            cmd_str = ' '.join(self.cmdline)
            if error == QProcess.FailedToStart:
                msg = "Process failed to start " + cmd_str
            elif error == QProcess.Crashed:
                msg = "Process exit " + cmd_str
            elif error == QProcess.Timedout:
                msg = "Process timeout " + cmd_str
            elif error in (QProcess.WriteError, QProcess.ReadError):
                msg = "Process communication error " + cmd_str
            else:
                msg = "Unknown error " + cmd_str

            log.warning(msg)
            # make the message print in red
            self.parent.stderr_signal.emit(msg)

        self.mfixproc.started.connect(slot_start)
        self.mfixproc.readyReadStandardOutput.connect(slot_read_out)
        self.mfixproc.readyReadStandardError.connect(slot_read_err)
        self.mfixproc.finished.connect(slot_finish)
        self.mfixproc.error.connect(slot_error)
        start_detached = True

        #if sys.platform.startswith('win') or 'mpirun' not in cmd:
        #    start_detached = False
        # On Windows, start_detached gives a DOS box
        # What was the issue with mpirun?
        start_detached = False

        # https://bugreports.qt.io/browse/QTBUG-2284
        # QProcessEnvironment does not work with startDetached,
        #   fixed in Qt5.10 which we aren't using yet
        saved_env = None
        if not start_detached:
            process_env = QProcessEnvironment()
            add_env = process_env.insert
        else:
            add_env = os.environ.__setitem__
            saved_env = os.environ.copy()

        for key, val in env.items():
            add_env(key, val)

        add_env('MFIX_RUN_CMD', ' '.join(cmd))

        if not start_detached:
            self.mfixproc.setProcessEnvironment(process_env)
            self.mfixproc.start(cmd[0], cmd[1:])
        else:
            self.mfixproc.startDetached(cmd[0], cmd[1:])

        # restore environment
        if saved_env:
            for (k, v) in list(os.environ.items()):
                if k not in saved_env:
                    del os.environ[k]
                elif v != saved_env[k]:
                    os.environ[k] = saved_env[k]

        # give gui a reference
        self.parent.mfix_process = self.mfixproc
        self.parent.slot_rundir_timer()
