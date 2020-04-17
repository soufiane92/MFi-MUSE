# -*- coding: utf-8 -*-
'''
Dialog to build the mfixsolver from the GUI
On windows, the following paths need to be added to PATH:
  - ANACONDA_HOME/Library/mingw-w64/bin
  - ANACONDA_HOME/Library/usr/bin
'''

from os.path import join, dirname
import os
import re
import shutil

from qtpy.QtCore import (QProcess, QProcessEnvironment, QSettings, QSize)
from qtpy.QtGui import (QColor, QTextCharFormat, QFont, QFontMetrics)

from qtpy.QtWidgets import QDialog, QTextBrowser

from mfixgui.tools import safe_int, SOURCE_DIR, SCRIPT_DIRECTORY, conda_prefix
from mfixgui.tools.qt import get_ui
import mfixgui.build_mfixsolver

SETTINGS = QSettings('MFIX', 'MFIX')

RE_HIGHLIGHT_ERROR = re.compile(r'error[: ]', re.IGNORECASE)
RE_PERCENT_DONE = re.compile(r'^\[(...)%\]')


def make_build_popup(gui, cwd):
    """ Dialog box for running build_mfixsolver to build the solver """

    build_popup = get_ui('build_popup.ui')

    # override sizeHint
    build_popup.sizeHint = lambda: _size_hint(build_popup)

    # add output widget
    tb = build_popup.textBrowser_output = QTextBrowser()
    build_popup.verticalLayout_run_dialog.addWidget(tb)
    tb.setVisible(False)

    build_popup.cwd = cwd
    build_popup.build_proc = None
    build_popup.min_height = None
    build_popup.setModal(False)
    build_popup.setWindowTitle('Build Solver')

    # don't show options on windows
    visible = os.name != 'nt'

    build_popup.gui_comments = gui.project.mfix_gui_comments

    if not visible:
        build_popup.compiler_groupbox.setVisible(False)
    else:
        _init_compiler(build_popup, build_popup.comboBox_compiler, visible)
        _init_flags(build_popup, visible)

    _init_crow(build_popup, build_popup.checkBox_crow)
    _init_dmp(build_popup, build_popup.checkBox_dmp, visible)
    _init_parallel(build_popup)
    _init_smp(build_popup, build_popup.checkBox_smp, visible)

    build_popup.pushButton_build.clicked.connect(lambda: _build(build_popup))
    build_popup.pushButton_clean.clicked.connect(lambda: _clean(build_popup))
    build_popup.pushButton_cancel.clicked.connect(lambda: _cancel(build_popup))
    build_popup.pushButton_show_out.clicked.connect(lambda: _toggle_output(build_popup))

    _resize_widgets(build_popup, visible)
    _set_output_visible(build_popup, False)
    _update_build_cmd(build_popup)

    fortran_compilers = ['gfortran', 'ifort', 'mpifort', 'mpiifort', 'mpif90']
    build_popup.comboBox_compiler.addItems([compiler for compiler in fortran_compilers if shutil.which(compiler)])

    return build_popup

def _resize_widgets(build_popup, visible):

    output = build_popup.textBrowser_output
    cmd = build_popup.lineEdit_cmd

    font = QFont('Monospace' if visible else '')
    font.setStyleHint(QFont.TypeWriter)

    metrics = QFontMetrics(font)
    output.setFont(font)
    mw = build_popup.min_width = metrics.width(' ' * 80)
    output.setMinimumSize(QSize(mw,
                                20*metrics.height()))
    cmd.setMinimumSize(QSize(metrics.width(' ' * 40),
                             metrics.height()))

def _cancel(build_popup):
    if build_popup.build_proc is not None:
        build_popup.build_proc.kill()
    build_popup.close()


def closeEvent(build_popup, event):
    """ save settings before closing """
    _save_build_settings(build_popup)
    QDialog.closeEvent(build_popup, event)


def _toggle_output(build_popup, show=False):
    """ hide or show the build output TextBrowser """
    if not build_popup.textBrowser_output.isVisible() or show:
        _set_output_visible(build_popup, True)
    else:
        _set_output_visible(build_popup, False)


def _set_output_visible(build_popup, show):
    if show:
        build_popup.textBrowser_output.show()
        build_popup.pushButton_show_out.setText('Hide Output')
    else:
        build_popup.textBrowser_output.hide()
        build_popup.pushButton_show_out.setText('Show Output')
        if build_popup.min_height is None:
            build_popup.min_height = build_popup.height()
    build_popup.adjustSize()

def _size_hint(build_popup):
    height = build_popup.height()
    width = build_popup.width()

    tb_vis = build_popup.textBrowser_output.isVisible()
    tb_width = build_popup.min_width+10
    if build_popup.min_height is not None and not tb_vis:
        height = build_popup.min_height
    if tb_vis and tb_width > width:
        width = tb_width
    size = QSize(width, height)
    return size

def _get_environment():
    env = QProcessEnvironment.systemEnvironment()
    env.insert("PYTHONUNBUFFERED", "y")
    env.insert("PYTHONPATH", dirname(SCRIPT_DIRECTORY))
    if os.name == 'nt':
        anaconda_home = conda_prefix()
        path = env.value('PATH')
        new_path = os.pathsep.join([
            join(anaconda_home, 'Library', 'mingw-w64', 'bin'),
            join(anaconda_home, 'Library', 'usr', 'bin'), path
        ])
        env.insert("PATH", new_path)
    return env

def _update_build_cmd(build_popup):
    cmd = _get_build_cmd(build_popup)
    build_popup.lineEdit_cmd.setText(' '.join(cmd))
    status = 'Press "Build Solver" to compile.'
    can_build = True
    if build_popup.checkBox_dmp.isChecked():
        compiler = build_popup.comboBox_compiler.currentText()
        can_build = bool(compiler)
        if not compiler:
            status = 'Specify Compiler [wrapper] for DMP'

    build_popup.pushButton_build.setEnabled(can_build)
    build_popup.label_progressBar.setText(status)


def _clean(build_popup):
    build_popup.label_progressBar.setText('Cleaning build directory...')
    build_popup.label_progressBar.setStyleSheet('color: None;')

    build_popup.progressBar.setValue(0)
    build_popup.progressBar.setRange(0, 0)

    _print_to_output(build_popup, 'Cleaning directory: %s' % build_popup.cwd)
    build_popup.pushButton_build.setEnabled(False)
    build_popup.pushButton_cancel.setText('Cancel')
    build_popup.pushButton_clean.setEnabled(False)

    removed_stuff = mfixgui.build_mfixsolver.do_clean(build_popup.cwd)

    build_popup.progressBar.setValue(0)
    if not removed_stuff:
        _print_to_output(build_popup, 'Nothing to clean.')
    else:
        _print_to_output(build_popup, removed_stuff)
    build_popup.label_progressBar.setText('Cleaning build directory... Done')

    build_popup.pushButton_cancel.setText('Close')
    build_popup.pushButton_build.setEnabled(True)
    build_popup.pushButton_clean.setEnabled(True)

    build_popup.progressBar.setValue(0)
    build_popup.progressBar.setRange(0, 100)


def _build(build_popup):
    """ Start a build_mfixsolver process """
    build_popup.textBrowser_output.clear()
    build_popup.label_progressBar.setText('Checking requirements...')
    build_popup.label_progressBar.setStyleSheet('color: None;')

    build_popup.build_proc = QProcess()
    build_popup.build_proc.setWorkingDirectory(build_popup.cwd)

    cmd = _get_build_cmd(build_popup)
    build_popup.progressBar.setRange(0, 0)

    _print_to_output(build_popup, 'Command: %s' % cmd)
    build_popup.build_proc.setProcessEnvironment(_get_environment())
    build_popup.build_proc.readyReadStandardOutput.connect(lambda: _check_progress(build_popup))
    build_popup.build_proc.readyReadStandardError.connect(lambda: _read_err(build_popup))
    build_popup.build_proc.finished.connect(lambda code, status: _finished_building(build_popup, code, status))
    build_popup.build_proc.error.connect(lambda code: _error(build_popup, code))
    build_popup.pushButton_cancel.setText('Cancel')
    build_popup.pushButton_build.setEnabled(False)
    build_popup.pushButton_clean.setEnabled(False)
    build_popup.compiler_groupbox.setEnabled(False)
    build_popup.checkbox_groupbox.setEnabled(False)
    build_popup.build_proc.start(cmd[0], cmd[1:])


def _finished_building(build_popup, exit_code, exit_status):
    if exit_code == 0 and exit_status == 0:
        build_popup.progressBar.setRange(0, 100)
        build_popup.progressBar.setValue(100)
        build_popup.label_progressBar.setText('Build succeeded.')
    else:
        build_popup.label_progressBar.setText('Build failed.')
        build_popup.label_progressBar.setStyleSheet('color: red;')
        _toggle_output(build_popup, show=True)
    build_popup.pushButton_cancel.setText('Close')
    build_popup.pushButton_build.setEnabled(True)
    build_popup.pushButton_clean.setEnabled(True)
    build_popup.compiler_groupbox.setEnabled(True)
    build_popup.checkbox_groupbox.setEnabled(True)
    build_popup.pushButton_show_out.setEnabled(True)


def _error(build_popup, error):
    cmd = ' '.join(_get_build_cmd(build_popup))
    if error == QProcess.FailedToStart:
        msg = "Process failed to start: " + cmd
    elif error == QProcess.Crashed:
        msg = "Process exit: "  + cmd
    elif error == QProcess.Timedout:
        msg = "Process timeout: "  + cmd
    elif error in (QProcess.WriteError, QProcess.ReadError):
        msg = "Process communication error "  + cmd
    else:
        msg = "Unknown error: "  + cmd

    build_popup.label_progressBar.setText('Process Error')
    _print_to_output(build_popup, msg, error=True)
    _toggle_output(build_popup, show=True)


def _read_err(build_popup):
    err_str = bytes(build_popup.build_proc.readAllStandardError()).decode('utf-8')
    _print_to_output(build_popup, err_str, error=True)
    _toggle_output(build_popup, show=True)


def _check_progress(build_popup):
    out = bytes(build_popup.build_proc.readAllStandardOutput()).decode('utf-8')
    error = RE_HIGHLIGHT_ERROR.search(out)
    match = RE_PERCENT_DONE.search(out)
    if match:
        percent = int(match.groups()[0])
        build_popup.label_progressBar.setText('Compiling...')
        build_popup.progressBar.setRange(0, 100)
        build_popup.progressBar.setValue(percent)

    elif build_popup.progressBar.value() == 100:
        build_popup.label_progressBar.setText('Linking...')
        build_popup.progressBar.setRange(0, 0)

    _print_to_output(build_popup, out, error)


def _print_to_output(build_popup, msg, error=False):
    """ display message in the build output TextBrowser """
    cursor = build_popup.textBrowser_output.textCursor()
    cursor.movePosition(cursor.End)

    scrollbar = build_popup.textBrowser_output.verticalScrollBar()
    scrolled_to_end = scrollbar.value() == scrollbar.maximum()

    char_format = QTextCharFormat()
    char_format.setForeground(QColor('red') if error else QColor('black'))
    cursor.setCharFormat(char_format)
    cursor.insertText(_fmt_message(build_popup, msg))
    if scrolled_to_end:
        scrollbar.setValue(scrollbar.maximum())


def _fmt_message(build_popup, msg):
    if not msg.endswith('\n'):
        msg += '\n'
    return msg.replace(join(build_popup.cwd, 'mfixsolver'),
                       join('[project]', 'mfixsolver'))


def _init_dmp(build_popup, dmp, visible):
    dmp.setVisible(visible)
    if visible:
        dmp.setChecked(
            safe_int(build_popup.gui_comments.get('BUILD_DMP', '0')))
    dmp.toggled.connect(lambda: _update_build_cmd(build_popup))


def _init_smp(build_popup, smp, visible):
    smp.setVisible(visible)
    if visible:
        smp.setChecked(
            safe_int(build_popup.gui_comments.get('BUILD_SMP', '0')))
    smp.toggled.connect(lambda: _update_build_cmd(build_popup))


def _init_compiler(build_popup, compiler, visible):
    compiler.setVisible(visible)
    if visible:
        compiler.setEditText(build_popup.gui_comments.get('BUILD_FC', ''))
    compiler.editTextChanged.connect(lambda: _update_build_cmd(build_popup))


def _init_flags(build_popup, visible):
    fc_flags = build_popup.lineEdit_compiler_flags
    fc_flags.setVisible(visible)
    if visible:
        fc_flags.setText(build_popup.gui_comments.get('BUILD_FC_FLAGS', ''))
    fc_flags.textChanged.connect(lambda: _update_build_cmd(build_popup))


def _init_crow(build_popup, crow):
    visible = SETTINGS.value('developer_mode', 0)
    crow.setVisible(bool(visible))
    if visible:
        crow.setChecked(
            safe_int(build_popup.gui_comments.get('BUILD_CROW', '0')))
    crow.toggled.connect(lambda: _update_build_cmd(build_popup))


def _init_parallel(build_popup):
    parallel = build_popup.checkBox_parallel
    parallel.setChecked(
        safe_int(build_popup.gui_comments.get('BUILD_PARALLEL', '1')))
    parallel.toggled.connect(lambda: _update_build_cmd(build_popup))


def _get_build_cmd(build_popup):
    """ return list of the build_mfixsolver (or build.sh) command and arguments """
    args = []

    fcflags = build_popup.lineEdit_compiler_flags.text()
    if fcflags:
        args += [f'-DCMAKE_Fortran_FLAGS="{fcflags}"']

    if build_popup.checkBox_parallel.isChecked():
        args += ['-j']

    if build_popup.checkBox_smp.isChecked():
        args += ['--smp']

    compiler = build_popup.comboBox_compiler.currentText()
    if build_popup.checkBox_dmp.isChecked():
        args += ['--dmp']
        args += [f'-DMPI_Fortran_COMPILER={compiler}']
    elif compiler:
        args += [f'-DCMAKE_Fortran_COMPILER={compiler}']

    if SETTINGS.value('developer_mode', 0):
        if build_popup.checkBox_crow.isChecked():
            args += ['--crow']

    build_script = join(SOURCE_DIR, 'mfixgui', 'build.sh') if SOURCE_DIR else 'build_mfixsolver'
    return [build_script] + args


def _save_build_settings(build_popup):
    build_popup.gui_comments['BUILD_DMP'] = int(build_popup.checkBox_dmp.isChecked())
    build_popup.gui_comments['BUILD_SMP'] = int(build_popup.checkBox_smp.isChecked())
    build_popup.gui_comments['BUILD_FC_FLAGS'] = build_popup.lineEdit_compiler_flags.text()
    build_popup.gui_comments['BUILD_PARALLEL'] = int(build_popup.checkBox_parallel.isChecked())
