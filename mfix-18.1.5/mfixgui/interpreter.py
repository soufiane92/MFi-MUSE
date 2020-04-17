# In-process Python REPL for MFIX-GUI


import sys
import traceback

from code import InteractiveConsole

from mfixgui.version import __version__

from qtpy import QtGui
from qtpy.QtCore import QEvent, Qt


class Interpreter(object):

    def eventFilter(self, obj, event):
        if event.type() != QEvent.KeyPress:
            return False

        if event.key() == Qt.Key_Up:
            self.interp_history_lineno -= 1
            if self.interp_history_lineno < 0:
                self.interp_history_lineno = 0
        elif event.key() == Qt.Key_Down:
            self.interp_history_lineno += 1
            if self.interp_history_lineno >= len(self.interp_history):
                self.interp_history_lineno = len(self.interp_history) - 1
        else:
            return False

        self.ui.lineedit_interpreter_input.setText(self.interp_prompt +
            self.interp_history[self.interp_history_lineno])

        return False

    def init_interpreter(self):
        self.interp_setup_done = False
        self.interp_history = []
        self.interp_history_lineno = 0
        le = self.ui.lineedit_interpreter_input
        te = self.ui.textedit_interpreter_output
        le.installEventFilter(self)
        le.editingFinished.connect(self.handle_interp_line)
        self.PS1 = '>>> '
        self.PS2 = '... '
        self.interp_prompt = self.PS1
        le.setText(self.interp_prompt)
        le.textEdited.connect(self.handle_interp_key)
        class Output:
            def __init__(self, textedit=te, err=False):
                self.textedit = te
                self.err = err
            def write(self, text):
                text = text.rstrip()
                if text:
                    cursor = te.textCursor()
                    cursor.movePosition(cursor.End)
                    char_format = QtGui.QTextCharFormat()
                    char_format.setForeground(QtGui.QColor('red' if self.err else 'black'))
                    cursor.setCharFormat(char_format)
                    cursor.insertText(text+'\n')
                    #te.ensureCursorVisible()
                    #scrollbar = te.verticalScrollBar()  # this scrolls too far
                    #scrollbar.setValue(scrollbar.maximum())

        self.stdout = Output()
        self.stderr = Output(err=True)
        self.interp = InteractiveConsole()
        banner = 'Python ' + sys.version + ' on ' + sys.platform + '\n'
        banner += 'MFiX-GUI version %s' % __version__ + '\n'
        te.insertPlainText(banner)


    def setup_interpreter(self):
        if self.interp_setup_done:
            return

        self.interp_setup_done = True
        try:
            from mfixgui.gui import gui as g
            installed = True
        except ImportError:
            installed = False
        if installed:
            prelude = ("from mfixgui.gui import gui as g", "p = g.project")
        else:
            prelude = ("from __main__ import gui as g", "p = g.project")
            installed = False

        for line in prelude:
            self.ui.lineedit_interpreter_input.setText('>>> ' + line)
            self.handle_interp_line()

    def capture_output(self, enable):
        if enable:
            sys.stdout = self.stdout
            sys.stderr = self.stderr
        else:
            sys.stdout = sys.__stdout__
            sys.stderr = sys.__stderr__

    def handle_interp_key(self):
        # Don't allow user to backspace over prompt
        le = self.ui.lineedit_interpreter_input
        text = le.text()
        if len(text) < 4:
            le.setText(self.interp_prompt)

    def handle_interp_line(self):
        le = self.ui.lineedit_interpreter_input
        text = le.text().rstrip()
        if len(text) == 3 and self.interp_prompt == self.PS1: # No input
            return
        self.stdout.write(text+'\n')
        text = text[4:] # Remove prompt
        le.setText('')
        try:
            saved_excepthook = sys.excepthook
            sys.excepthook = traceback.print_exception
            result = self.interp.push(text)
        except Exception as e:
            result = str(e)
        finally:
            sys.excepthook = saved_excepthook
        self.interp_history.append(text)
        self.interp_history_lineno = len(self.interp_history)
        self.interp_prompt = self.PS2 if result else self.PS1 # True = more input required
        le.setText(self.interp_prompt)
