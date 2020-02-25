"""Species selector dialog for MFIX GUI, includes stand-alone test"""

# 2016-11-20  Species/alias unification
#  we will only expose 'alias' to the user.  'species' is only used
#  as a key into Burcat/THERMO_DATA, and we're going to inline all
#  of the thermodynamic data - cgw

import os
import sys
import signal
from collections import OrderedDict
import pickle
from copy import deepcopy

from qtpy.QtWidgets import (QApplication, QDialog, QHeaderView,
                            QLineEdit, QTableWidgetItem)

from qtpy.QtGui import  QDoubleValidator, QValidator
from qtpy.QtCore import Signal

from mfixgui.tools.qt import (get_combobox_item, get_selected_row,
                              get_selected_rows, set_item_noedit, get_ui)

def resize_column(tw, col, flags):
    tw.horizontalHeader().setSectionResizeMode(col, flags)

# Make search case-, whitespace- and punctuation-insensitive
squash_table = [c.lower() if c.isalnum() else None for c in map(chr,range(256))]
def squash(string):
    r = string.translate(squash_table)
    return r


phase_names = 'Gas', 'Liquid', 'Solid', 'Composite'

class SpeciesPopup(QDialog):

    save = Signal()
    cancel = Signal()

    def load_burcat(self, path):
        if not os.path.exists(path):
            print("%s not found, create it by running read_burcat.py" % path)
            sys.exit(-1)
        with open(path, 'rb') as db_file:
            database = pickle.load(db_file)
        by_phase = {}

        for k, v in database.items():
            phase = k[1]
            if phase not in by_phase:
                by_phase[phase] = {}
            key = k[0], k[2], k[3]
            by_phase[phase][key] = v
        self.db = by_phase

        # build search list, lowercased
        self.haystack = []
        self.comments = {}
        for phase in 'GLSC':
            htmp = [((squash(k[0]), squash(v[2])), k, phase) for (k,v) in self.db[phase].items()]
            htmp.sort()
            self.haystack.extend(htmp)
            # comment fields
            self.comments[phase] = dict((k, v[2])
                                        for (k,v) in self.db[phase].items())


    def do_search(self, string):
        lineedit = self.ui.lineedit_search
        string = string.lstrip() # Don't allow leading spaces
        if string != lineedit.text():
            lineedit.setText(string)
            return

        self.ui.tablewidget_search.clearContents()
        results = []
        match_empty = True # Show all possibilities when no search string
        if match_empty or string:
            needle = squash(string)
            for (k, key, phase) in self.haystack:
                if (phase in self.phases and
                    (needle in k[0] or
                     (self.include_comments and needle in k[1]))):
                    results.append((key, phase))
        # Put exact matches & leading-substring matches first
        if string:
            results.sort(
                key=lambda x:
                (1 - x[0][0].lower().startswith(string.lower()), x))

        tw = self.ui.tablewidget_search
        nrows = len(results)
        self.ui.tablewidget_search.clearContents()
        self.ui.tablewidget_search.setRowCount(nrows)
        self.search_results = [None]*nrows

        # http://stackoverflow.com/questions/10192579/
        tw.model().blockSignals(True)
        for (i, r) in enumerate(results):
            key, phase = r
            comment = self.comments[phase][key]
            item = QTableWidgetItem(key[0])
            item.setToolTip(comment)
            set_item_noedit(item)
            tw.setItem(i, 0, item)
            item = QTableWidgetItem(phase)
            set_item_noedit(item)
            tw.setItem(i, 1, item)
            self.search_results[i] = (key, phase)
        tw.model().blockSignals(False)


    def get_species_data(self, key, phase):
        """exposes species database to external clients"""
        db = self.db.get(phase)
        if not db:
            return None
        # FIXME, this is inefficient.  remove tmin/tmax from key tuple.
        #  also, it's possible that there are multiple definitions for the
        #  same species, with different temp. ranges.  This just returns
        #  the first one
        for (keytuple, data) in db.items():
            (species, tmin, tmax) = keytuple
            if species == key:
                (coeffs, mol_weight, _) = data
                a_high = coeffs[:7]
                a_low = coeffs[7:14]
                h_f = coeffs[14]
                return {'phase': phase,
                        'mol_weight': mol_weight,
                        'h_f': h_f,
                        'tmin':  tmin,
                        'tmax': tmax,
                        'a_low': a_low,
                        'a_high': a_high}


    def handle_search_selection(self):
        row = get_selected_row(self.tablewidget_search)
        self.ui.pushbutton_import.setEnabled(row is not None)


    def handle_include_comments(self, val):
        self.include_comments = val
        self.do_search(self.ui.lineedit_search.text())


    def clear_species_panel(self):
        for item in self.species_panel_items:
            item.setEnabled(False)
            if hasattr(item, 'setText'):
                item.setText('')
        tw = self.ui.tablewidget_params
        for row in range(8):
            for col in range(2):
                w = tw.cellWidget(row, col)
                if w:
                    w.setText('')
                    #tw.cellWidget(i,j).setText('')


    def enable_species_panel(self):
        for item in self.species_panel_items:
            item.setEnabled(True)
        species = self.current_species
        data = self.defined_species.get(species)
        ui = self.ui
        self.ui.combobox_phase.setEnabled(False)

        def make_item(val, key=None):
            item = QLineEdit()
            item.setText(str(val))
            item.setValidator(QDoubleValidator(item))
            item.setFrame(False)
            if key:
                item.editingFinished.connect(make_handler(item=item, key=key))
            return item

        def make_handler(item, key):
            def handler(item=item, key=key):
                if not self.current_species:
                    print("Error, no current species")
                    return
                val = item.text()
                try:
                    data = self.defined_species[self.current_species]
                    val = float(val)
                    if isinstance(key, tuple):
                        data[key[0]][key[1]] = val
                    else:
                        data[key] = val
                except ValueError:
                    # reset field to prev. value
                    pass
            return handler

        ui.label_species.setText(species)
        i = 'GLSC'.index(data['phase'])
        ui.combobox_phase.setCurrentIndex(i)
        ui.combobox_phase.setToolTip(phase_names[i])
        ui.lineedit_alias.setText(data['alias'])
        ui.lineedit_mol_weight.setText(str(data['mol_weight']))
        handler = make_handler(ui.lineedit_mol_weight, 'mol_weight')
        ui.lineedit_mol_weight.editingFinished.connect(handler)
        ui.lineedit_h_f.setText(str(data['h_f']))
        ui.lineedit_h_f.editingFinished.connect(make_handler(ui.lineedit_h_f, 'h_f'))
        if self.density_enabled:
            density = data.get('density')
            ui.lineedit_density.setText('' if density is None else str(density))
            handler = make_handler(ui.lineedit_density, 'density')
            ui.lineedit_density.editingFinished.connect(handler)

        tw = ui.tablewidget_params
        tw.setCellWidget(0, 0, make_item(data['tmin'], key='tmin'))
        tw.setCellWidget(0, 1, make_item(data['tmax'], key='tmax'))
        for (i, x) in enumerate(data['a_low']):
            tw.setCellWidget(i+1, 0, make_item(x, key=('a_low', i)))
        for (i, x) in enumerate(data['a_high']):
            tw.setCellWidget(i+1, 1, make_item(x, key=('a_high', i)))


    def enable_density(self, enabled):
        self.density_enabled = enabled
        ui = self.ui
        if not enabled:
            if ui.lineedit_density in self.species_panel_items:
                self.species_panel_items.remove(ui.lineedit_density)
                for widget in (ui.label_density, ui.label_density_units, ui.lineedit_density):
                    widget.setEnabled(False)
            ui.lineedit_density.clear()
        else:
            self.species_panel_items.append(ui.lineedit_density)


    def handle_defined_species_selection(self):
        self.ui.tablewidget_search.clearSelection()
        tw = self.tablewidget_defined_species
        row = get_selected_row(tw)

        if row is None:
            self.current_species = None
            self.clear_species_panel()
            self.ui.pushbutton_copy.setEnabled(False)
            self.ui.combobox_phase.setEnabled(False)
        else:
            self.ui.pushbutton_copy.setEnabled(True)
            self.current_species = tw.item(row, 0).text()
            self.enable_species_panel()


    def make_alias(self, species):
        alias = self.parent().species_make_alias(species)
        count = 1
        while alias in self.defined_species: # Append numeric suffix as needed
            if '_' in species and species.split('_')[-1].isdigit():
                species = species[:species.rindex('_')] # Strip old suffix
            alias = self.parent().species_make_alias('%s_%s' % (species, count))
            count += 1
        return alias



    def make_user_species_name(self):
        n = 1
        while "Species_%d" % n in self.user_species_names:
            n += 1
        name = "Species_%d" % n
        self.user_species_names.add(name)
        return name


    def do_import(self):
        rows = get_selected_rows(self.tablewidget_search)
        for row in rows:
            self.do_import_row(row)


    def do_import_row(self, row):
        self.ui.combobox_phase.setEnabled(False)
        rowdata = self.search_results[row]
        key, phase = rowdata
        data = self.db[phase][key]
        (species, tmin, tmax) = key
        (coeffs, mol_weight, _) = data

        if species in self.defined_species:
            return # Don't allow duplicates in tmp list (?)

        alias = self.make_alias(species)

        a_high = coeffs[:7]
        a_low = coeffs[7:14]
        h_f = coeffs[14]

        species_data = {
            'phase': phase,
            'alias': alias, # == species
            'mol_weight': mol_weight,
            'h_f': h_f,
            'tmin':  tmin,
            'tmax': tmax,
            'a_low': a_low,
            'a_high': a_high,
            'burcat': key[0], # Save this as comment in THERMO DATA
        }

        if self.density_enabled:
            species_data['density'] = None # ? where do we get this?
        self.defined_species[alias] = species_data
        self.add_defined_species_row(alias, select=True)
        self.set_ok_button(True) # FIXME, don't do this until something has changed


    def update_defined_species(self):
        self.tablewidget_defined_species.clearSelection()
        self.tablewidget_defined_species.setRowCount(0)
        for species_key in self.defined_species.keys():
            self.add_defined_species_row(species_key, select=False)


    def add_defined_species_row(self, species, select=False):
        species_data = self.defined_species[species]
        ui = self.ui
        tw = ui.tablewidget_defined_species
        nrows = tw.rowCount()
        tw.setRowCount(nrows+1)
        alias = species_data['alias']
        assert alias == species
        phase = species_data['phase']
        item = QTableWidgetItem(alias)
        set_item_noedit(item)
        tw.setItem(nrows, 0, item)
        item = QTableWidgetItem(phase)
        set_item_noedit(item)
        tw.setItem(nrows, 1, item)

        if select:
            tw.setCurrentCell(nrows, 0) # Cause the new row to be selected


    def handle_copy(self):
        tw = self.ui.tablewidget_defined_species
        row = get_selected_row(tw)
        if row is None:
            return
        species = tw.item(row, 0).text()
        alias = self.make_alias(species)
        if species not in self.defined_species:
            return
        species_data = deepcopy(self.defined_species[species])
        species_data['alias'] = alias
        species = alias
        self.defined_species[species] = species_data
        self.current_species = species
        self.enable_species_panel()
        self.add_defined_species_row(alias, select=True)
        lineedit = self.ui.lineedit_alias
        lineedit.selectAll()
        lineedit.setFocus()
        self.ui.combobox_phase.setEnabled(True)


    def handle_new(self):
        phase = self.default_phase
        alias = species = self.make_user_species_name()
        mol_weight = 0
        density = None
        h_f = 0
        tmin = 200.0 # ?
        tmax = 600.0 # ?
        a_low = [0.0]*7
        a_high = [0.0]*7

        species_data = {'phase': phase,
                        'alias': alias,
                        'mol_weight': mol_weight,
                        'density': density,
                        'h_f': h_f,
                        'tmin':  tmin,
                        'tmax': tmax,
                        'a_low': a_low,
                        'a_high': a_high}

        self.defined_species[species] = species_data
        self.current_species = species
        self.enable_species_panel()
        self.add_defined_species_row(alias, select=True)
        lineedit = self.ui.lineedit_alias
        lineedit.selectAll()
        lineedit.setFocus()
        self.ui.combobox_phase.setEnabled(True)

    def handle_alias(self):
        val = self.ui.lineedit_alias.text() # Already validated (?)
        tw = self.ui.tablewidget_defined_species
        row = get_selected_row(tw)
        if row is None: # No selection
            return
        #note, making a new item here, instead of changing item inplace
        item = QTableWidgetItem(val)
        set_item_noedit(item)
        tw.setItem(row, 0, item)
        defined_species = OrderedDict()
        for (key, data) in self.defined_species.items():
            if key == self.current_species:
                key = val
                data['alias'] = val
            defined_species[key] = data
        self.current_species = val
        self.defined_species = defined_species

    def set_ok_button(self, state, msg=''):
        self.ui.pushbutton_ok.setEnabled(state)
        self.ui.label_status.setText(msg)

    def handle_combobox_phase(self, index):
        phase = 'GLSC'[index]
        if not self.current_species:
            return
        species = self.defined_species[self.current_species]
        species['phase'] = phase
        self.ui.combobox_phase.setToolTip(phase_names[index])

    def reset_signals(self):
        for sig in (self.cancel, self.save):
            try:
                sig.disconnect()
            except:
                pass

    def handle_phase(self):
        phases = ''
        for phase in 'GLSC':
            button = getattr(self.ui, 'pushbutton_%s' % phase)
            if button.isChecked():
                phases += phase
        if phases == self.phases:
            return
        self.phases = phases
        self.default_phase = phases[0] if phases else ''
        self.do_search(self.ui.lineedit_search.text())

    def __init__(self, app, parent=None, phases='GLCS'):
        super(SpeciesPopup, self).__init__(parent)
        self.app = app
        self.phases = phases
        self.include_comments = False
        self.default_phase = phases[0] if phases else ''
        self.density_enabled = True
        datadir = os.path.abspath(os.path.dirname(__file__))
        self.load_burcat(os.path.join(datadir, 'burcat.pickle'))
        ui = self.ui = get_ui('species_popup.ui', self)

        # key=species, val=data tuple.  can add phase to key if needed
        self.defined_species = OrderedDict()
        self.extra_aliases = set() # To support enforcing uniqueness
        self.mfix_keywords = set(k.lower() for k in self.parent().keyword_doc.keys())
        self.mfix_fortran_globals = set() # TODO: implement this
        # uniqueness of aliases across all phases

        self.search_results = []
        self.user_species_names = set()

        # Set up UI
        ui.lineedit_search.textChanged.connect(self.do_search)
        ui.pushbutton_import.clicked.connect(self.do_import)
        ui.pushbutton_import.setEnabled(False)
        ui.tablewidget_search.itemSelectionChanged.connect(
            self.handle_search_selection)
        ui.tablewidget_defined_species.itemSelectionChanged.connect(
            self.handle_defined_species_selection)

        ui.pushbutton_new.clicked.connect(self.handle_new)
        ui.pushbutton_copy.clicked.connect(self.handle_copy)
        ui.checkbox_include_comments.clicked.connect(self.handle_include_comments)

        for phase in 'GLSC':
            button = getattr(self.ui, 'pushbutton_%s' % phase)
            button.clicked.connect(self.handle_phase)

        cb = ui.combobox_phase
        cb.currentIndexChanged.connect(self.handle_combobox_phase)
        for i,t in enumerate(phase_names):
            get_combobox_item(cb, i).setToolTip(t)

        #http://stackoverflow.com/questions/15845487/how-do-i-prevent-the-enter-key-from-closing-my-qdialog-qt-4-8-1
        # Do not use buttonbox.  https://mfix.netl.doe.gov/gitlab/develop/mfix/issues/101
        buttons = (ui.pushbutton_ok, ui.pushbutton_cancel)
        buttons[0].clicked.connect(lambda: (self.save.emit(), self.close()))
        buttons[1].clicked.connect(lambda: (self.cancel.emit(), self.close()))

        class AliasValidator(QValidator):

            # Make sure aliases are unique
            def __init__(self, parent=None):
                super(AliasValidator, self).__init__()
                self.parent = parent

            def validate(self, text, pos):
                if text == "":
                    self.parent.set_ok_button(False)
                    return (QValidator.Intermediate, text, pos)
                dig_start = text[0].isdigit()
                und_start = text[0] == '_'
                not_alphanum_und = not all(c.isalnum() or c == '_' for c in text)
                if dig_start or und_start or not_alphanum_und:
                    return (QValidator.Invalid, text, pos)
                current_species = self.parent.current_species
                if current_species not in self.parent.defined_species: # Why would this happen?
                    self.parent.set_ok_button(False)
                    return (QValidator.Invalid, text, pos)
                current_alias = self.parent.defined_species[current_species].get('alias')
                if current_alias is None:
                    self.parent.set_ok_button(False)
                    return (QValidator.Invalid, text, pos)
                for v in self.parent.defined_species.values():
                    v_alias = v.get('alias', '')
                    if v_alias.lower() == current_alias.lower(): # Skip selected item
                        continue
                    if v_alias.lower() == text.lower():
                        self.parent.set_ok_button(False, 'Alias must be unique')
                        return (QValidator.Intermediate, text, pos)
                tlower = text.lower()
                if tlower in self.parent.extra_aliases:
                    self.parent.set_ok_button(False, 'Alias must be unique')
                    return (QValidator.Intermediate, text, pos)
                if tlower in self.parent.mfix_keywords:
                    self.parent.set_ok_button(False, '%s is an MFiX keyword'%text)
                    return (QValidator.Intermediate, text, pos)
                self.parent.set_ok_button(True)
                return (QValidator.Acceptable, text, pos)

        lineedit = ui.lineedit_alias
        lineedit.setValidator(AliasValidator(parent=self))
        lineedit.editingFinished.connect(self.handle_alias)

        for line_edit in (ui.lineedit_mol_weight,
                          ui.lineedit_h_f,
                          ui.lineedit_density):
            line_edit.setValidator(QDoubleValidator())

        self.species_panel_items = [ui.label_species,
                                    ui.lineedit_alias,
                                    ui.lineedit_mol_weight,
                                    ui.lineedit_h_f,
                                    ui.lineedit_density,
                                    ui.tablewidget_params]

        hv = QHeaderView
        for tw in (self.tablewidget_search, self.tablewidget_defined_species):
            resize_column(tw, 0, hv.Stretch)
            resize_column(tw, 1, hv.ResizeToContents)
        tw = self.tablewidget_params
        for i in (0, 1):
            resize_column(tw, i, hv.Stretch)

        self.set_ok_button(False) # nothing to accept
        self.clear_species_panel()


    def set_phases(self, phases):
        if phases == self.phases:
            return
        self.phases = phases
        for phase in 'GLSC':
            button = getattr(self.ui, 'pushbutton_%s' % phase)
            button.setChecked(phase in phases)
        self.default_phase = phases[0] if phases else ''
        self.do_search(self.ui.lineedit_search.text())


    def popup(self):
        self.show()
        self.raise_()
        self.activateWindow()


def main():
    args = sys.argv
    qapp = QApplication(args)
    dialog = QDialog()
    species_popup = SpeciesPopup(dialog, phases='GL')
    species_popup.show()
    # exit with Ctrl-C at the terminal
    signal.signal(signal.SIGINT, signal.SIG_DFL)

    qapp.exec_()
    qapp.deleteLater()

    sys.exit()

if __name__ == '__main__':
    main()
