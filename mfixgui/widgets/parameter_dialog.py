#!/usr/bin/env python
"""Parameter Dialog and management of the PARAMETER_DICT constant"""

from collections import OrderedDict
from qtpy import QtWidgets, QtCore, QtGui
import copy

from mfixgui.constants import *
from mfixgui.project import Equation
from mfixgui.regexes import re_math
from mfixgui.tools import get_unique_string, parse_key_with_args
from mfixgui.tools.qt import get_icon, sub_icon_size

from mfixgui.tools.simpleeval import DEFAULT_FUNCTIONS, DEFAULT_NAMES
from mfixgui.widgets.base import CustomDelegate

PROTECTED_NAMES = list(DEFAULT_FUNCTIONS.keys()) + list(DEFAULT_NAMES.keys()) + SPECIAL_PARAMETERS

class ParameterDialog(QtWidgets.QDialog):

    def __init__(self, parent):
        QtWidgets.QDialog.__init__(self, parent)

        self.data_old = {}

        self.setWindowIcon(get_icon('mfix.png'))
        self.setWindowTitle('Parameters')

        self.grid_layout = QtWidgets.QGridLayout(self)
        self.grid_layout.setContentsMargins(5, 5, 5, 5)

        # --- tool buttons ---
        self.button_bar = QtWidgets.QWidget(self)
        self.button_bar_layout = QtWidgets.QHBoxLayout(self.button_bar)
        self.button_bar_layout.setContentsMargins(0, 0, 0, 0)
        self.button_bar.setLayout(self.button_bar_layout)
        self.button_bar.setGeometry(QtCore.QRect(0, 0, 300, 300))
        self.grid_layout.addWidget(self.button_bar, 0, 0)

        self.toolbutton_add = QtWidgets.QToolButton()
        self.toolbutton_add.clicked.connect(self.new_parameter)
        self.toolbutton_add.setIcon(get_icon('add.svg'))
        self.toolbutton_add.setIconSize(sub_icon_size())

        self.toolbutton_remove = QtWidgets.QToolButton()
        self.toolbutton_remove.clicked.connect(self.remove_parameter)
        self.toolbutton_remove.setIcon(get_icon('remove.svg'))
        self.toolbutton_remove.setEnabled(False)
        self.toolbutton_remove.setIconSize(sub_icon_size())

        self.toolbutton_copy = QtWidgets.QToolButton()
        self.toolbutton_copy.clicked.connect(self.copy_parameter)
        self.toolbutton_copy.setIcon(get_icon('copy.svg'))
        self.toolbutton_copy.setEnabled(False)
        self.toolbutton_copy.setIconSize(sub_icon_size())

        for widget in [self.toolbutton_add, self.toolbutton_remove,
                       self.toolbutton_copy]:
            self.button_bar_layout.addWidget(widget)
            widget.setAutoRaise(True)

        self.button_bar_layout.addStretch()

        # --- tree ---
        delegate = CustomDelegate(
            column_dict={0: {'widget': 'lineedit',
                             },
                         1: {'widget': 'combobox',
                             'items':  ['integer', 'float', 'string'],
                             },
                         2: {'widget': 'lineedit',
                             },
                         })
        self.tree = QtWidgets.QTreeWidget()
        self.tree.setItemDelegate(delegate)
        self.tree.setColumnCount(3)
        self.tree.setHeaderLabels(['parameter', 'type', 'value'])
        self.tree.setMinimumWidth(400)
        self.tree.itemSelectionChanged.connect(self.tree_clicked)
        self.tree.itemChanged.connect(self.parameter_changed)

        # base items
        self.units_parent = QtWidgets.QTreeWidgetItem(['units'])
        self.model_parent = QtWidgets.QTreeWidgetItem(['model'])
        self.user_parent = QtWidgets.QTreeWidgetItem(['user'])

        for item in [self.units_parent, self.model_parent, self.user_parent]:
            self.tree.addTopLevelItem(item)

        self.grid_layout.addWidget(self.tree, 1, 0)

        # --- apply/close ---
        btn_widget = QtWidgets.QWidget()
        btn_widget_ly = QtWidgets.QHBoxLayout(btn_widget)
        btn_widget_ly.setContentsMargins(0, 0, 0, 0)
        self.grid_layout.addWidget(btn_widget, 2, 0)

        spacer = QtWidgets.QSpacerItem(100, 10, QtWidgets.QSizePolicy.Expanding,
                                       QtWidgets.QSizePolicy.Maximum)
        btn_widget_ly.addItem(spacer)

        self.pushbutton_close = QtWidgets.QPushButton('Close')
        self.pushbutton_close.clicked.connect(self.close)
        btn_widget_ly.addWidget(self.pushbutton_close)


    def update_table(self):
        for item in [self.units_parent, self.model_parent, self.user_parent]:
            item.takeChildren()
        for value in self.parameters_dict.values():
            self.add_item(value)

    def add_item(self, data):
        name = data.get('parameter', None)
        parent = self.user_parent
        editable = True
        if name in CONVERSION_TO_METERS:
            parent = self.units_parent
            editable = False
        elif name in SPECIAL_PARAMETERS:
            parent = self.model_parent
            editable = False

        item = QtWidgets.QTreeWidgetItem([str(data.get(k)) for k in ['parameter', 'type', 'value']])
        if editable:
            item.setFlags(item.flags() | QtCore.Qt.ItemIsEditable)
        parent.addChild(item)
        return item

    def tree_clicked(self):
        item = self.tree.currentItem()

        if item.parent() == self.user_parent:
            self.toolbutton_remove.setEnabled(True)
            self.toolbutton_copy.setEnabled(True)
        else:
            self.toolbutton_remove.setEnabled(False)
            self.toolbutton_copy.setEnabled(False)

    def new_parameter(self):
        data = self.parameters_dict
        new_name = get_unique_string('new', data.keys())

        i = data[new_name] = {'parameter': new_name, 'type': 'float',
                              'value': 0.0}

        i = self.add_item(i)
        self.tree.setCurrentItem(i)
        self.user_parent.setExpanded(True)

    def remove_parameter(self):
        items = self.tree.selectedItems()

        geo = self.parent().vtkwidget
        geo_p_map = geo.parameter_key_map
        prj_p_map = self.parent().project.parameter_key_map

        if items:
            data = self.parameters_dict
            cant_remove = []
            for item in items:
                name = str(item.text(0))
                if name in geo_p_map or name in prj_p_map:
                    cant_remove.append(name)
                else:
                    data.pop(name)
            self.update_table()
            if cant_remove:
                self.parent().message(title='Error', text='The following parameters are being used: <b>{}</b>. \nPlease remove reference before deleting.'.format(', '.join(cant_remove)))

    def copy_parameter(self):
        items = self.tree.selectedItems()

        if items:
            data = self.parameters_dict
            for item in items:
                name = str(item.text(0))

                new_name = get_unique_string(name, data.keys())

                i = data[new_name] = {'parameter': new_name,
                                      'type': copy.deepcopy(data[name]['type']),
                                      'value': copy.deepcopy(data[name]['value'])}
                i = self.add_item(i)
                self.tree.setCurrentItem(i)

    def load_parameters(self):
        self.parameters_dict = OrderedDict()
        for key in PARAMETER_DICT.keys():
            dtype = 'string'
            value = PARAMETER_DICT[key]
            if isinstance(value, float):
                dtype = 'float'
            elif isinstance(value, int):
                dtype = 'integer'
            elif isinstance(value, Equation):
                dtype = {int:'integer', float:'float'}[value.dtype]
                value = value.eq
            self.parameters_dict[key] = {'parameter': key, 'type': dtype,
                                 'value': value}

        self.update_table()
        self.user_parent.setExpanded(True)

    @property
    def parameters(self):
        param_dict = OrderedDict()
        data = self.parameters_dict
        param_names = [val['parameter'] for val in data.values()]
        for k, v in data.items():
            type_ = v['type']
            value = v['value']
            param_name = v['parameter']
            param_value = str(value)

            if (type_ in ('float', 'integer') and
                (re_math.search(param_value) or
                 any(p in param_value
                     for p in param_names))):
                param_value = Equation(param_value)
            elif type_ == 'float':
                param_value = float(value)
            elif type_ == 'integer':
                param_value = int(value)

            param_dict[param_name] = param_value

        return param_dict

    def get_parameters(self):
        self.load_parameters()
        self.changed_parameters = set()
        self.exec_()
        self.update_parameter_dict(self.parameters)
        return self.changed_parameters

    def update_parameter_dict(self, data):
        PARAMETER_DICT.update(data)

        for key in set(PARAMETER_DICT.keys()) - set(data.keys()):
            PARAMETER_DICT.pop(key)

    def collect_names(self):
        names = []
        for item in [self.units_parent, self.model_parent, self.user_parent]:
            for i in range(item.childCount()):
                names.append(str(item.child(i).text(0)))
        return names

    def parameter_changed(self, item, col):
        """parameter changed"""
        data = self.parameters_dict
        name = str(item.text(0))
        dtype = str(item.text(1))
        value = str(item.text(2))

        # check value
        if col == 2:
            self.changed_parameters.add(name)
            old_value = data[name]['value']
            new_value = self.check_value(value, old_value, dtype)
            data[name]['value'] = new_value
            item.setText(2, str(new_value))

        # check name
        elif col == 0:
            old_names = list(set(data.keys()) - set(self.collect_names()))
            if len(old_names) > 1:
                print('warning: multiple names changed:{}'.format(', '.join(old_names)))
            elif not old_names:
                return

            old_name =  old_names[0]
            new_name = self.check_name(name, old_name)
            data[old_name]['parameter'] = new_name
            data[new_name] = data.pop(old_name)
            item.setText(0, str(new_name))

            if new_name != old_name:
                self.change_parameter_name(old_name, new_name)

    def check_value(self, value, old_value, dtype):
        if dtype == 'float':
            try:
                value = float(value)
            except ValueError:
                self.parent().message(title='Error', text='The value: <b>{}</b> is not a valid float.'.format(value))
                value = old_value
        elif dtype == 'integer':
            try:
                value = int(value)
            except ValueError:
                self.parent().message(title='Error', text='The value: <b>{}</b> is not a valid integer.'.format(value))
                value = old_value
        return value

    def check_name(self, name, old_name):
        """check the parameter name"""
        param_names = list(self.parameters_dict.keys())
        param_names.remove(old_name)

        if name in PROTECTED_NAMES or name in self.parent().keyword_doc:
            self.parent().message(title='Error', text='The parameter name: <b>{}</b> is protected and cannot be used.'.format(name))
            return old_name
        elif name[0].isdigit():
            self.parent().message(title='Error', text='The parameter name: <b>{}</b> can not start with a digit.'.format(name))
            return old_name
        elif name in param_names:
            return get_unique_string(name, param_names)
        else:
            return name

    def change_parameter_name(self, old_name, new_name):
        """a parameter was renamed, update equations"""

        # update project keywords
        proj = self.parent().project
        p_map = proj.parameter_key_map

        if old_name in p_map:
            for keyword in p_map[old_name]:
                key, args = parse_key_with_args(keyword)
                eq = proj.get_value(key, default=None, args=args)
                if eq is not None and isinstance(eq, Equation):
                    eq.eq = eq.eq.replace(old_name, new_name)

            p_map[new_name] = p_map.pop(old_name)
            self.changed_parameters.add(new_name)

        # update regions
        regions = self.parent().ui.regions
        p_map = regions.parameter_key_map
        data = regions.tablewidget_regions.value

        if old_name in p_map:
            for keyword in p_map[old_name]:
                name, key = keyword.split(',')
                if 'to' in key or 'from' in key:
                    item = key.split('_')
                    index = ['x', 'y', 'z'].index(item[1])
                    val = data[name][item[0]][index]
                elif 'filter' in key:
                    item = key.split('_')
                    index = ['x', 'y', 'z'].index(item[1])
                    val = data[name][item[0]][index]
                else:
                    val = data[name][key]

                val.eq = val.eq.replace(old_name, new_name)
            p_map[new_name] = p_map.pop(old_name)
            regions.update_region_parameters()

        # update geometry
        gui = self.parent()
        geo = gui.vtkwidget
        p_map = geo.parameter_key_map
        data = geo.geometrydict

        if old_name in p_map:
            for keyword in p_map[old_name]:
                name, key = keyword.split(',')
                val = data[name][key]

                val.eq = val.eq.replace(old_name, new_name)
            p_map[new_name] = p_map.pop(old_name)
            geo.selected_geometry_changed()

        # pluck the other variable widgets
        gui.handle_ics_region_selection()
        gui.handle_bcs_region_selection()
        gui.handle_pss_region_selection()
        gui.handle_iss_region_selection()
