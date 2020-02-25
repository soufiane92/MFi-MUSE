# -*- coding: utf-8 -*-
from collections import OrderedDict
from itertools import groupby

from qtpy import QtCore, QtWidgets

from mfixgui.tools import sort_dict, safe_float, safe_int

MESH_EXTENT_KEYS = ['x_min', 'x_max', 'y_min', 'y_max', 'z_min', 'z_max']
MESH_CELL_KEYS = ['imax', 'jmax', 'kmax']
TABLE_MFIXKEY_MAP = {'position': 'cp', 'cells': 'nc', 'stretch': 'er',
                     'first': 'first_d', 'last': 'last_d'}
CELL_MFIX_KEYS = ['imax', 'jmax', 'kmax']


def ctrl_pts_to_loc(ctrl, min_loc):
    """given control points, convert to location"""
    loc = [min_loc]
    last = min_loc
    for pt in ctrl.values():
        er = safe_float(pt['stretch'], 1.0)
        cp = safe_float(pt['position'], 0.0)
        nc = safe_int(pt['cells'], 1)

        # uniform dx
        dx = (cp-last)/nc

        # first dx?
        fdx = safe_float(pt['first'], 0.0)
        if fdx < 0 and len(loc) > 2:
            fdx = loc[-1] - loc[-2]

        # expansion ratio
        ratio = 1
        prev_cell_w = dx
        if nc > 1 and er != 1:
            ratio = er**(1/(nc-1))
            prev_cell_w = (cp-loc[-1])*(1-ratio)/(1-ratio**nc)  # current cell width
            prev_cell_w /= ratio  # backup one cell for the loop below

        # add cell positions to list
        for i in range(nc):
            cell_w = dx
            if er != 1:
                cell_w = prev_cell_w*ratio
            loc.append(loc[-1] + cell_w)
            prev_cell_w = cell_w
        last = loc[-1]

    return loc


def linspace(f, t, c):
    """copy of numpy's linspace"""
    if int(c) <= 1:
        return [f, t]
    dx = (t-f)/float(c)
    l = [f]
    for i in range(c):
        l.append(l[-1]+dx)
    l[-1] = t # make sure the last value is the one given
    return l


def build_context_menu(actions):
    """given a list of (text, method), generate a menu"""
    menu = QtWidgets.QMenu()
    for s, method in actions:
        action = QtWidgets.QAction(s, menu)
        action.triggered.connect(method)
        menu.addAction(action)
    return menu


class KeyHandler(QtCore.QObject):
    value_updated = QtCore.Signal(object, object, object)
    update_mesh_extents = QtCore.Signal(object, object, object)
    update_mesh_cells = QtCore.Signal(object, object, object)
    def updateValue(self, key, val, args):
        if key in MESH_EXTENT_KEYS:
            self.update_mesh_extents.emit(key, val, args)
        elif key in MESH_CELL_KEYS + ['no_k']:
            self.update_mesh_cells.emit(key, val, args)


class Mesh(object):
    # Mesh Task Pane Window: This section allows a user to define the mesh
    # Methods that need vtk are in the vtkwidget
    def init_mesh(self):
        self.mesh_extents = []
        self.mesh_cells = []
        self.mesh_spacing = [[],[],[]] # x, y, z
        ui = self.ui.mesh
        self.cell_spacing_widgets = [ui.lineedit_mesh_cells_size_x,
                                     ui.lineedit_mesh_cells_size_y,
                                     ui.lineedit_mesh_cells_size_z]
        self.mesh_tables = [ui.table_mesh_control_points_x,
                            ui.table_mesh_control_points_y,
                            ui.table_mesh_control_points_z]
        self.cell_count_widgets = [ui.lineedit_keyword_imax,
                                   ui.lineedit_keyword_jmax,
                                   ui.lineedit_keyword_kmax]
        self.mesh_delete_btns = [ui.toolbutton_mesh_remove_x,
                                 ui.toolbutton_mesh_remove_y,
                                 ui.toolbutton_mesh_remove_z]
        self.mesh_add_btns = [ui.toolbutton_mesh_add_x,
                              ui.toolbutton_mesh_add_y,
                              ui.toolbutton_mesh_add_z]

        # connect delete btns
        for (i, btn) in enumerate(self.mesh_delete_btns):
            btn.clicked.connect(lambda ignore, i=i: self.mesh_delete(i))

        # connect add btns
        for (i, btn) in enumerate(self.mesh_add_btns):
            btn.clicked.connect(lambda ignore, i=i: self.mesh_add(i))

        # key handler
        self.mesh_key_handler = KeyHandler()
        self.project.register_widget(self.mesh_key_handler, MESH_EXTENT_KEYS + MESH_CELL_KEYS + ['no_k'], [])
        self.mesh_key_handler.update_mesh_extents.connect(self.update_background_mesh_extents)
        self.mesh_key_handler.update_mesh_cells.connect(self.update_background_mesh_cells)

        # connect mesh tab btns
        for (i, btn) in enumerate([ui.pushbutton_mesh_uniform, ui.pushbutton_mesh_mesher]):
            btn.clicked.connect(lambda ignore, i=i, btn=btn:self.change_mesh_tab(i, btn))

        ui.combobox_mesher.currentIndexChanged.connect(
            self.change_mesher_options)

        column_delegate = {0: {'widget': 'lineedit',
                               'dtype':  'dp'},
                           1: {'widget': 'lineedit',
                               'dtype':  'i'},
                           2: {'widget': 'lineedit',
                               'dtype':  'dp'},
                           3: {'widget': 'lineedit',
                               'dtype':  'dp'},
                           4: {'widget': 'lineedit',
                               'dtype':  'dp'},
                           }

        # setup tables
        for (i, (d, table)) in enumerate(zip(['x', 'y', 'z'], self.mesh_tables)):
            table.dtype = OrderedDict
            table._setModel() # FIXME: Should be in __init__
            table.set_selection_model()
            table.set_value(OrderedDict())
            table.set_columns(['position', 'cells', 'stretch', 'first', 'last'])
            table.show_vertical_header(True)
            table.auto_update_rows(True)
            table.new_selection.connect(lambda f, t, i=i, d=d, table=table:
                                        self.mesh_new_selection(f, t, i, d, table))
#            table.clicked.connect(self.cell_clicked)
            table.default_value = OrderedDict()
            table.value_changed.connect(lambda row, col, val, d=d, t=table:
                                        self.mesh_table_changed(row, col, val, d, t))
#            table.fit_to_contents()
            table.menu = build_context_menu([
                ('split', lambda ignore, t=table, d=d: self.mesh_split(t, d))])
            table.auto_update_rows(True)
            table.set_delegate(col=column_delegate, row=None)

    def change_mesh_tab(self, tabnum, btn):
        """switch mesh stacked widget based on selected"""
        ui = self.ui.mesh
        btn_layout=ui.gridlayout_mesh_tab_btns
        for i in range(btn_layout.columnCount()):
            item = btn_layout.itemAtPosition(0, i)
            if not item:
                continue
            widget = item.widget()
            if not widget:
                continue
            font = widget.font()
            font.setBold(i==tabnum)
            widget.setFont(font)

        self.animate_stacked_widget(
            ui.stackedwidget_mesh,
            ui.stackedwidget_mesh.currentIndex(),
            tabnum,
            direction='horizontal',
            line=ui.line_mesh,
            to_btn=btn,
            btn_layout=ui.gridlayout_mesh_tab_btns,
            )

    def change_mesher_options(self):
        """switch the mesh options stacked widget"""
        ui = self.ui.mesh
        mesher = str(ui.combobox_mesher.currentText()).lower()

        current_index = 0
        for i in range(ui.stackedwidget_mesher_options.count()):
            widget = ui.stackedwidget_mesher_options.widget(i)
            if mesher == str(widget.objectName()).lower():
                current_index = i
                break

        self.animate_stacked_widget(
            ui.stackedwidget_mesher_options,
            ui.stackedwidget_mesher_options.currentIndex(),
            current_index,
            direction='horizontal',
            )

        enable = mesher != 'cutcell'
        ui.pushbutton_generate_mesh.setEnabled(enable)
        ui.pushbutton_remove_mesh.setEnabled(enable)

    def mesh_new_selection(self, from_, to, index, dir_, table):
        """handle a new selection"""
        self.mesh_delete_btns[index].setEnabled(len(to)>0)

    def update_mesh_keyword(self, key, value, args=None):
        """check the keywords and correct the index"""
        # erx, ery, erz, ncx, ncy, ncz, first_dx, first_dy, first_dz, last_dx, last_dy, last_dz all start at 1
        if args is not None and key[:-1] != 'cp':
            args += 1
        self.update_keyword(key, value, args)

    def init_background_mesh(self):
        """init the background mesh"""
        project = self.project

        self.mesh_extents = [safe_float(project.get_value(key, default=0.0))
                             for key in MESH_EXTENT_KEYS]
        self.mesh_cells = [safe_int(project.get_value(key, default=1))
                           for key in MESH_CELL_KEYS]
        self.mesh_spacing = [[], [], []]

        # disable delete btns
        for btn in self.mesh_delete_btns:
            btn.setEnabled(False)

        # collect dx, dy, dz
        for (i, (s, c)) in enumerate(zip(['dx', 'dy', 'dz'], MESH_CELL_KEYS)):
            d = [project.get_value(s, args=args) for args in sorted(project.get_key_indices(s))]
            l = len(d)

            # if there are spacing, update the keywords.
            if l > 0:
                self.mesh_cells[i] = l
                self.update_mesh_keyword(c, l)
                self.mesh_spacing[i] = d
                self.extract_mesh_spacing(i, d)

        # collect variable grid spacing keywords
        for (i, k) in enumerate(['x', 'y', 'z']):
            indices = project.get_key_indices('cp' + k)
            if indices:
                table_dic = OrderedDict()
                for j, ind in enumerate(sorted(indices)):
                    ind = ind[0]
                    table_dic[j] = {
                        'position': project.get_value('cp' + k, 0, args=ind),
                        'cells': project.get_value('nc' + k, 1, args=ind+1),
                        'stretch': project.get_value('er' + k, 1, args=ind+1),
                        'first': project.get_value('first_d' + k, 0, args=ind+1),
                        'last': project.get_value('last_d' + k, 0, args=ind+1)}
                self.mesh_tables[i].set_value(table_dic)
                self.mesh_tables[i].fit_to_contents()

        self.update_background_mesh()

    def extract_mesh_spacing(self, index, spacing):
        """given a list of cell spacing, convert to control points,
        also update [xyz]_max"""

        start = 0
        table_dic = OrderedDict()
        d = ['x', 'y', 'z'][index]

        for (i, (val, count)) in enumerate((k, sum(1 for _i in g))
                                         for (k,g) in groupby(spacing)):
            loc = count*val + start
            start = loc
            self.update_mesh_keyword('cp' + d, loc, args=i)
            self.update_mesh_keyword('nc' + d, count, args=i)
            table_dic[i] = {'position': loc, 'cells': count, 'stretch': 1.0,
                            'first': 0.0, 'last': 0.0}
        #Use location of final control point for [xyz]_max
        self.mesh_extents[index*2+1] = loc
        self.update_mesh_keyword(MESH_EXTENT_KEYS[1::2][index], loc)

        #Unset d[xyz] keys
        for i in range(len(spacing)):
            self.unset_keyword('d' + d, args=i)

        self.mesh_tables[index].set_value(table_dic)
        self.mesh_tables[index].fit_to_contents()

    def mesh_table_changed(self, row, col, val, d, table):
        """a value in the table was edited, update"""
        data = table.value
        if col == 'position':
            sort = sort_dict(data, 'position')
            table.set_value(sort, block=False) # unblock because the table is currently in an "edit" state
            table.fit_to_contents()
            for i, val in sort.items():
                self.mesh_update_mfixkeys(val, i, d)
        else:
            mfix_key = TABLE_MFIXKEY_MAP[col] + d

            self.update_mesh_keyword(mfix_key, val, args=row)

        self.update_background_mesh()

    def mesh_delete(self, index):
        """delete the selected control point"""
        table = self.mesh_tables[index]
        data = table.value
        rows = table.current_rows()
        if not rows: return
        max_i = max(data.keys())
        min_row = min(rows)
        d = ['x', 'y', 'z'][index]

        # remove rows
        for row in rows:
            data.pop(row)

        # rebuild dict
        # TODO: better way?
        new = OrderedDict()
        for (i, ctrl) in enumerate(data.values()):
            new[i] = ctrl
            if i >= min_row:
                self.mesh_update_mfixkeys(ctrl, i, d)
        table.set_value(new)
        table.fit_to_contents()
        self.update_background_mesh()

        nrows = len(new)
        if rows[-1] == nrows: # We deleted the last row,
            if nrows > 0:
                table.selectRow(nrows-1)

        # remove trailing keywords
        k = new.keys()
        if k:
            m = max(k)
        else:
            m = 0
        for i in range(m, max_i):
            ind = i + 1
            for key in TABLE_MFIXKEY_MAP.values():
                self.update_mesh_keyword(key+d, None, args=ind)

        # the last control point
        if m == max_i == 0:
            for key in TABLE_MFIXKEY_MAP.values():
                self.update_mesh_keyword(key+d, None, args=0)

    def mesh_add(self, index):
        """add a control point to the end"""
        table = self.mesh_tables[index]
        data = table.value
        d = ['x', 'y', 'z'][index]
        k = data.keys()
        i = 0
        if k:
            i = max(k) + 1
            loc = safe_float(list(data.values())[-1]['position']) + 1
            c = 1
        else:
            loc = self.project.get_value(d + '_max', 1)
            c = self.project.get_value(CELL_MFIX_KEYS[index], 1)
        ctrl = data[i] = {'position': loc, 'cells': c, 'stretch': 1.0, 'first': 0.0, 'last': 0.0}

        self.mesh_update_mfixkeys(ctrl, i, d)
        table.set_value(data)
        table.fit_to_contents()
        self.update_background_mesh()

    def mesh_split(self, table, d):
        """split the selected control point"""
        row, col = table.get_clicked_cell()
        data = table.value
        split_data = data[row]
        prev_data_loc = self.project.get_value(d + '_min', 0)
        if row >= 2:
            prev_data_loc = safe_float(data[row-1]['position'])

        # find the midpoint, and slit the cells evenly
        midpoint = (safe_float(split_data['position']) - prev_data_loc)/2.0 + prev_data_loc
        cells = max(int(safe_int(split_data['cells'], 1)/2), 1)
        split_data['cells'] = cells

        # insert the cell
        # TODO: better way?
        new = OrderedDict()
        for i, ctrl in data.items():
            if i < row:
                new[i] = ctrl
            elif i == row:
                new[i] = {'position': midpoint, 'cells': cells, 'stretch': 1.0,
                          'first': 0.0, 'last': 0.0}
                self.mesh_update_mfixkeys(new[i], i, d)
                new[i+1] = ctrl
                self.mesh_update_mfixkeys(ctrl, i+1, d)
            else:
                new[i+1] = ctrl
                self.mesh_update_mfixkeys(ctrl, i+1, d)

        table.set_value(new)
        table.fit_to_contents()
        self.update_background_mesh()

    def mesh_update_mfixkeys(self, ctrl, index, dir_):
        for key, value in ctrl.items():
            mfix_key = TABLE_MFIXKEY_MAP[key] + dir_
            self.update_mesh_keyword(mfix_key, value, args=index)

    def update_background_mesh_cells(self, key, val, args):
        """collect cells changes, check if value is different"""
        if not self.mesh_cells: return
        if key == 'no_k':
            self.update_background_mesh()
        else:
            val = safe_int(val, 1)
            ind = MESH_CELL_KEYS.index(key)
            old_val = self.mesh_cells[ind]
            if old_val != val:
                self.mesh_cells[ind] = val
                self.update_background_mesh()

    def update_background_mesh_extents(self, key, val, args):
        """collect extents changes, check if value is different"""
        if not self.mesh_extents: return
        val = safe_float(val, 0)
        ind = MESH_EXTENT_KEYS.index(key)
        old_val = self.mesh_extents[ind]
        if old_val != val:
            self.mesh_extents[ind] = val
            self.update_background_mesh()

    def update_background_mesh(self):
        """update the background mesh"""
        extents = self.mesh_extents
        cells = self.mesh_cells
        project = self.project

        # average cell width
        for (f, t), c, wid in zip(zip(extents[::2], extents[1::2]), cells, self.cell_spacing_widgets):
            if c > 0:
                w = (t-f)/c
            else:
                w = t-f
            wid.setText('{0:.2e}'.format(w))

        # determine cell spacing
        spacing = []
        for (i, table) in enumerate(self.mesh_tables):
            axis='xyz'[i]
            val = table.value
            if val:
                s = ctrl_pts_to_loc(val, project.get_value('%s_min'%axis, 0))
                spacing.append(s)
                # disable imax, jmax, kmax
                self.cell_count_widgets[i].setEnabled(False)
                # update imax, jmax, kmax
                self.update_mesh_keyword(CELL_MFIX_KEYS[i], len(s)-1) # TODO: fix redundant update calls
            else:
                spacing.append(linspace(extents[i*2], extents[i*2+1], cells[i]))
                # enable imax, jmax, kmax
                if i == 2:
                    self.cell_count_widgets[i].setEnabled(not project.get_value('no_k', False))
                else:
                    self.cell_count_widgets[i].setEnabled(True)

        self.vtkwidget.update_background_mesh(spacing)
