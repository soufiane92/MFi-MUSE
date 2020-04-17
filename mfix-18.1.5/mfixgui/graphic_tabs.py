# -*- coding: utf-8 -*-
from collections import OrderedDict
import math
import os
import json
import itertools

# graphics libraries
try:
    import pyqtgraph as pg
    from pyqtgraph import PlotWidget
    PYQTGRAPH_AVAILABLE = True
    pg.setConfigOption('background', 'w')
    pg.setConfigOption('foreground', 'k')

    # monkey patch default method to plot widget
    def plotwidget_default(cls):
        '''default method to mix in to PlotWidget'''
        cls.clear()

    PlotWidget.default = plotwidget_default

except (ImportError, RuntimeError):
    pg = None
    PYQTGRAPH_AVAILABLE = False

from qtpy import QtCore, QtWidgets

from mfixgui.tools import get_unique_string
from mfixgui.tools.qt import get_icon, clear_layout, get_ui, CellColor, get_pixmap
from mfixgui.widgets.base_vtk import VTK_AVAILABLE
from mfixgui.widgets.base import Table, ComboBox

NaN = float('NaN')


PLOT_ITEMS = OrderedDict([
    ['DT', {
        'btn_name':'pushButton_dt',
        'left':'Time Step [s]',
        'bottom':'Simulation Time [s]',
        'x_var':'time',
        'y_var':'dt'}],
    ['NIT', {
        'btn_name':'pushButton_nit',
        'left':'Number of Iterations [-]',
        'bottom':'Simulation Time [s]',
        'x_var':'time',
        'y_var':'nit'}],
    ['Elapsed Time', {
        'btn_name':'pushButton_walltime',
        'left':'Simulation Time [s]',
        'bottom':'Elapsed Wall Time [s]',
        'x_var':'walltime_elapsed',
        'y_var':'time'}],
    ['Residuals', {
        'btn_name':'pushButton_residuals',
        'left':'Residual',
        'bottom':'Iterations',
        'x_var':'nit',
        'y_var':'residuals'}],
])

TABLEAU20 = [(31, 119, 180),
             (255, 127, 14),
             (44, 160, 44),
             (214, 39, 40),
             (148, 103, 189),
             (140, 86, 75),
             (227, 119, 194),
             (127, 127, 127),
             (188, 189, 34),
             (219, 219, 141),
             (23, 190, 207),
             (158, 218, 229),
             (174, 199, 232),
             (255, 187, 120),
             (152, 223, 138),
             (255, 152, 150),
             (197, 176, 213),
             (196, 156, 148),
             (247, 182, 210),
             (199, 199, 199)
            ]


DEFAULT_PENS = [pg.mkPen(color=color, width=2)
                for color in TABLEAU20] if PYQTGRAPH_AVAILABLE else []


def is_valid(val):
    return isinstance(val, (float, int)) and math.isfinite(val)


def fix_table_spacing(tw, last_column=3):
    hv = QtWidgets.QHeaderView
    resize = tw.horizontalHeader().setSectionResizeMode
    ncols = tw.model().columnCount()
    for n in range(0, ncols):
        resize(n, hv.Stretch if n==last_column else hv.ResizeToContents)

def legend_patch(cls):
    if cls.size is not None:
        return
    height = 0
    width = 0
    for sample, label in cls.items:
        if sample.isVisible():
            height += max(sample.height(), label.height()) + 3
            width = max(width, sample.width()+label.width())
    cls.setGeometry(0, 0, width+25, height)


class BaseGraphicTab(QtWidgets.QWidget):
    """Base graphics widget, provides options to redirect to a specific
    graphic widget, i.e. plot"""
    def __init__(self, name, parent, graphic_tabs, existing_tabnames):
        QtWidgets.QWidget.__init__(self, parent)

        graphics_tab_ui = get_ui('graphics_tab.ui', self)

        self.vtk = False
        self.monitors = False

        self.x = []
        self.iteration = 0
        self.y = []
        self.name = name
        self.curve = {}
        self.graphic_tabs = graphic_tabs

        for plot_name, props in PLOT_ITEMS.items():
            plot_btn = getattr(graphics_tab_ui, props['btn_name'])
            plot_btn.setEnabled(PYQTGRAPH_AVAILABLE and plot_name not in existing_tabnames)
            plot_btn.clicked.connect(lambda _, plot_name=plot_name: self.create_plot_widget(plot_name))

        # monitor btn
        graphics_tab_ui.pushButton_monitors.setEnabled(PYQTGRAPH_AVAILABLE)
        graphics_tab_ui.pushButton_monitors.clicked.connect(lambda:self.create_monitor_plot_widget())

        vtk_btn = graphics_tab_ui.pushButton_vtk
        vtk_btn.clicked.connect(lambda: self.create_vtk_widget(load=False))
        vtk_btn.setEnabled(graphic_tabs.loadvtk and 'MFIX_NO_VTK' not in os.environ and VTK_AVAILABLE)


    def create_monitor_plot_widget(self, name='Monitors'):
        """ convert existing "New" tab to a monitor plot tab """
        layout = self.layout()

        clear_layout(layout)
        name = get_unique_string(name, self.graphic_tabs.plot_dict.keys())
        self._change_name(name)

        self.monitors = True
        self.curve = {}
        self.color_cycle = itertools.cycle(DEFAULT_PENS)

        mr = self.graphic_tabs.monitor_reader
        mr.connect(self.update_monitors)


        # splitter
        splitter = QtWidgets.QSplitter(QtCore.Qt.Vertical)
        layout.addWidget(splitter, 0, 0)

        # plot widget
        plot = self.plot_widget = PlotWidget()
        plot.getPlotItem().showGrid(True, True, 0.5)
        plot.setDownsampling(ds=False, auto=True, mode='subsample')
        plot.setLabel('bottom', 'Time [s]')

        plot.addLegend()
        # monkey patch updateSize of the legend
        legend = plot.plotItem.legend
        legend.updateSize = lambda: legend_patch(legend)
        splitter.addWidget(plot)

        # table
        opt_widget = QtWidgets.QWidget(self)
        opt_layout = QtWidgets.QGridLayout(opt_widget)
        opt_layout.setContentsMargins(0, 0, 0, 0)

        # save
        tb_sv = QtWidgets.QToolButton()
        tb_sv.setToolTip('save plot')
        tb_sv.setIcon(get_icon('save.svg'))
        tb_sv.setAutoRaise(True)
        plot.sceneObj.contextMenuItem = plot.plotItem.vb  # fix missing attr
        tb_sv.clicked.connect(plot.sceneObj.showExportDialog)
        opt_layout.addWidget(tb_sv, 0, 0)

        # hide/show legend
        tb_lg = QtWidgets.QToolButton()
        tb_lg.setToolTip('hide/show legend')
        tb_lg.setCheckable(True)
        tb_lg.setChecked(True)
        tb_lg.setIcon(get_icon('bullet_list.svg'))
        tb_lg.setAutoRaise(True)
        tb_lg.clicked.connect(lambda: legend.setVisible(tb_lg.isChecked()))
        opt_layout.addWidget(tb_lg, 0, 1)

        mt = self.monitor_table = Table(dtype=dict, columns=['visible', 'color', 'variable', 'monitor'],
                                        selection_behavior='row', multi_selection=True)
        mt.set_value(dict())
        mt.show_vertical_header(False)
        mt.auto_update_rows(True)
        mt.clicked.connect(self.cell_clicked)
        mt.setEditTriggers(QtWidgets.QTableView.NoEditTriggers)

        # right click menu
        menu = QtWidgets.QMenu(mt)
        for text, method in [('Select All', self.select_all),
                             ('Clear Selection', self.clear_selection),
                             ('Hide', self.hide_curves),
                             ('Show', self.show_curves),
                             ]:
            action = QtWidgets.QAction(text, mt)
            action.triggered.connect(method)
            menu.addAction(action)

        mt.menu = menu
        opt_layout.addWidget(mt, 1, 0, 1, 10)

        splitter.addWidget(opt_widget)
        splitter.setSizes([800, 200])

        # lb = QtWidgets.QLabel('X Axis')
        # layout.addWidget(lb, 2, 0)
        # cb = self.x_axis_cb = ComboBox()
        # cb.addItems(['Time'])
        # layout.addWidget(cb, 2, 1)

        for path in mr.data.keys():
            self.update_monitors(path, mr.data)

    def select_all(self):
        self.monitor_table.selectAll()

    def clear_selection(self):
        self.monitor_table.clear_selection()

    def hide_curves(self):
        self.set_monitor_visibility(False)

    def show_curves(self):
        self.set_monitor_visibility(True)

    def set_monitor_visibility(self, visible=True):
        mt = self.monitor_table
        data =  self.monitor_table.value
        img = self.get_visibility_image(visible)

        for row in mt.current_rows():
            name = list(data.keys())[row]
            mon = data.get(name, None)
            if mon is None:
                continue
            if name in self.curve:
                self.set_monitor_visible(name, visible)

            mon['visible'] = img
            mon['visibility'] = visible

            data.update({name: mon})
        mt.set_value(data)

    def create_plot_widget(self, name):
        """ convert existing "New" tab to Plot tab """
        clear_layout(self.layout())
        self._change_name(name)
        props = PLOT_ITEMS.get(name, None)

        if props is None:
            # monitor
            return

        plot = self.plot_widget = PlotWidget()
        plot.addLegend()
        plot.setLabel('left', props['left'])
        plot.setLabel('bottom', props['bottom'])
        plot.getPlotItem().showGrid(True, True, 0.5)
        plot.setDownsampling(ds=True, auto=True, mode='subsample')

        if 'residuals' in name.lower():
            plot.setLogMode(y=True)
            plot.setXRange(0, 100)
            plot.setAutoPan(x=True)
            plot.enableAutoRange(x=True)
            self.curve = {}
            self.y = {}
        else:
            self.curve = plot.plot([], pen=DEFAULT_PENS[0], name=name)
        self.layout().addWidget(plot)

    def create_vtk_widget(self, load):
        """ convert existing "New" tab to VTK tab """
        clear_layout(self.layout())
        name = get_unique_string('VTK', self.graphic_tabs.plot_dict.keys())
        self._change_name(name)
        from mfixgui.widgets.vtk_results_widget import GraphicsVtkWidget
        self.vtk_widget = GraphicsVtkWidget(self, load=load)
        self.layout().addWidget(self.vtk_widget)
        self.vtk = True
        return self.vtk_widget

    def update_monitors(self, path, data):
        tw = self.monitor_table
        d = tw.value
        update_table = False

        mon = os.path.basename(path)
        header = data[path].get('header', [0])
        array = data[path].get('data', [])
        t = array[:, 0]

        if len(header) < 2:
            return

        for var in header[1:]:
            key = mon+var
            idx = header.index(var)
            vals = array[:, idx]

            pen = None
            if key not in d:
                pen = next(self.color_cycle)
                d[key] = {'visible': self.get_visibility_image(),
                          'visibility': True,
                          'variable': var, 'monitor':mon,
                          'color': CellColor(pen.color()),
                          'pen': pen}
                update_table = True

            if key not in self.curve:
                pen = d[key].get('pen', next(self.color_cycle))
                self.curve[key] = self.plot_widget.plot([], pen=pen, name=var)

            vis = d[key].get('visibility', True)
            if len(t) > 1 and len(vals) == len(t):
                curve = self.curve[key]
                try:
                    curve.setData(t, vals, connect='finite')
                except (ValueError, OverflowError):
                    pass
                self.set_monitor_visible(key, vis)

        if update_table:
            tw.set_value(d)
            fix_table_spacing(tw)

    def set_monitor_visible(self, key, vis=True):

        curve = self.curve.get(key, None)
        if curve is None or curve.isVisible() == vis:
            return
        curve.setVisible(vis)

        legend = self.plot_widget.plotItem.legend
        for item in legend.items:
            if item[0].item == curve:
                [obj.setVisible(vis) for obj in item]
        legend.updateSize()


    def get_visibility_image(self, visible=True):
        return get_pixmap('visibility.svg' if visible else 'visibilityofflight.svg',
                          16, 16)

    def cell_clicked(self, index):
        data = self.monitor_table.value
        name = list(data.keys())[index.row()]
        mon =  data.get(name)
        if mon is None:
            return
        update = False

        # curve visibility
        if index.column() == 0:
            vis = mon['visibility'] = not mon['visibility']
            if name in self.curve:
                self.set_monitor_visible(name, vis)

            mon['visible'] = self.get_visibility_image(vis)
            update = True

        # curve color
        elif index.column() == 1:
            init_color =  mon.get('color', None)
            if init_color is None:
                init_color = QtCore.Qt.White
            else:
                init_color = init_color.color
            color = QtWidgets.QColorDialog.getColor(init_color, parent=self, title='Select plot color')
            if color.isValid():
                c = mon['color'] = CellColor(color)
                pen = mon['pen'] = pg.mkPen(color=c.color, width=2)
                self.curve[name].setPen(pen)
                update = True

        if update:
            data.update({name: mon})
            self.monitor_table.set_value(data)

    def get_monitor_state(self):
        d = {}
        data = self.monitor_table.value

        mr_data = self.graphic_tabs.monitor_reader.data
        valid_keys = []
        for key in mr_data.keys():
            mon = os.path.basename(key)
            for var in mr_data[key].get('header', []):
                valid_keys.append(mon+var)

        for k in data.keys():
            sub_d = d[k] = {}
            if k not in valid_keys:
                continue
            for sub_k in data[k].keys():
                if sub_k not in ['visible', 'color', 'pen']:
                    sub_d[sub_k] = data[k][sub_k]
                elif sub_k == 'color':
                    sub_d[sub_k] = data[k][sub_k].color_hex
        return d

    def set_monitor_state(self, state):
        data  = {}
        for k in state.keys():
            sub = data[k] = {}
            for sub_k in state[k].keys():
                if sub_k == 'color':
                    c = sub[sub_k] = CellColor(state[k][sub_k])
                    sub['pen'] = pg.mkPen(color=c.color, width=2)
                elif sub_k == 'visibility':
                    vis = sub['visibility'] = state[k][sub_k]
                    sub['visible'] = self.get_visibility_image(visible=vis)
                else:
                    sub[sub_k] = state[k][sub_k]

        tw = self.monitor_table
        tw.set_value(data)
        fix_table_spacing(tw)


    def reset_plot_data(self):
        """ Delete data for a plot """
        self.plot_widget.clear()
        self.x = []
        self.iteration = 0
        if isinstance(self.y, dict):
            for key in self.y:
                self.y[key] = [[], []]
                self.plot_widget.plotItem.legend.removeItem(key)
            self.curve = {}
        else:
            self.y = []
            self.plot_widget.plotItem.legend.removeItem(self.name)
            self.curve = self.plot_widget.plot([], pen=DEFAULT_PENS[0], name=self.name)

    def plot(self, x=None, y=None, append=False):
        """update the plot"""

        # check data integrity
        if y is None:
            return
        if self.x and (x is None or y is None):
            return
        ylen = len(self.y)
        xlen = len(self.x)
        # append the data to the internal data structure
        if append:
            # append y
            if isinstance(y, (list, tuple)):
                if self.y == []:
                    self.y = {}

                # determine iteration
                if x is not None and xlen > 0 and x != self.x[-1]:
                    self.iteration += 1

                for item in y:
                    if len(item) == 2:
                        key = item[0].strip()
                        if not key:
                            continue
                        new_value = item[1]
                        if not is_valid(new_value):
                            continue
                        xy = self.y.setdefault(key, [[], []])
                        if len(xy) != 2:
                            continue

                        xy_len = len(xy[1])
                        if (xy_len >= 1 and new_value != xy[1][-1]) or xy_len == 0:
                            xy[1].append(new_value)
                            xy[0].append(self.iteration)

            # if the value did not change, don't append.
            elif is_valid(y) and (ylen == 0 or y != self.y[-1]):
                self.y.append(y)
            else:
                return

            # append x
            if x is not None:
                self.x.append(x)

        ylen = len(self.y)
        xlen = len(self.x)
        # handle residual ploting
        if isinstance(self.y, dict):
            for i, (key, y) in enumerate(self.y.items()):
                curve = self.curve.get(key, None)
                ylen = len(y[1])

                # no curve, create one
                if curve is None and ylen > 4:
                    curve = self.curve[key] = self.plot_widget.plot([], pen=DEFAULT_PENS[i], name=key)
                if ylen > 5:
                    try:
                        curve.setData(y[0], y[1], connect='finite')
                    except (ValueError, OverflowError):
                        pass
        # normal plotting
        elif ylen > 1:
            if xlen == 0:
                try:
                    self.curve.setData(self.y, connect='finite')
                except (ValueError, OverflowError):
                    pass
            elif xlen == ylen and self.x[-1] - self.x[0] > 0:
                try:
                    self.curve.setData(self.x, self.y, connect='finite')
                except (ValueError, OverflowError):
                    pass

    def _change_name(self, new_name):
        old_name = self.name
        self.name = new_name
        self.graphic_tabs.set_tab_name(self, new_name, old_name)

class GraphicTabs(object):
    """mixin to the gui.MfixGui class to handle plots and graphics"""
    def init_graphics_tabs(self, loadvtk=True):

        self.loadvtk = loadvtk
        self.plot_dict = OrderedDict()

        # configure tab widget
        tab_widget = self.ui.tabWidgetGraphics
        tab_widget.setTabsClosable(True)
        tab_widget.tabCloseRequested.connect(self.remove_tab)
        tab_bar = tab_widget.tabBar()
        tab_bar.setElideMode(QtCore.Qt.ElideRight)

        # create a tab at the end to act like "new" button
        tab_bar.setSelectionBehaviorOnRemove(QtWidgets.QTabBar.SelectLeftTab)
        new_tab = QtWidgets.QWidget()
        new_tab.unsaved_flag = False
        new_tab.name = None
        tab_widget.addTab(new_tab, '')
        idx = self.ui.tabWidgetGraphics.count()-1
        tab_widget.setTabIcon(idx, get_icon('add.svg'))
        tab_widget.currentChanged.connect(self._tab_changed)

        # remove close btn
        right_btn = tab_widget.tabBar().tabButton(idx, QtWidgets.QTabBar.RightSide)
        if right_btn:
            right_btn.resize(0, 0)
        left_btn = tab_widget.tabBar().tabButton(idx, QtWidgets.QTabBar.LeftSide)
        if left_btn:
            left_btn.resize(0, 0)

    def _tab_changed(self, index):
        if index == self.ui.tabWidgetGraphics.count()-1:
            # last tab was clicked, create new tab
            self.add_tab()

    def remove_tab(self, index):
        """ handle signal to close tab """
        tab_widget = self.ui.tabWidgetGraphics
        tab = tab_widget.widget(index)
        if hasattr(tab, 'vtk_widget'):
            tab.vtk_widget.close()
        if tab.monitors:
            self.monitor_reader.disconect(tab.update_monitors)
        tab_widget.removeTab(index)
        if tab.name in self.plot_dict:
            self.plot_dict.pop(tab.name)

    def add_tab(self, name="New"):
        """callback to add a new tab to tabWidgetGraphics"""
        name = get_unique_string(name, self.plot_dict.keys())
        self.set_unsaved_flag()
        return self.create_tab(name)

    def create_tab(self, name="New"):
        """ create a tab (either from click, or restoring from JSON) """
        tab_w = self.ui.tabWidgetGraphics
        new_tab = BaseGraphicTab(name, tab_w, self, self.plot_dict.keys())
        self.plot_dict[name] = new_tab

        index = tab_w.count()-1
        tab_w.insertTab(index, new_tab, name)
        self.ui.tabWidgetGraphics.setCurrentIndex(index)
        return new_tab

    def set_tab_name(self, tab, new_name, old_name):
        """ change a tab name (from "New") """
        tab_widget = self.ui.tabWidgetGraphics
        index = tab_widget.indexOf(tab)
        tab_widget.tabBar().setTabText(index, new_name)
        if old_name in self.plot_dict:
            self.plot_dict[new_name] = self.plot_dict.pop(old_name)

    def reset_graphics_tabs(self):
        '''remove graphics tabs'''
        tab_widget = self.ui.tabWidgetGraphics
        for tab in self.plot_dict.values():
            tab_widget.removeTab(tab_widget.indexOf(tab))
            if tab.vtk:
                tab.vtk_widget.reset()
            tab.setParent(None)
        self.plot_dict = OrderedDict()

    def reset_plots(self):
        """ Delete data for all plots """
        for key, tab in self.plot_dict.items():
            if key in PLOT_ITEMS:
                tab.reset_plot_data()

    def graphics_to_str(self):
        ''' return graphic tab information as a JSON string'''
        order = []
        data = {}
        for key, tab in self.plot_dict.items():
            order.append(key)
            plot = key in PLOT_ITEMS
            vtk = tab.vtk
            monitors = tab.monitors
            d = data[key] = {'plot':plot, 'vtk':vtk, 'monitors':monitors}

            if vtk:
                d['vtk_state'] = tab.vtk_widget.get_state()

            if monitors:
                d['monitors_state'] = tab.get_monitor_state()

        return json.dumps({'order':order, 'data':data})

    def graphics_from_str(self, string):
        '''load graphics tabs from string'''
        json_data = json.loads(string)
        order = json_data.get('order', [])
        data = json_data.get('data', {})

        for key in order:
            tab_data = data.get(key, {})
            if not tab_data:
                continue
            tab = self.create_tab() # set name?
            if tab_data.get('plot', False) and key in PLOT_ITEMS:
                if PYQTGRAPH_AVAILABLE:
                    tab.create_plot_widget(name=key)
                else:
                    self.warn('The pyqtgraph libary is not available, can not create plot')
            elif tab_data.get('vtk', False):
                if self.loadvtk and 'MFIX_NO_VTK' not in os.environ and VTK_AVAILABLE:
                    vtk_widget = tab.create_vtk_widget(load=True)
                    vtk_widget.set_state(tab_data.get('vtk_state', {}))
                else:
                    self.warn('The vtk libary is not available, can not create vtk window')
            elif tab_data.get('monitors', False):
                if PYQTGRAPH_AVAILABLE:
                    tab.create_monitor_plot_widget(name=key)
                    tab.set_monitor_state(tab_data.get('monitors_state', {}))
                else:
                    self.warn('The pyqtgraph libary is not available, can not create plot')

    def update_plots(self, status):
        """ Update the plot data for each tab """
        for k, plot in self.plot_dict.items():
            if k in PLOT_ITEMS:
                props = PLOT_ITEMS[k]
                x_var = props.get('x_var', None)
                y_var = props.get('y_var', None)
                if y_var is None:
                    continue
                x = status.get(x_var, None)
                y = status.get(y_var, None)
                plot.plot(x=x, y=y, append=True)
