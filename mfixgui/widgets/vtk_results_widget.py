# -*- coding: utf-8 -*-
import copy
import glob
import os
from bisect import bisect_left
from collections import Mapping, OrderedDict
from xml.etree import ElementTree

from qtpy import QtCore, QtGui, QtWidgets

from mfixgui.colormaps.color_maps import build_qicons
from mfixgui.constants import *
from mfixgui.tools.qt import get_icon, sub_icon_size, get_ui
from mfixgui.widgets.base import CustomPopUp
from mfixgui.widgets.base_vtk import BaseVtkWidget
from mfixgui.widgets.vtk_constants import SPLAT_SHADERS

# graphics libraries
try:
    import pyqtgraph as pg
    PYQTGRAPH_AVAILABLE = True
    pg.setConfigOption('background', 'w')
    pg.setConfigOption('foreground', 'k')
    DEFAULT_PEN = pg.mkPen(color='#64B5F6', width=2)
except ImportError:
    pg = None
    PYQTGRAPH_AVAILABLE = False
    DEFAULT_PEN = 'b'
except RuntimeError:
    pg = None
    PYQTGRAPH_AVAILABLE = False
    DEFAULT_PEN = 'b'

try:
    import vtk
    from mfixgui.colormaps.color_maps import build_vtk_lookup_tables
    VTK_AVAILABLE = True
    LOOKUP_TABLES = build_vtk_lookup_tables()
    GLYPHS = {
        'sphere':   vtk.vtkSphereSource,
        'cube':      vtk.vtkCubeSource,
        'cylinder': vtk.vtkCylinderSource,
        'cone':     vtk.vtkConeSource}

    # check for point gaussian
    POINT_GAUSSIAN = hasattr(vtk, 'vtkPointGaussianMapper')
except:
    vtk = None
    VTK_AVAILABLE = False
    build_vtk_lookup_tables, get_color_map_pngs = None, None
    LOOKUP_TABLES = {}
    POINT_GAUSSIAN = False

PLOT_ITEMS = OrderedDict([
    ['Select an item', {}],
    ['dt', {'left':'dt', 'bottom':'Time Step', 'var':'dt'}],
    ['nit', {'left':'Number of Iterations', 'bottom':'Time Step', 'var':'nit'}],
    ['time', {'left':'Simulation Time [s]', 'bottom':'Elapsed Wall Time [s]', 'var':'time', 'var2':'walltime_elapsed'}],
    ])

SETTINGS = QtCore.QSettings('MFIX', 'MFIX')
DEFAULT_MAXIMUM_POINTS = 10000
MAX_PLAYBACK_DELAY = 1000 # ms
DEFAULT_PLAYBACK_DELAY = 100 # ms
DEFAULT_GEO_COLOR = QtGui.QColor(224, 224, 224)
DEFAULT_TEXT_COLOR = QtGui.QColor(0, 0, 0)
GEOMETRY = ['Cells', 'Nodes', 'Points', 'Geometry', 'Color Bar', 'Time Label']

def safe_float(s, d=0.0):
    try:
        return float(s)
    except:
        return d

def clean_dict(dirty_dict):
    """remove qcolor objects save the hex values"""
    cd = {}
    for k, v in dirty_dict.items():
        if isinstance(v, (dict, OrderedDict)):
            cd[k] = clean_dict(v)
        elif isinstance(v, QtGui.QColor):
            cd[k] = v.name()
        else:
            cd[k] = v
    return cd

def qcolor_dict(d):
    """the reverse of clean_dict, change hex back to qcolor"""
    qd = {}
    for k, v in d.items():
        if isinstance(v, (dict, OrderedDict)):
            qd[k] = qcolor_dict(v)
        elif isinstance(v, str) and v.startswith('#'):
            qd[k] = QtGui.QColor(v)
        else:
            qd[k] = v
    return qd

def parse_pvd_file(fname):
    '''given a pvd file, return a dict of time (float) : file_name'''
    f_dict = OrderedDict()
    if not os.path.exists(fname): return f_dict
    try:
        tree = ElementTree.parse(fname)
    except:
        return f_dict
    root = tree.getroot()
    for data_set in root.iter('DataSet'):
        f_dict[float(data_set.attrib['timestep'])] = data_set.attrib['file']

    return f_dict

def build_time_dict(search_str):
    '''given a glob search string, return a dictionary of
    time (float) : file_name'''
    f_dict = OrderedDict()

    files = glob.glob(search_str)
    for f in sorted(files):
        time = None
        with open(f, 'r', encoding='utf-8', errors='replace') as xml:
            for i, line in enumerate(xml):
                if '<!-- Time =' in line:
                    try:
                        time = float(line.replace('<!-- Time =', '').replace('sec. -->', ''))
                    except:
                        pass
                    break

                if i > 4:
                    break
        if time is not None:
            f_dict[time] = os.path.basename(f)
    return f_dict

def update(d, u):
    for k, v in u.items():
        if isinstance(v, Mapping):
            r = update(d.get(k, {}), v)
            d[k] = r
        else:
            d[k] = u[k]
    return d

class ArrowWidget(QtWidgets.QWidget):
    '''a widget that draws an arrow'''
    def __init__(self, parent=None):
        QtWidgets.QWidget.__init__(self, parent)

    def paintEvent(self, event):

        w = self.width()
        h = self.height()

        # draw path
        path = QtGui.QPainterPath()
        path.moveTo(int(w/2.0), 0)
        path.lineTo(int(w/2.0), int(h/2.0))
        path.lineTo(w-2, int(h/2.0))

        pen = QtGui.QPen(QtCore.Qt.darkGray)
        pen.setWidth(2)

        painter = QtGui.QPainter(self)
        painter.setPen(pen)
        painter.drawPath(path)

        # draw arrow heading
        path = QtGui.QPainterPath()
        m = int(h/2.0)
        path.moveTo(w, m)
        path.lineTo(w-5, m-5)
        path.lineTo(w-5, m+5)
        path.lineTo(w, m)

        painter.setBrush(QtCore.Qt.darkGray)
        painter.setPen(QtCore.Qt.NoPen)
        painter.drawPath(path)

class ColorMapPopUp(QtWidgets.QDialog):
    applyEvent = QtCore.Signal(object, object, object)
    def __init__(self, parent=None):
        QtWidgets.QDialog.__init__(self, parent)

        self.color = None
        ui = self.ui = get_ui('color_map.ui', self)

        self.setWindowTitle('Color Map')

        ui.toolbutton_select_color.clicked.connect(self.select_color)
        ui.toolbutton_select_color.setStyleSheet("QToolButton{{ background: {};}}".format(DEFAULT_GEO_COLOR.name()))
        ui.lineedit_from.dtype = float
        ui.lineedit_to.dtype = float

        for name, icons in build_qicons().items():
            if not name.endswith('_reversed'):
                ui.combobox_color_map.addItem(icons.get('bar', QtGui.QIcon()), name)
        self.set_color_map('viridis')

        btn = ui.buttonBox.button(QtWidgets.QDialogButtonBox.Apply)
        btn.clicked.connect(self.emit_apply_event)

        ui.toolbutton_auto_scale.clicked.connect(self.auto_scale)

    def emit_apply_event(self):
        self.applyEvent.emit(self.geo, self.button, self.array)

    def set_(self, array):
        self.array = array
        self.color = self.array.get('color', DEFAULT_GEO_COLOR)
        self.set_color(self.color)
        self.set_color_map(self.array.get('color_map', 'viridis'))
        self.ui.checkbox_reversed.setChecked(self.array.get('reversed', False))

        d_range = self.array.get('range', [0, 1])
        self.ui.lineedit_from.updateValue(None, self.array.get('from', '{:.3g}'.format(d_range[0])))
        self.ui.lineedit_to.updateValue(None, self.array.get('to', '{:.3g}'.format(d_range[1])))

        single_color = self.array.get('single_color', False)
        self.ui.checkbox_single_color.setChecked(single_color)
        self.ui.widget_color_map.setEnabled(not single_color)

    def get(self):
        color_map = self.ui.combobox_color_map.currentText()
        reverse = self.ui.checkbox_reversed.isChecked()

        rng = [safe_float(self.ui.lineedit_from.value, 0),
               safe_float(self.ui.lineedit_to.value, 1)]

        if reverse:
            color_map += '_reversed'
        return {
            'color':        self.color,
            'single_color': self.ui.checkbox_single_color.isChecked(),
            'color_map':    color_map,
            'reversed':     reverse,
            'from':         min(rng),
            'to':           max(rng),
            }

    def select_color(self):
        col = QtWidgets.QColorDialog.getColor()
        if not col.isValid():
            return
        self.color = col
        self.set_color(col)

    def set_color(self, color):
        if isinstance(color, QtGui.QColor):
            self.ui.toolbutton_select_color.setStyleSheet("QToolButton{{ background: {};}}".format(
                color.name()))

    def set_color_map(self, color_map):
        color_map = color_map.replace('_reversed', '')
        self.ui.combobox_color_map.setCurrentText(color_map)

    def auto_scale(self):
        d_range = self.array.get('range', [0, 1])
        self.ui.lineedit_from.updateValue(None, '{:.3g}'.format(d_range[0]))
        self.ui.lineedit_to.updateValue(None, '{:.3g}'.format(d_range[1]))


class ParticleOptions(QtWidgets.QDialog):
    applyEvent = QtCore.Signal()
    def __init__(self, parent=None):
        QtWidgets.QDialog.__init__(self, parent)

        self.color = None
        self.ui = get_ui('particle_options.ui', self)

        self.ui.lineedit_maximum_particles.updateValue(None, DEFAULT_MAXIMUM_POINTS)
        self.ui.lineedit_maximum_particles.dtype = int
        self.setWindowTitle('Particle Options')

        btn = self.ui.buttonBox.button(QtWidgets.QDialogButtonBox.Apply)
        btn.clicked.connect(self.emit_apply_event)

        self.ui.combobox_mapper.setEnabled(POINT_GAUSSIAN)
        self.ui.combobox_mapper.currentIndexChanged.connect(self.handle_mapper_change)

        self.ui.label_gaussian.setVisible(False)
        self.ui.combobox_gaussian.setVisible(False)
        self.ui.combobox_gaussian.addItems(sorted(SPLAT_SHADERS.keys()))

    def emit_apply_event(self):
        self.applyEvent.emit()

    def set(self, data):
        """set options"""
        self.ui.lineedit_maximum_particles.updateValue(None, data.get('max_points', DEFAULT_MAXIMUM_POINTS))
        self.ui.combobox_mapper.updateValue(None, data.get('mapper', 'glyphs') if POINT_GAUSSIAN else 'glyphs')
        self.ui.combobox_glyph.updateValue(None, data.get('glyph', 'sphere'))
        self.ui.combobox_gaussian.updateValue(None, data.get('splat', 'sphere'))

    def handle_mapper_change(self):

        glyphs = self.ui.combobox_mapper.currentText() == 'glyphs'
        self.ui.lineedit_maximum_particles.setEnabled(glyphs)
        self.ui.label_glyph.setVisible(glyphs)
        self.ui.combobox_glyph.setVisible(glyphs)
        self.ui.label_gaussian.setVisible(not glyphs)
        self.ui.combobox_gaussian.setVisible(not glyphs)

    def get(self):
        """return options"""
        return {
            'max_points': self.ui.lineedit_maximum_particles.value,
            'mapper': self.ui.combobox_mapper.currentText(),
            'glyph': self.ui.combobox_glyph.currentText(),
            'splat': self.ui.combobox_gaussian.currentText(),
            }


class GraphicsVtkWidget(BaseVtkWidget):
    """vtk widget for showing results"""
    def __init__(self, parent=None, load=False):
        BaseVtkWidget.__init__(self, parent)

        self.loading = False
        self.cell_arrays = {}
        self.vtu_pattern = None
        self.node_arrays = {}
        self.point_arrays = {}
        self.vtp_pattern = None
        self.pvd_files = {}
        self.frame_index = -1
        self.vtp_files = []
        self.vtu_files = []
        self.update_color_by = False
        self.time = 0.0
        self.time_format = '{:.2f} s'
        self.time_label_color = DEFAULT_TEXT_COLOR
        self.color_bar_color = DEFAULT_TEXT_COLOR
        self.geometry_color = DEFAULT_GEO_COLOR
        self.particle_mapper_str = 'point gaussian' if POINT_GAUSSIAN else 'glyphs'
        self.particle_render_options = {
            'max_points': DEFAULT_MAXIMUM_POINTS,
            'glyph': 'sphere',
            'mapper': self.particle_mapper_str,
            'splat': 'sphere',
            }


        self.play_timer = QtCore.QTimer()
        self.play_timer.timeout.connect(self.forward)

        # look for vtu files
        self.file_timer = QtCore.QTimer()
        self.file_timer.timeout.connect(self.look_for_files)
        self.file_timer.start(1000)

        # dialogs
        self.color_dialog  = ColorMapPopUp(self)
        self.color_dialog.applyEvent.connect(self.change_color)

        self.particle_option_dialog = ParticleOptions(self)
        self.particle_option_dialog.applyEvent.connect(self.change_particle_options)

        self.init_toolbar()
        self.init_vtk()
        self.init_geometry()

        # enable time label and color bar
        self.enable_toolbar_geo('time_label')

        if not load:
            # look for files
            self.look_for_files()
            self.change_frame(0)
            self.reset_view()

    @property
    def project_dir(self):
        return self.gui.get_project_dir()

    def set_state(self, state):
        '''load a saved vtk state'''
        self.defer_render = True # defer rendering vtk until load complete
        self.loading = True

        state = qcolor_dict(state)

        # set particle mapper
        if not POINT_GAUSSIAN:
            self.particle_mapper_str = 'glyphs'
        else:
            self.particle_mapper_str = state.get('particle_options', {}).get('mapper', 'glyphs')

        # look for files
        self.look_for_files()

        # update colorbar/range/etc. data
        self.cell_arrays.update(state.get('cell_arrays', {}))
        self.node_arrays.update(state.get('node_arrays', {}))
        self.point_arrays.update(state.get('point_arrays', {}))

        # set the file patterns
        for geo, key in [('cells', 'vtu_pattern'), ('points', 'vtp_pattern')]:
            cb = self.visual_btns[geo]['file_pattern']
            val = state.get(key, None)
            if val is not None:
                cb.setCurrentText(val)
                self.change_file_pattern(geo, cb, new=val)

        # set the array names
        for geo in ['cells', 'nodes', 'points']:
            cb = self.visual_btns[geo]['color_by']
            color_button = self.visual_btns[geo]['color']
            component = self.visual_btns[geo]['component']

            array_name = state.get('_'.join([geo, 'color_by']), None)
            if array_name is not None:
                index = cb.findText(array_name)
                if index < 0:
                    continue
                cb.setCurrentIndex(index)
                self.handle_change_color(geo, color_button, cb, popup=False)

                val = state.get('_'.join([geo, 'component']), None)
                if val is not None and val.lower() in ['mag', 'x', 'y', 'z']:
                    index = component.findText(val)
                    if index < 0 :
                        continue
                    component.setCurrentIndex(index)
                    self.change_color_by(geo, cb, component, array_name)

        # particle render options
        self.change_particle_options(data=state.get('particle_options', {}))

        # visibility and opacity for all actors
        visible = state.get('visible', {})
        opacity =  state.get('opacity', {})
        for geo_name in GEOMETRY:
            geo = geo_name.lower().replace(' ', '_')
            geo_btns = self.visual_btns.get(geo)
            vis = visible.get(geo, True)
            geo_btns['visible'].setChecked(vis)
            self.change_visibility(geo, vis)
            op = opacity.get(geo, 1)
            geo_btns['opacity'].setValue(op)
            self.change_opacity(geo, op)

        # geometry
        pb = self.visual_btns.get('geometry', {}).get('color')
        self.change_geo_color(pb, state.get('geometry_color',  DEFAULT_GEO_COLOR))

        # color_bar
        cb = self.visual_btns.get('color_bar', {}).get('mapper')
        cb.setCurrentText(state.get('color_bar_mapper', 'Cells'))
        self.change_colorbar_mapper(cb)
        cb = self.visual_btns.get('color_bar', {}).get('pos')
        cb.setCurrentText(state.get('color_bar_pos', 'Right'))
        self.change_colorbar_loc(cb)
        pb = self.visual_btns.get('color_bar', {}).get('color')
        self.change_color_bar_color(pb, state.get('color_bar_color',  DEFAULT_TEXT_COLOR))

        #time label
        lb = self.visual_btns.get('time_label', {}).get('label_format')
        formt = state.get('time_label_format', '{:.2f}')
        lb.setText(formt)
        self.handle_label_format(formt)
        pb = self.visual_btns.get('time_label', {}).get('color')
        self.change_time_label_color(pb, state.get('time_label_color',  DEFAULT_TEXT_COLOR))

        # camera
        camera_state = state.get('camera', None)
        if camera_state is not None:
            self.set_camera_state(camera_state)

        # set the current frame
        self.change_frame(state.get('frame', 0))

        self.render(defer_render=False) # render
        self.loading = False

    def set_unsaved_flag(self):
        """don't call set_unsaved_flag when loading"""
        if not self.loading:
            self.gui.set_unsaved_flag()

    def get_state(self):
        state = {
        'vtu_pattern': self.vtu_pattern,
        'vtp_pattern': self.vtp_pattern,
        'cell_arrays': self.cell_arrays,
        'node_arrays': self.node_arrays,
        'point_arrays': self.point_arrays,
        'frame': self.frame_index,
        'camera': self.get_camera_state(),
        'particle_options': self.particle_render_options,
        }

        # save array and component selection
        for geo in ['cells', 'nodes', 'points']:
            for key in ['color_by', 'component']:
                val = self.visual_btns[geo][key].currentText()
                if val:
                    state['_'.join([geo, key])] = val

        # opacity/visible
        visible = state['visible'] = {}
        opacity = state['opacity'] = {}
        for geo_name in GEOMETRY:
            geo = geo_name.lower().replace(' ', '_')
            geo_btns = self.visual_btns.get(geo)
            visible[geo] = geo_btns['visible'].isChecked()
            opacity[geo] = geo_btns['opacity'].value()

        # geometry
        state['geometry_color'] = self.geometry_color

        #color bar
        state['color_bar_mapper'] = self.visual_btns.get('color_bar', {}).get('mapper').currentText()
        state['color_bar_pos'] = self.visual_btns.get('color_bar', {}).get('pos').currentText()
        state['color_bar_color'] = self.color_bar_color

        #time label
        state['time_label_format'] = self.visual_btns.get('time_label', {}).get('label_format').text()
        state['time_label_color'] = self.time_label_color

        return clean_dict(state)

    def reset(self):
        self.play_timer.stop()
        self.file_timer.stop()

    def init_vtk(self):

        self.actors = {'time_label':self.time_label, 'color_bar':self.scalar_bar}
        self.mappers = {}
        self.lookuptables = {}

        self.ugrid_cell_mapper = None
        self.ugrid_node_mapper = None
        self.particle_mapper_glyph = None
        self.particle_mapper_pg = None
        self.particle_reader = None

        self.time_label.SetVisibility(True)
        self.time_label.SetInput(self.time_format.format(self.time))

    @property
    def particle_mapper(self):
        return self.particle_mapper_glyph if self.particle_mapper_str == 'glyphs' else self.particle_mapper_pg

    @property
    def points_str(self):
        return 'points_glyph' if self.particle_mapper_str == 'glyphs' else 'points_pg'

    def init_ugrid(self):
        '''setup the cell/point vtk stuff'''
        # cells
        self.enable_toolbar_geo('cells', visible=False)
        self.enable_colorbar_select(0)
        self.ugrid_reader = vtk.vtkXMLUnstructuredGridReader()

        self.ugrid_cell_mapper = self.mappers['cells'] = vtk.vtkDataSetMapper()
        self.ugrid_cell_mapper.SetInputConnection(self.ugrid_reader.GetOutputPort())
        self.ugrid_cell_mapper.SetScalarVisibility(True)
        self.ugrid_cell_mapper.SetScalarModeToUseCellFieldData()
        self.change_color_map('cells', 'viridis')

        actor = self.actors['cells'] = vtk.vtkActor()
        actor.SetMapper(self.ugrid_cell_mapper)

        self.vtkrenderer.AddActor(actor)
        self.change_visibility('cells', False) # hide cells by default

        # points
        self.enable_toolbar_geo('nodes')
        self.enable_colorbar_select(1)

        self.ugrid_cell_to_points = vtk.vtkCellDataToPointData()
        self.ugrid_cell_to_points.SetInputConnection(self.ugrid_reader.GetOutputPort())

        self.ugrid_node_mapper = self.mappers['nodes'] = vtk.vtkDataSetMapper()
        self.ugrid_node_mapper.SetInputConnection(self.ugrid_cell_to_points.GetOutputPort())
        self.ugrid_node_mapper.SetScalarVisibility(True)
        self.ugrid_node_mapper.SetScalarModeToUsePointFieldData()
        #self.ugrid_node_mapper.UseLookupTableScalarRangeOn()
        #self.ugrid_node_mapper.InterpolateScalarsBeforeMappingOn()
        self.change_color_map('nodes', 'viridis')

        actor = self.actors['nodes'] = vtk.vtkActor()
        actor.SetMapper(self.ugrid_node_mapper)

        self.vtkrenderer.AddActor(actor)

    def init_particles(self):
        '''setup the particle vtk stuff'''
        self.enable_toolbar_geo('points')
        self.enable_colorbar_select(2)

        if self.particle_reader is None:
            self.particle_reader = vtk.vtkXMLPolyDataReader()

        if POINT_GAUSSIAN and self.particle_mapper_str == 'point gaussian':
            self.init_point_gaussian()
        else:
            self.particle_mapper_str = 'glyphs'
            self.init_glyph_mapper()

    def init_glyph_mapper(self):
        # glyph mapper
        self.glyph_mask = vtk.vtkMaskPoints()
        self.glyph_mask.SetInputConnection(self.particle_reader.GetOutputPort())
        self.glyph_mask.RandomModeOn()
        self.glyph_mask.SetRandomModeType(2) # setting to type 1 crashes on windows
        self.glyph_mask.SetMaximumNumberOfPoints(self.particle_render_options.get('max_points', DEFAULT_MAXIMUM_POINTS))

        self.glyph = vtk.vtkGlyph3D()
        self.glyph.SetInputConnection(self.glyph_mask.GetOutputPort())
        self.glyph.SetColorModeToColorByVector()
        self.set_glyph_source(self.particle_render_options.get('glyph', 'sphere'))

        self.particle_mapper_glyph = self.mappers['points_glyph'] = vtk.vtkPolyDataMapper()
        self.particle_mapper_glyph.SetInputConnection(self.glyph.GetOutputPort())
        self.change_color_map('points', 'viridis')

        actor = self.actors['points_glyph'] = vtk.vtkActor()
        actor.SetMapper(self.particle_mapper_glyph)
        if hasattr(actor, 'SetForceOpaque'):
            actor.SetForceOpaque(True)
        self.vtkrenderer.AddActor(actor)

    def init_point_gaussian(self):

        # setup the point gaussian mapper
        pg = vtk.vtkPointGaussianMapper()
        pg.EmissiveOff()
        pg.SetScaleArray('Diameter')
        pg.SetScaleFactor(.5)
        pg.SetScalarVisibility(True)
        pg.SetScalarModeToUsePointFieldData()
        pg.SetInputConnection(self.particle_reader.GetOutputPort())
        pg.SetSplatShaderCode(SPLAT_SHADERS.get('sphere'))
        self.particle_mapper_pg = self.mappers['points_pg'] = pg
        self.change_color_map('points', 'viridis')

        actor= self.actors['points_pg'] = vtk.vtkActor()
        actor.SetMapper(self.particle_mapper_pg)
        actor.SetForceOpaque(True)

        self.vtkrenderer.AddActor(actor)

    def init_geometry(self):
        self.enable_toolbar_geo('geometry')
        poly_data = self.gui.vtkwidget.collect_toplevel_geometry()

        # Create a mapper
        mapper = self.mappers['geometry'] = vtk.vtkPolyDataMapper()
        if poly_data is not None:
            mapper.SetInputConnection(poly_data.GetOutputPort())
        mapper.ScalarVisibilityOff()

        # Create an actor
        actor = self.actors['geometry'] = vtk.vtkActor()
        actor.SetMapper(mapper)
        actor.GetProperty().SetColor(DEFAULT_GEO_COLOR.getRgbF()[:3])
        actor.GetProperty().SetOpacity(0.4)

        self.vtkrenderer.AddActor(actor)

    def enable_toolbar_geo(self, geo, visible=True):
        for name, wid in self.visual_btns[geo].items():
            if name == 'component':
                continue
            if visible and name == 'visible':
                wid.setChecked(True)
            wid.setEnabled(True)

    def enable_colorbar_select(self, index, enable=True, combo=None):
        if combo is None:
            combo = self.visual_btns['color_bar']['mapper']
        model = combo.model()
        item = model.item(index)
        if not enable:
            flags = QtCore.Qt.NoItemFlags
        else:
            flags = QtCore.Qt.ItemIsSelectable|QtCore.Qt.ItemIsEnabled
        item.setFlags(flags)

    def init_toolbar(self):
        self.init_base_toolbar()

        # more buttons
        self.toolbutton_visible = QtWidgets.QToolButton()
        self.toolbutton_visible.setToolTip('Change Visibility')
        self.toolbutton_visible.setIcon(get_icon('visibility.svg'))
        self.toolbutton_visible.setIconSize(sub_icon_size())
        self.visible_menu = CustomPopUp(self, self.toolbutton_visible)
        self.toolbutton_visible.clicked.connect(self.show_visible_menu)

        # --- visual representation menu ---
        layout = self.visible_menu.layout
        layout.setContentsMargins(0, 5, 5, 5)
        self.visual_btns = {}
        for i, geo_name in enumerate(GEOMETRY):
            geo = geo_name.lower().replace(' ', '_')
            btns = self.visual_btns[geo] = {}
            # visibility
            toolbutton = QtWidgets.QToolButton(self.visible_menu)
            toolbutton.clicked.connect(lambda down, g=geo: self.change_visibility(g, down))
            toolbutton.setCheckable(True)
            toolbutton.setChecked(False)
            toolbutton.setAutoRaise(True)
            toolbutton.setIcon(get_icon('visibility.svg'))
            toolbutton.setIconSize(sub_icon_size())
            toolbutton.setEnabled(False)
            layout.addWidget(toolbutton, i, 0)
            btns['visible'] = toolbutton

            combo = None
            if geo in ['cells', 'nodes', 'points']:
                # file pattern
                if not geo == 'nodes':
                    combo = QtWidgets.QComboBox(self.visible_menu)
                    combo.activated.connect(lambda item, g=geo, c=combo: self.change_file_pattern(g, c))
                    combo.setEnabled(False)
                    combo.setToolTip('File pattern to display')
                    layout.addWidget(combo, i, 1)
                    btns['file_pattern'] = combo
                elif geo == 'nodes':
                    lb = ArrowWidget(self.visible_menu)
                    layout.addWidget(lb, i, 1)

                # color by (array name)
                combo = QtWidgets.QComboBox(self.visible_menu)
                combo.activated.connect(lambda item, g=geo, c=combo: self.change_color_by(g, c))
                combo.setEnabled(False)
                combo.setToolTip('Variable to color by')
                layout.addWidget(combo, i, 2)
                btns['color_by'] = combo

                # component of array (mag, X, Y, Z)
                combo2 = QtWidgets.QComboBox(self.visible_menu)
                combo2.activated.connect(lambda item, g=geo, c=combo, c2=combo2: self.change_color_by(g, c, c2))
                combo2.addItems(['mag', 'x', 'y', 'z'])
                combo2.setToolTip('Component to color by')
                combo2.setEnabled(False)
                layout.addWidget(combo2, i, 3)
                btns['component'] = combo2

            elif geo == 'color_bar':
                combo = QtWidgets.QComboBox(self.visible_menu)
                combo.activated.connect(lambda item, c=combo: self.change_colorbar_mapper(c))
                combo.setEnabled(False)
                layout.addWidget(combo, i, 1)
                combo.addItems(['Cells', 'Nodes', 'Points'])
                for ind in range(3):
                    self.enable_colorbar_select(ind, False, combo)
                btns['mapper'] = combo

                combo = QtWidgets.QComboBox(self.visible_menu)
                combo.activated.connect(lambda item, c=combo: self.change_colorbar_loc(c))
                combo.setEnabled(False)
                combo.setToolTip('Location of color bar')
                layout.addWidget(combo, i, 3)
                combo.addItems(['Left', 'Right', 'Top', 'Bottom'])
                combo.setCurrentIndex(1)
                btns['pos'] = combo
            elif geo == 'time_label':
                lineedit = QtWidgets.QLineEdit(self.visible_menu)
                lineedit.setText(self.time_format)
                lineedit.textChanged.connect(self.handle_label_format)
                lineedit.setEnabled(False)
                layout.addWidget(lineedit, i, 1)
                lineedit.setToolTip('Format to be used in the display of the '
                                    'time label. Needs to be a valid python'
                                    'format string such as "{:.2f}", or'
                                    '"{:2E}".')
                btns['label_format'] = lineedit

            toolbutton = QtWidgets.QToolButton(self.visible_menu)
            size = QtCore.QSize(25, 25)
            toolbutton.setMinimumSize(size)
            toolbutton.setMaximumSize(size)
            toolbutton.setIconSize(size)
            if geo in ['cells', 'nodes', 'points']:
                toolbutton.clicked.connect(lambda ignore, g=geo, t=toolbutton, c=combo: self.handle_change_color(g, t, c))
                toolbutton.setIcon(build_qicons().get('viridis', {}).get('icon', QtGui.QIcon))
            elif geo == 'geometry':
                toolbutton.clicked.connect(lambda ignore, t=toolbutton: self.change_geo_color(t))
                toolbutton.setStyleSheet("QToolButton{{ background: {};}}".format(DEFAULT_GEO_COLOR.name()))
            elif geo == 'color_bar':
                toolbutton.clicked.connect(lambda ignore, t=toolbutton: self.change_color_bar_color(t))
                toolbutton.setStyleSheet("QToolButton{{ background: {};}}".format(DEFAULT_TEXT_COLOR.name()))
            elif geo == 'time_label':
                toolbutton.clicked.connect(lambda ignore, t=toolbutton: self.change_time_label_color(t))
                toolbutton.setStyleSheet("QToolButton{{ background: {};}}".format(DEFAULT_TEXT_COLOR.name()))
            toolbutton.setAutoRaise(True)
            toolbutton.setEnabled(False)
            layout.addWidget(toolbutton, i, 4)
            btns['color'] = toolbutton

            opacity = QtWidgets.QDoubleSpinBox(self.visible_menu)
            opacity.setRange(0, 1)
            if geo == 'geometry':
                opacity.setValue(0.4)
            else:
                opacity.setValue(1.0)
            opacity.setSingleStep(0.1)
            opacity.editingFinished.connect(lambda o=opacity, g=geo: self.change_opacity(g, o))
            opacity.setEnabled(False)
            layout.addWidget(opacity, i, 5)
            btns['opacity'] = opacity

            # label
            label = QtWidgets.QLabel(geo_name, self.visible_menu)
            if geo == 'points':
                layout.addWidget(label, i, 10)
            else:
                layout.addWidget(label, i, 10, 1, 2)

            # more
            if geo in ['points']:
                toolbutton = QtWidgets.QToolButton()
                toolbutton.setAutoRaise(True)
                toolbutton.setIcon(get_icon('right.svg'))
                toolbutton.setIconSize(sub_icon_size())
                toolbutton.setEnabled(False)
                toolbutton.clicked.connect(self.handle_particle_options)
                layout.addWidget(toolbutton, i, 11)
                btns['more'] = toolbutton

        # --- play/stop/forward/backward controls ---
        self.toolbutton_first = QtWidgets.QToolButton()
        self.toolbutton_first.clicked.connect(self.handle_first)
        self.toolbutton_first.setIcon(get_icon('first.svg'))
        self.toolbutton_first.setIconSize(sub_icon_size())
        self.toolbutton_first.setToolTip('First')

        self.toolbutton_back = QtWidgets.QToolButton()
        self.toolbutton_back.clicked.connect(self.handle_back)
        self.toolbutton_back.setIcon(get_icon('back.svg'))
        self.toolbutton_back.setIconSize(sub_icon_size())
        self.toolbutton_back.setToolTip('Previous')

        self.toolbutton_play = QtWidgets.QToolButton()
        self.toolbutton_play.clicked.connect(self.handle_play_stop)
        self.toolbutton_play.setIcon(get_icon('play.svg'))
        self.toolbutton_play.setIconSize(sub_icon_size())
        self.toolbutton_play.setToolTip('Play')

        self.toolbutton_next = QtWidgets.QToolButton()
        self.toolbutton_next.clicked.connect(self.handle_next)
        self.toolbutton_next.setIcon(get_icon('next.svg'))
        self.toolbutton_next.setIconSize(sub_icon_size())
        self.toolbutton_next.setToolTip('Next')

        self.toolbutton_last = QtWidgets.QToolButton()
        self.toolbutton_last.clicked.connect(self.handle_last)
        self.toolbutton_last.setIcon(get_icon('last.svg'))
        self.toolbutton_last.setIconSize(sub_icon_size())
        self.toolbutton_last.setToolTip('Last')

        self.toolbutton_play_speed = QtWidgets.QToolButton()
        self.toolbutton_play_speed.setIcon(get_icon('speed.svg'))
        self.toolbutton_play_speed.setIconSize(sub_icon_size())
        self.toolbutton_play_speed.setToolTip('Play Speed')

        self.speed_menu = CustomPopUp(self, self.toolbutton_play_speed)
        self.speed_menu.finished.connect(lambda ignore: self.toolbutton_play_speed.setDown(False))
        self.speed_slider = QtWidgets.QSlider(self.speed_menu)
        self.speed_slider.setRange(0, MAX_PLAYBACK_DELAY) # delay = MAX - speed
        self.speed_slider.setValue(MAX_PLAYBACK_DELAY - DEFAULT_PLAYBACK_DELAY)
        self.speed_slider.setOrientation(QtCore.Qt.Horizontal)
        self.speed_slider.sliderReleased.connect(self.handle_speed_changed)
        self.speed_menu.layout.addWidget(self.speed_slider)
        self.toolbutton_play_speed.clicked.connect(self.speed_menu.popup)

        self.toolbutton_repeat = QtWidgets.QToolButton()
        self.toolbutton_repeat.setIcon(get_icon('autorenew.svg'))
        self.toolbutton_repeat.setIconSize(sub_icon_size())
        self.toolbutton_repeat.setCheckable(True)
        self.toolbutton_repeat.setToolTip('Repeat from begining')

        hspacer = QtWidgets.QSpacerItem(99999, 10, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Maximum,)

        self.frame_spinbox = QtWidgets.QSpinBox()
        self.frame_spinbox.editingFinished.connect(lambda: self.change_frame(self.frame_spinbox.value()))
        self.frame_spinbox.setMaximum(9999999)
        self.frame_spinbox.setButtonSymbols(QtWidgets.QAbstractSpinBox.NoButtons)

        self.checkbox_snap = QtWidgets.QCheckBox('Save Snapshots')

        for btn in [self.toolbutton_visible,
                    self.toolbutton_first, self.toolbutton_back,
                    self.toolbutton_play,
                    self.toolbutton_next, self.toolbutton_last,
                    self.toolbutton_play_speed,
                    self.toolbutton_repeat,
                    hspacer, self.frame_spinbox,
                    ]:
            if btn == hspacer:
                self.button_bar_layout.addSpacerItem(btn)
            else:
                self.button_bar_layout.addWidget(btn)
            if isinstance(btn, QtWidgets.QToolButton):
                btn.setAutoRaise(True)
                if btn is not self.toolbutton_visible:
                    btn.setFocusPolicy(QtCore.Qt.ClickFocus)

        self.button_bar_layout.addStretch()

    def showEvent(self, event):
        # has to be called after the widget is visible
        self.vtkiren.Initialize()

    def hideEvent(self, event):
        self.stop()

    def close(self):
        BaseVtkWidget.close(self)

        # clean up timer
        self.play_timer.stop()

    def show_visible_menu(self):
        # update comboboxes based on avaliable arrays
        for type_, array in [('cells', self.cell_arrays.get(self.vtu_pattern)),
                             ('nodes', self.node_arrays.get(self.vtu_pattern)),
                             ('points', self.point_arrays.get(self.vtp_pattern))]:
            btns = self.visual_btns[type_]
            combo = btns['color_by']
            text = combo.currentText()
            combo.clear()

            if array:
                combo.addItems(array.keys())
                combo.setCurrentText(text)
            combo.setEnabled(bool(array))

        self.visible_menu.popup()

    def handle_speed_changed(self):
        if self.play_timer.isActive():
            self.play_timer.stop()
            self.handle_play_stop()

    def stop(self):
        self.toolbutton_play.setIcon(get_icon('play.svg'))
        self.play_timer.stop()

    def handle_play_stop(self):
        if self.play_timer.isActive():
            self.stop()
        else:
            self.toolbutton_play.setIcon(get_icon('stop.svg'))
            delay_ms = max(0, MAX_PLAYBACK_DELAY-self.speed_slider.value())
            self.play_timer.start(delay_ms)

    def handle_first(self):
        self.change_frame(0)

    def handle_back(self):
        self.change_frame(self.frame_index - 1)

    def handle_next(self):
        self.change_frame(self.frame_index + 1)

    def handle_last(self):
        self.change_frame(max(len(self.vtu_files), len(self.vtp_files))-1)

    def forward(self):
        self.change_frame(self.frame_index + 1)

    def change_frame(self, index, force=False):

        # assume that whatever one is bigger has the smaller time step
        n_vtp = len(self.vtp_files)
        n_vtu = len(self.vtu_files)
        n_max = max(n_vtp, n_vtu)

        if self.play_timer.isActive() and self.toolbutton_repeat.isChecked() and index == n_max:
            index = 0
        elif index >= n_max:
            index = n_max-1
        elif index < 0:
            index = 0

        if index == self.frame_index and not force:
            return
        else:
            self.frame_index = index


        if n_max > 0:
            vtp_success =  vtu_success = False
            if n_vtp > n_vtu:
                time = list(self.vtp_files.keys())[index]
                vtp_success = self.read_vtp(self.vtp_files[time])
                if n_vtu:
                    vtu_success = self.read_vtu(list(self.vtu_files.values())[bisect_left(list(self.vtu_files.keys()), time)-1])
            else:
                time = list(self.vtu_files.keys())[index]
                vtu_success = self.read_vtu(self.vtu_files[time])
                if n_vtp:
                    vtp_success = self.read_vtp(list(self.vtp_files.values())[bisect_left(list(self.vtp_files.keys()), time)-1])
            self.time = time

            if vtu_success or vtp_success:
                self.frame_spinbox.setValue(index)
                self.set_timelabel(text=self.time_format.format(time))
                self.render()

            if self.checkbox_snap.isChecked():
                project_name = os.path.splitext(os.path.basename(self.gui.get_project_file()))[0]
                self.screenshot(True, fname=os.path.join(self.project_dir, project_name+'_'+str(index).zfill(4)+'.svg'))

    def look_for_files(self):
        if self.project_dir is None:
            return
        pvd_files = glob.glob(os.path.join(self.project_dir, '*.pvd'))
        new_pattern = False
        for pvd in pvd_files:
            base_name = os.path.basename(pvd).replace('.pvd', '')
            files = parse_pvd_file(pvd) # returns OrderedDict keyed by time
            if files:
                if base_name in self.pvd_files:
                    f_dict = self.pvd_files[base_name]['files']
                    f_dict.update(files)
                    # remove missing files
                    remove_list = []
                    for key, path in f_dict.items():
                        if not os.path.exists(os.path.join(self.project_dir, path)):
                            remove_list.append(key)
                    for key in remove_list:
                        f_dict.pop(key)
                else:
                    # new file pattern
                    new_pattern = True
                    key = list(files.keys())[0] # files is nonempty
                    filename = files[key]
                    t = 'vtp' if filename and filename.endswith('vtp') else 'vtu'
                    self.pvd_files[base_name] = {'files':files, 'type':t}

        # update the combo_boxes
        vtp = []
        vtu = []
        for k, v in self.pvd_files.items():
            if v.get('type') == 'vtp':
                vtp.append(k)
            else:
                vtu.append(k)

        if self.vtp_pattern is None and vtp:
            self.vtp_pattern = vtp[0]
        if self.vtu_pattern is None and vtu:
            self.vtu_pattern = vtu[0]

        for geo, btns in self.visual_btns.items():
            fp = btns.get('file_pattern')
            if fp is None: continue
            items = vtp
            if geo in ['cells']:
                items = vtu

            cur = fp.currentText()
            fp.clear()
            fp.addItems(items)
            if len(cur)>0:
                fp.setCurrentText(cur)
            elif items:
                new = items[0]
                fp.setCurrentText(new)
                if items is vtp:
                    self.vtp_files = self.pvd_files[new]['files']
                else:
                    self.vtu_files = self.pvd_files[new]['files']

        # render if new file pattern
        if new_pattern:
            self.change_frame(self.frame_index, force=True)

    def change_file_pattern(self, geo, combo, new=None):
        if new is None:
            new = combo.currentText()
        if not new: return
        pvd_file = self.pvd_files.get(new, None)
        if pvd_file is None: return
        if geo == 'points':
            self.vtp_pattern = new
            self.vtp_files = pvd_file['files']
        else:
            self.vtu_pattern = new
            self.vtu_files = pvd_file['files']
        self.update_color_by = True
        self.change_frame(self.frame_index, True)
        self.change_color_by(geo, self.visual_btns.get(geo, {}).get('color_by'))
        if geo == 'cells':
            self.change_color_by('nodes', self.visual_btns.get('nodes', {}).get('color_by'))
        self.set_unsaved_flag()

    # --- vtk functions ---
    def read_vtu(self, vtu_file):
        init = False
        if self.ugrid_cell_mapper is None:
            self.init_ugrid()
            init = True

        path = os.path.join(self.project_dir, vtu_file)
        if not os.path.exists(path):
            return False

        self.ugrid_reader.SetFileName(path)
        self.ugrid_reader.Update()

        # TODO: Build Once
        data = self.ugrid_reader.GetOutput()
        cell_data = data.GetCellData()
        new_array_info = {}
        for i in range(cell_data.GetNumberOfArrays()):
            array = cell_data.GetArray(i)
            new_array_info[cell_data.GetArrayName(i)] = {
                'i':i,
                'components':array.GetNumberOfComponents(),
                'range': array.GetRange(),}

        cell_info = self.cell_arrays.get(self.vtu_pattern, {})
        node_info = self.node_arrays.get(self.vtu_pattern, {})
        cell_info = update(cell_info, copy.deepcopy(new_array_info))
        node_info = update(node_info, copy.deepcopy(new_array_info))

        self.cell_arrays[self.vtu_pattern] = cell_info
        self.node_arrays[self.vtu_pattern] = node_info

        if init or self.update_color_by:
            name = cell_data.GetArrayName(0)
            for t, m in [('cells', self.ugrid_cell_mapper), ('nodes', self.ugrid_node_mapper)]:
                m.SelectColorArray(name)
                combo = self.visual_btns[t]['color_by']
                combo.clear()
                items = cell_info.keys()
                combo.addItems(items)
                combo.setEnabled(bool(items))
                combo.setCurrentText(name)
            if init:
                self.set_colorbar(mapper=self.ugrid_node_mapper, label=name)
                self.visual_btns['color_bar']['mapper'].setCurrentIndex(1)
            self.update_color_by = False

        return True

    def read_vtp(self, vtp_file):
        init = False
        if self.particle_mapper is None:
            self.init_particles()
            init = True

        path = os.path.join(self.project_dir, vtp_file)
        if not os.path.exists(path):
            return False

        self.particle_reader.SetFileName(path)
        self.particle_reader.Update()

        data = self.particle_reader.GetOutput()
        point_data = data.GetPointData()
        new_array_info = {}
        for i in range(point_data.GetNumberOfArrays()):
            array = point_data.GetArray(i)
            new_array_info[point_data.GetArrayName(i)] = {
                'i':i,
                'components':array.GetNumberOfComponents(),
                'range': array.GetRange()}
        point_info = self.point_arrays.get(self.vtp_pattern, {})
        point_info = update( point_info, copy.deepcopy(new_array_info))
        self.point_arrays[self.vtp_pattern] = point_info

        if 'Diameter' in point_info:
            if self.particle_mapper_glyph is not None:
                self.glyph.SetScaleModeToScaleByScalar()
                self.glyph.SetInputArrayToProcess(0, 0, 0, 0, 'Diameter')
            elif self.particle_mapper_pg is not None:
                self.particle_mapper_pg.SetScaleArray('Diameter')
                self.particle_mapper_pg.SetScaleFactor(.5)
        if init or self.update_color_by:
            name = point_data.GetArrayName(0)
            if self.particle_mapper_glyph is not None:
                self.glyph.SetInputArrayToProcess(1, 0, 0, 0, name)
            elif self.particle_mapper_pg is not None:
                self.particle_mapper_pg.SelectColorArray(name)
            combo = self.visual_btns['points']['color_by']
            combo.clear()
            items = point_info.keys()
            combo.addItems(items)
            combo.setEnabled(bool(items))
            combo.setCurrentIndex(combo.findText(name))
            if init:
                self.set_colorbar(mapper=self.particle_mapper, label=name)
                self.visual_btns['color_bar']['mapper'].setCurrentIndex(2)
            self.update_color_by = False
        return True

    def change_visibility(self, geo, visible):
        geo = self.points_str if geo=='points' else geo
        if geo in self.actors:
            actor = self.actors.get(geo, None)
            if actor is not None:
                actor.SetVisibility(visible)
                self.render()
        self.set_unsaved_flag()

    def change_color_by(self, geo, colorby, component=None, array_name=None):
        if array_name is None:
            array_name = colorby.currentText()

        index = None
        if component is not None:
            comp = component.currentText()
            index = {'x':0, 'y':1, 'z':2, 'mag':None}[comp]

        if geo == 'points':
            if self.particle_mapper_str == 'glyphs':
                self.glyph.SetInputArrayToProcess(1, 0, 0, 0, array_name)
            else:
                self.particle_mapper.SelectColorArray(array_name)
            array = self.point_arrays.get(self.vtp_pattern, {}).get(array_name)
        elif geo == 'cells':
            self.ugrid_cell_mapper.SelectColorArray(array_name)
            array = self.cell_arrays.get(self.vtu_pattern, {}).get(array_name)
        elif geo == 'nodes':
            self.ugrid_node_mapper.SelectColorArray(array_name)
            array = self.node_arrays.get(self.vtu_pattern, {}).get(array_name)
        else:
            return

        if array is None:
            return

        mapper = self.mappers.get(self.points_str if geo=='points' else geo)
        mapper.SetScalarRange(safe_float(array.get('from', 0.0), 0.0), safe_float(array.get('to', 1.0), 1.0))

        single_color = array.get('single_color', False)
        if single_color:
            mapper.ScalarVisibilityOff()
            color = array.get('color', QtCore.Qt.white)
            btn = self.visual_btns[geo]['color']
            btn.setStyleSheet("QToolButton{{ background: {};}}".format(color.name()))
            btn.setIcon(QtGui.QIcon())
        else:
            self.change_color_map(geo, array.get('color_map', 'viridis'), index)
            mapper.ScalarVisibilityOn()

        map_text = self.visual_btns['color_bar']['mapper'].currentText().lower()
        if map_text == geo:
            label = self.visual_btns[map_text]['color_by'].currentText()
            self.set_colorbar(mapper=mapper, label=label)

        is_comp = array['components'] == 3 and not geo == 'points'
        self.visual_btns[geo]['component'].setEnabled(is_comp)

        self.render()
        self.set_unsaved_flag()

    def change_color_map(self, geo, colormap, component=None):
        """change the color map"""
        mapper = self.mappers.get(self.points_str if geo=='points' else geo, None)
        if mapper is None: return
        lut = self.lookuptables.get(geo, None)

        if colormap is not None:
            new_lut = vtk.vtkLookupTable()
            new_lut.DeepCopy(LOOKUP_TABLES.get(colormap, 'viridis'))

            # Fix for bug introduced in VTK7, bug fixed in VTK8
            # https://gitlab.kitware.com/vtk/vtk/issues/16966
            if hasattr(new_lut, 'BuildSpecialColors'):
                new_lut.BuildSpecialColors()

            new_lut.Build()
            # check component in old lut
            if lut is not None and component is None and lut.GetVectorMode() != 0:
                component = lut.GetVectorComponent()
        else:
            new_lut = lut

        if isinstance(component, int):
            new_lut.SetVectorModeToComponent()
            new_lut.SetVectorComponent(component)
        else:
            new_lut.SetVectorModeToMagnitude()

        mapper.SetLookupTable(new_lut)
        self.lookuptables[geo] = new_lut

        if colormap is not None:
            btn = self.visual_btns[geo]['color']
            btn.setIcon(build_qicons().get(colormap).get('icon', QtGui.QIcon()))
            btn.setStyleSheet("QToolButton{{ background: {};}}".format(None))

    def handle_change_color(self, geo, button, colorby, popup=True):
        """popup the color bar dialog"""

        array_name = colorby.currentText()
        if len(array_name) == 0: return
        if geo == 'points':
            array = self.point_arrays.get(self.vtp_pattern, {}).get(array_name)
        elif geo == 'cells':
            array = self.cell_arrays.get(self.vtu_pattern, {}).get(array_name)
        elif geo == 'nodes':
            array = self.node_arrays.get(self.vtu_pattern, {}).get(array_name)
        else:
            return

        if array is None:
            return

        self.color_dialog.set_(array)
        self.color_dialog.geo = geo
        self.color_dialog.button = button
        if popup:
            result = self.color_dialog.exec_()
            if result == QtWidgets.QDialog.Rejected:
                return

        self.change_color(geo, button, array)
        self.set_unsaved_flag()

    def change_color(self, geo, button, array):
        """change the color or color map of an actor"""
        mapper = self.mappers.get(self.points_str if geo=='points' else geo)
        actor = self.actors.get(self.points_str if geo=='points' else geo)

        params = self.color_dialog.get()

        array.update(params)

        color = params.get('color', QtCore.Qt.white)
        color_map = params.get('color_map', 'viridis')

        self.change_color_map(geo, color_map)

        single_color = params.get('single_color', False)

        if single_color:
            button.setStyleSheet("QToolButton{{ background: {};}}".format(color.name()))
            actor.GetProperty().SetColor(color.getRgbF()[:3])
            button.setIcon(QtGui.QIcon())
            mapper.ScalarVisibilityOff()
        else:
            button.setStyleSheet("QToolButton{{ background: {};}}".format(None))
            mapper.ScalarVisibilityOn()

        mapper.SetScalarRange(safe_float(array.get('from', 0.0), 0.0), safe_float(array.get('to', 1.0), 1.0))
        map_text = self.visual_btns['color_bar']['mapper'].currentText().lower()
        if map_text == geo:
            label = self.visual_btns[map_text]['color_by'].currentText()
            self.set_colorbar(mapper=mapper, label=label)

        self.render()

    def change_geo_color(self, button, color=None):
        """Change the color of the geometry actor"""
        if color is None:
            color = QtWidgets.QColorDialog.getColor(parent=self)
            if not color.isValid():
                return
        self.geometry_color = color

        button.setStyleSheet("QToolButton{{ background: {};}}".format(
            color.name()))

        self.actors['geometry'].GetProperty().SetColor(color.getRgbF()[:3])
        self.render()
        self.set_unsaved_flag()

    def change_color_bar_color(self, button, color=None):
        """Change the color of the geometry actor"""
        if color is None:
            color = QtWidgets.QColorDialog.getColor(parent=self)
            if not color.isValid():
                return
        self.color_bar_color = color

        button.setStyleSheet("QToolButton{{ background: {};}}".format(
            color.name()))

        self.set_colorbar(color=color.getRgbF()[:3])
        self.set_unsaved_flag()

    def change_time_label_color(self, button, color=None):
        """Change the color of the geometry actor"""
        if color is None:
            color = QtWidgets.QColorDialog.getColor(parent=self)
            if not color.isValid():
                return
        self.time_label_color = color

        button.setStyleSheet("QToolButton{{ background: {};}}".format(
            color.name()))

        self.set_timelabel(color=color.getRgbF()[:3])
        self.set_unsaved_flag()

    def change_opacity(self, geo, opacity):
        """change the opactiy of an actor"""
        if isinstance(opacity, QtWidgets.QDoubleSpinBox):
            opacity = opacity.value()
        geo = self.points_str if geo=='points' else geo
        if geo in self.actors:
            actor = self.actors.get(geo, None)
            if actor is not None:
                actor.GetProperty().SetOpacity(opacity)
                self.render()
        self.set_unsaved_flag()

    def handle_particle_options(self):
        self.particle_option_dialog.set(self.particle_render_options)
        result = self.particle_option_dialog.exec_()
        if result == QtWidgets.QDialog.Rejected:
            return

        self.change_particle_options()
        self.set_unsaved_flag()

    def change_particle_options(self, data=None):
        if data is None:
            data = self.particle_render_options = self.particle_option_dialog.get()
        mpr = data.get('mapper', 'glyphs')
        if not POINT_GAUSSIAN:
            mpr = data['mapper'] = 'glyphs'
        self.particle_render_options = data

        if self.particle_mapper is None:
            return

        # change particle mapper
        if mpr != self.particle_mapper_str:
            self.particle_mapper_str = mpr

            if self.particle_mapper is None:
                self.init_particles()

            ga = self.actors.get('points_glyph')
            pa = self.actors.get('points_pg')

            if mpr == 'glyphs':
                if ga is not None:
                    self.vtkrenderer.AddActor(ga)
                if pa is not None:
                    self.vtkrenderer.RemoveActor(pa)
            else:
                if ga is not None:
                    self.vtkrenderer.RemoveActor(ga)
                if pa is not None:
                    self.vtkrenderer.AddActor(pa)

            self.change_color_by('points', self.visual_btns['points']['color_by'])

        if POINT_GAUSSIAN and self.particle_mapper_pg is not None:
            self.particle_mapper_pg.SetSplatShaderCode(SPLAT_SHADERS.get(data.get('splat', 'sphere'), 'sphere'))
        if self.particle_mapper_glyph is not None:
            self.glyph_mask.SetMaximumNumberOfPoints(data.get('max_points', DEFAULT_MAXIMUM_POINTS))
            self.set_glyph_source(data.get('glyph', 'sphere'))

        self.render()

    def set_glyph_source(self, name):
        gs = GLYPHS[name]()
        self.glyph.SetSourceConnection(gs.GetOutputPort())

    def set_colorbar(self, mapper=None, label=None, position=None, color=None,
                     shadow=None, italic=None):
        BaseVtkWidget.set_colorbar(self, mapper, label, position, color, shadow, italic)
        self.enable_toolbar_geo('color_bar')
        self.actors['color_bar'] = self.scalar_bar

    def change_colorbar_loc(self, combo):
        pos = combo.currentText().lower()
        self.set_colorbar(position=pos)
        self.render()
        self.set_unsaved_flag()

    def change_colorbar_mapper(self, combo):
        map_text = combo.currentText().lower()

        if map_text == 'cells':
            mapper = self.ugrid_cell_mapper
        elif map_text == 'nodes':
            mapper = self.ugrid_node_mapper
        else:
            mapper = self.particle_mapper

        label = self.visual_btns[map_text]['color_by'].currentText()

        self.set_colorbar(mapper=mapper, label=label)

        self.render()
        self.set_unsaved_flag()

    def handle_label_format(self, text):

        try:
            text.format(1.34)
            self.time_format = text
            self.set_timelabel(text=self.time_format.format(self.time))
            color = 'black'
        except:
            color = 'red'
        self.visual_btns['time_label']['label_format'].setStyleSheet("color: " + color)
        self.set_unsaved_flag()
