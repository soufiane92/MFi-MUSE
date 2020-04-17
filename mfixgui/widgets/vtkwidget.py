"""
This is the vtk widget. It handles displaying the 3D graphics in the GUI as
well as a simple geometry creation tool and region selection.
"""
import os
import copy
import shutil
import logging
from functools import partial
import numpy as np
from qtpy import QtCore, QtGui, QtWidgets

from mfixgui.constants import *
from mfixgui.tools.qt import (widget_iter, get_icon, sub_icon_size,
                           get_image_path, deepcopy_dict)
from mfixgui.tools import (get_unique_string, topological_sort)
from mfixgui.widgets.base import LineEdit, ComboBox, CustomPopUp
from mfixgui.widgets.base_vtk import BaseVtkWidget, vtk, VTK_AVAILABLE, VTK_IMPORT_INFO
try:
    from mfixgui.widgets.distributed_popup import DistributionPopUp
    from mfixgui.widgets.cyclone_popup import CyclonePopUp
    from mfixgui.widgets.reactor_popup import ReactorPopUp
    from mfixgui.widgets.hopper_popup import HopperPopUp
except ImportError:
    DistributionPopUp = None
    CyclonePopUp = None
    ReactorPopUp = None
    HopperPopUp = None
from mfixgui.project import Equation, ExtendedJSON

try:
    from mfixgui.widgets.vtk_constants import *
except ImportError:
    DEFAULT_MESH_NAME = 'geometry.stl'
    VTK_IMPORT_INFO.append('Can not import VTK constants.')

GUI = None
LOG = logging.getLogger(__name__)
CONVERSION = copy.deepcopy(CONVERSION_TO_METERS)
CONVERSION['custom'] = 0

def safe_float(value):
    """try to convert the value to a float, if ValueError, send error to gui
    and return 0"""
    # Note this is slightly different from the function in tools, which does
    #  not have the reporting
    try:
        return float(value)
    except ValueError as error:
        if GUI:
            GUI.error(str(error))
        else:
            LOG.error(str(error))
        return 0.0


def safe_int(value):
    """try to convert the value to a int, if ValueError, send error to gui
    and return 0"""
    try:
        return int(value)
    except ValueError as error:
        if GUI:
            GUI.error(str(error))
        else:
            LOG.error(error)
        return 0


def clean_geo_dict(dirty_dict):
    """clean the geometry dictionary so it can be saved"""
    clean_dict = {}
    for geo, geo_dict in dirty_dict.items():
        new_dict = clean_dict[geo] = {}
        geo_type = None
        if 'geo_type' in geo_dict:
            geo_type = geo_dict['geo_type']
        new_dict['geo_type'] = geo_type
        for key, value in geo_dict.items():
            # filter out vtk objects/default values
            if not isinstance(value, vtk.vtkObject) and (isinstance(value, Equation) or value != DEFAULT_PARAMS[geo_type].get(key, None)):
                new_dict[key] = value

    return clean_dict


def remove_vtk_objects(dirty_dict):
    """given a dictionary, remove vtkObjects from it"""
    clean_dict = {}
    for key, value in dirty_dict.items():
        if not isinstance(value, vtk.vtkObject):
            clean_dict[key] = value
    return clean_dict


def clean_visual_dict(dirty_dict):
    """remove qcolor objects from visual dict and save hex values"""
    clean_dict = {}
    for geo, geo_dict in dirty_dict.items():
        clean_geo = clean_dict[geo] = {}
        for key, _ in geo_dict.items():
            if key in ['color', 'edge']:
                clean_geo[key] = geo_dict[key].name()
            else:
                clean_geo[key] = geo_dict[key]

    return clean_dict


def is_stl_ascii(fname):
    """see if the stl file is ASCII by checking the first line for solid"""
    with open(fname, 'rb') as stlFile:
        try:
            solid = stlFile.readline().strip().lower().startswith(b'solid')

            # verify by checking the second line, issues/500
            if solid:
                solid = stlFile.readline().strip().lower().startswith(b'facet normal')
        except:
            solid = False
    return solid


def purge_multi_solids(fname):
    """Remove multiple solids from an stl file, only works with ascii"""
    # if it already ends in .onesolid.stl, assume cleaned
    if fname.endswith('.onesolid.stl'):
        return fname
    name = os.path.splitext(os.path.basename(fname))[0]
    fname_dir = os.path.dirname(fname)
    newfile = os.path.join(fname_dir, name + '.onesolid.stl')
    multi_solid = 0
    with open(fname, 'r', encoding='utf-8', errors='replace') as input:
        with open(newfile, 'w', encoding='utf-8', errors='replace') as output:
            output.write('solid ascii\n')
            for line in input:
                if 'solid' not in line:
                    output.write(line)
                else:
                    multi_solid += 1
            output.write('endsolid\n')
    if multi_solid > 2:
        LOG.warn('the stl file: %s has multiple solids, removing', fname)
        return newfile
    else:
        os.remove(newfile)
        return fname


def unit_vector(vector):
    """Returns the unit vector of the vector."""
    return vector / np.linalg.norm(vector)


def angle_between(v1, v2):
    """Returns the angle in radians between vectors 'v1' and 'v2'"""
    v1_u = unit_vector(v1)
    v2_u = unit_vector(v2)
    return np.arccos(np.clip(np.dot(v1_u, v2_u), -1.0, 1.0))


class VtkWidget(BaseVtkWidget):
    """the vtk widget"""
    value_updated = QtCore.Signal(object, object, object)

    def __init__(self, parent=None):
        BaseVtkWidget.__init__(self, parent)
        global GUI
        GUI = parent

        self.ui = parent.ui
        ui = self.ui.geometry
        self.geometrytree = self.ui.geometry.treeWidgetGeometry

        # --- data ---
        self.animate = True
        self.geometrydict = {}
        self.region_dict = {}
        self.parameter_key_map = {}

        self.booleanbtndict = {
            'union':        self.ui.geometry.toolbutton_geometry_union,
            'intersection': self.ui.geometry.toolbutton_geometry_intersect,
            'difference':   self.ui.geometry.toolbutton_geometry_difference,
            }
        self.visual_props = deepcopy_dict(DEFAULT_VISUAL_PROPS, qobjects=True)

        self.rectilinear_grid = vtk.vtkRectilinearGrid()
        self.grid_viewer_dict = {
            'filters': [],
            'mappers': [],
            'actors':  [],
            }

        self.cell_spacing_widgets = [
            self.ui.mesh.lineedit_mesh_cells_size_x,
            self.ui.mesh.lineedit_mesh_cells_size_y,
            self.ui.mesh.lineedit_mesh_cells_size_z
            ]

        # enable parameters
        for widget in widget_iter(self.ui.geometry.groupBoxGeometryParameters):
            if isinstance(widget, LineEdit):
                widget.allow_parameters = True

        # ui file widget tweaks
        ui.combobox_stl_units.clear()
        ui.combobox_stl_units.addItems(list(CONVERSION.keys()))
        ui.combobox_stl_units.setCurrentText('m')
        ui.combobox_stl_units.currentIndexChanged.connect(self.handle_stl_units)

        # --- setup vtk mappers/actors ---
        self.mesh = None
        self.mesh_actor = None
        self.mesh_mapper = None

        # set types on lineedits
        for widget in widget_iter(self.ui.geometry.stackedWidgetGeometryDetails):
            if isinstance(widget, LineEdit):
                parameter = str(widget.objectName()).lower().split('_')

                if any([s in parameter for s in ['divisions', 'resolution',
                                                 'nhills', 'iterations']]):
                    widget.dtype = int
                else:
                    widget.dtype = float

        self.__connect_events()
        self.__add_tool_buttons()
        self.__setup_wizards()

        self.ui.geometry.stackedWidgetGeometryDetails.setCurrentIndex(0)

    # --- setup ---
    def __connect_events(self):

        # --- Geometry Tree ---
        self.geometrytree.setStyleSheet(
            "QTreeView::indicator:unchecked {image: url(%s);}"
            "QTreeView::indicator:checked {image: url(%s);}"
            % (get_image_path('visibilityofflight.svg'),
               get_image_path('visibility.svg'))
            )
        self.geometrytree.itemSelectionChanged.connect(
            self.selected_geometry_changed)
        self.geometrytree.itemClicked.connect(self.geometry_clicked)

        # --- geometry button ---
        self.add_geometry_menu = QtWidgets.QMenu(self)
        self.ui.geometry.toolbutton_add_geometry.setMenu(
            self.add_geometry_menu)

        action = QtWidgets.QAction('STL File', self.add_geometry_menu)
        action.triggered.connect(self.add_stl)
        action.setIcon(get_icon('geometry.svg'))
        action.setIconVisibleInMenu(True)
        self.add_geometry_menu.addAction(action)

        # --- implicit functions ---
        p_menu = self.add_geometry_implicit = QtWidgets.QMenu(self)
        p_menu.setTitle('Implicits')
        p_menu.setIcon(get_icon('function.svg'))
        a = self.add_geometry_menu.addMenu(p_menu)
        a.setIconVisibleInMenu(True)
        for geo in IMPLICIT_DICT.keys():
            action = QtWidgets.QAction(geo.replace('_', ' '), p_menu)
            action.triggered.connect(partial(self.add_implicit, implicittype=geo))
            action.setIcon(get_icon('function.svg'))
            action.setIconVisibleInMenu(True)
            p_menu.addAction(action)

        # --- primitives ---
        p_menu = self.add_geometry_primitive = QtWidgets.QMenu(self)
        p_menu.setTitle('Primitives')
        p_menu.setIcon(get_icon('geometry.svg'))
        a = self.add_geometry_menu.addMenu(p_menu)
        a.setIconVisibleInMenu(True)
        for geo in PRIMITIVE_DICT.keys():
            action = QtWidgets.QAction(geo, p_menu)
            action.triggered.connect(partial(self.add_primitive, primtype=geo))
            action.setIcon(get_icon('geometry.svg'))
            action.setIconVisibleInMenu(True)
            p_menu.addAction(action)

        # --- parametric ---
        p_menu = self.add_geometry_parametric = QtWidgets.QMenu(self)
        p_menu.setTitle('Parametrics')
        p_menu.setIcon(get_icon('geometry.svg'))
        a = self.add_geometry_menu.addMenu(p_menu)
        a.setIconVisibleInMenu(True)
        for geo in PARAMETRIC_DICT.keys():
            action = QtWidgets.QAction(geo.replace('_', ' '), p_menu)
            action.triggered.connect(partial(self.add_parametric, paramtype=geo))
            action.setIcon(get_icon('geometry.svg'))
            action.setIconVisibleInMenu(True)
            p_menu.addAction(action)


        # --- filter button ---
        self.add_filter_menu = QtWidgets.QMenu(self)
        self.ui.geometry.toolbutton_add_filter.setMenu(self.add_filter_menu)
        self.add_filter_menu.aboutToShow.connect(self.enable_filters)
        self.filter_actions = []
        for geo in FILTER_DICT.keys():
            action = QtWidgets.QAction(geo.replace('_', ' '), self.add_filter_menu)
            action.triggered.connect(partial(self.add_filter, filtertype=geo))
            action.setIcon(get_icon('filter.svg'))
            action.setIconVisibleInMenu(True)
            self.filter_actions.append(action)
            self.add_filter_menu.addAction(action)

        # setup signals
        self.ui.geometry.toolbutton_remove_geometry.clicked.connect(
            self.remove_geometry)
        self.ui.geometry.toolbutton_copy_geometry.clicked.connect(
            self.handle_copy_geometry)

        # connect boolean
        for key, btn in self.booleanbtndict.items():
            btn.clicked.connect(lambda ignore, k=key: self.boolean_operation(k))

        # connect parameter widgets
        for widget in widget_iter(
                self.ui.geometry.stackedWidgetGeometryDetails):
            if isinstance(widget, QtWidgets.QLineEdit):
                widget.editingFinished.connect(partial(self.handle_widget_value_changed, widget))
            elif isinstance(widget, QtWidgets.QCheckBox):
                widget.stateChanged.connect(lambda i, w=widget: self.handle_widget_value_changed(w))

        # --- mesh ---
        self.ui.geometry.pushbutton_mesh_autosize.clicked.connect(
            self.auto_size_mesh_extents)


    def __add_tool_buttons(self):

        self.init_base_toolbar()

        self.toolbutton_visible = QtWidgets.QToolButton(self)
        self.toolbutton_visible.setToolTip('Change Visibility')
        self.toolbutton_visible.setIcon(get_icon('visibility.svg'))
        self.toolbutton_visible.setIconSize(sub_icon_size())

        self.visible_menu = CustomPopUp(parent=self, button=self.toolbutton_visible)
        self.toolbutton_visible.clicked.connect(self.visible_menu.popup)

        # --- visual representation menu ---
        layout = self.visible_menu.layout
        layout.setContentsMargins(0, 5, 5, 5)
        self.visual_btns = {}
        for i, geo in enumerate(['Background Mesh',  'Geometry', 'Regions']):
            geo_name = geo
            geo = geo.lower().replace(' ', '_')
            btns = self.visual_btns[geo] = {}
            # tool button
            toolbutton = QtWidgets.QToolButton(self.visible_menu)
            toolbutton.clicked.connect(lambda ignore, g=geo, t=toolbutton: self.change_visibility(g, t))
            toolbutton.setCheckable(True)
            toolbutton.setChecked(True)
            toolbutton.setAutoRaise(True)
            toolbutton.setIcon(get_icon('visibility.svg'))
            toolbutton.setIconSize(sub_icon_size())
            layout.addWidget(toolbutton, i, 0)
            btns['visible'] = toolbutton

            # style
            combobox = QtWidgets.QComboBox(self.visible_menu)
            combobox.addItems(['wire', 'solid', 'edges', 'points'])
            combobox.activated.connect(lambda ignore, g=geo, c=combobox: self.change_representation(g, c))
            layout.addWidget(combobox, i, 1)
            btns['rep'] = combobox

            # color
            if not geo == 'regions':
                toolbutton = QtWidgets.QToolButton(self.visible_menu)
                toolbutton.clicked.connect(lambda ignore, g=geo, t=toolbutton: self.change_color(g, t))
                toolbutton.setAutoRaise(True)
                layout.addWidget(toolbutton, i, 2)
                btns['color'] = toolbutton

            # opacity
            opacity = QtWidgets.QDoubleSpinBox(self.visible_menu)
            opacity.setRange(0, 1)
            opacity.setSingleStep(0.1)
            opacity.editingFinished.connect(lambda o=opacity, g=geo: self.change_opacity(o, g))
            layout.addWidget(opacity, i, 3)
            btns['opacity'] = opacity

            # label
            label = QtWidgets.QLabel(geo_name, self.visible_menu)
            layout.addWidget(label, i, 4)
        self.set_visual_btn_values()

        for btn in [self.toolbutton_visible]:
            self.button_bar_layout.addWidget(btn)
            btn.setAutoRaise(True)

        self.button_bar_layout.addStretch()

    def __setup_wizards(self):

        self.cyclone_popup = None
        self.distribution_popup = None
        self.reactor_popup = None
        self.hopper_popup = None

        # --- wizards ---
        wizard_menu = QtWidgets.QMenu('wizards', self)

        action = QtWidgets.QAction('distributed', wizard_menu)
        action.triggered.connect(self.handle_distributed_wizard)
        wizard_menu.addAction(action)

        action = QtWidgets.QAction('cyclone', wizard_menu)
        action.triggered.connect(self.handle_cyclone_wizard)
        wizard_menu.addAction(action)

        action = QtWidgets.QAction('reactor', wizard_menu)
        action.triggered.connect(self.handle_reactor_wizard)
        wizard_menu.addAction(action)

        action = QtWidgets.QAction('hopper', wizard_menu)
        action.triggered.connect(self.handle_hopper_wizard)
        wizard_menu.addAction(action)

        self.ui.geometry.toolbutton_wizard.setMenu(wizard_menu)

    def set_visual_btn_values(self):
        """change the visual btns to be in sync with self.visual_props"""
        for geo, info in self.visual_props.items():
            for key, value in info.items():
                if geo in self.visual_btns and key in self.visual_btns[geo]:
                    wid = self.visual_btns[geo][key]
                    if key == 'rep':
                        wid.setCurrentIndex(wid.findText(value))
                    elif key == 'visible':
                        wid.setChecked(value)
                        self.set_visible_btn_image(wid, value)
                    elif key == 'color':
                        wid.setStyleSheet("QToolButton{{ background: {};}}".format(value.name()))
                    elif key == 'opacity':
                        wid.setValue(value)

    def handle_stl_units(self):
        unit = str(self.ui.geometry.combobox_stl_units.currentText())
        enable = unit == 'custom'
        if not enable:
            self.ui.geometry.lineedit_stl_scale.setText(str(CONVERSION[unit]))
            self.handle_widget_value_changed(self.ui.geometry.lineedit_stl_scale)
        self.handle_widget_value_changed(self.ui.geometry.combobox_stl_units)
        self.ui.geometry.lineedit_stl_scale.setEnabled(enable)

    def emitUpdatedValue(self, key, value, args=None):
        """emit an updates value"""
        self.value_updated.emit(self, {key: value}, args)

    def updateValue(self, key, newValue, args=None):
        """receive keyword changed from project manager"""

        if key == 'no_k':
            self.change_interaction(newValue)

    def objectName(self):
        """return the name of this object"""
        return 'VTK Widget'

    def default(self):
        """reset to defaults"""
        self.ui.geometry.lineedit_keyword_z_max.setEnabled(True)
        self.ui.mesh.lineedit_keyword_kmax.setEnabled(True)
        self.vtkrenderer.RemoveAllViewProps()
        self.clear_all_geometry()
        self.change_interaction()
        self.render()

    # --- save/load ---
    def geometry_to_str(self):
        """convert geometry to string"""

        # save tree
        tree = {}
        itr = QtWidgets.QTreeWidgetItemIterator(self.geometrytree)
        while itr.value():
            item = itr.value()
            text = item.text(0)

            children = tree.setdefault(text, [])
            for i in range(item.childCount()):
                children.append(item.child(i).text(0))
            itr += 1

        data = {'geometry_dict': clean_geo_dict(self.geometrydict),
                'tree': tree}

        return ExtendedJSON.dumps(data)

    def geometry_from_str(self, string):
        """convert string to geometry"""
        data = ExtendedJSON.loads(string)
        tree = data.get('tree')
        tree_copy = copy.deepcopy(tree)
        geo_dict = data.get('geometry_dict')

        if not tree or not data:
            return

        # convert lists to sets
        for key, value in tree.items():
            tree[key] = set(tree[key])

        # build geometry
        for nodes in topological_sort(tree):
            for node in nodes:
                if node in geo_dict and 'geo_type' in geo_dict[node]:
                    geo_data = copy.deepcopy(DEFAULT_PARAMS[geo_dict[node]['geo_type']])
                    geo_data.update(geo_dict[node])
                    geo_type = geo_dict[node]['geo_type']
                    if geo_type == 'primitive':
                        name = self.add_primitive(name=node, data=geo_data, loading=True)
                    elif geo_type == 'implicit':
                        name = self.add_implicit(name=node, data=geo_data, loading=True)
                    elif geo_type == 'parametric':
                        name = self.add_parametric(name=node, data=geo_data, loading=True)
                    elif geo_type == 'filter':
                        name = self.add_filter(name=node, data=geo_data,
                                               child=tree[node].pop(), loading=True)
                    elif geo_type == 'boolean' or geo_type == 'boolean_implicit':
                        name = self.boolean_operation(boolname=node, data=geo_data,
                                                      children=sorted(tree[node], key=tree_copy[node].index),
                                                      loading=True)
                    elif geo_type == 'stl':
                        name = self.add_stl(None, filename=geo_data['filename'],
                                            name=node, data=geo_data, loading=True)

                    if name is None:
                        GUI.message(text='Error loading geometry: {}'.format(node))
                        return

                    # update parameter mapping
                    for key, value in geo_data.items():
                        self.update_parameter_map(value, node, key, check_old=False)

                    if not geo_data['visible']:
                        item = self.get_tree_item(name)
                        item.setCheckState(0, QtCore.Qt.Unchecked)
                        self.geometrydict[name]['actor'].VisibilityOff()
                else:
                    GUI.message(text='Error loading geometry: Geometry does not have parameters.')
                    return

    def visual_props_to_str(self):
        """convert visual props to str"""
        return ExtendedJSON.dumps(clean_visual_dict(self.visual_props))

    def visual_props_from_str(self, string):
        """convert string to visual props"""
        data = ExtendedJSON.loads(string)

        for geo, geo_dict in data.items():
            for key, value in geo_dict.items():
                if key in ['color', 'edge']:
                    if isinstance(value, (list, tuple)):
                        data[geo][key] = QtGui.QColor(*value)
                    else:
                        data[geo][key] = QtGui.QColor(value)

        self.visual_props = deepcopy_dict(DEFAULT_VISUAL_PROPS, qobjects=True)
        self.visual_props.update(data)
        if self.mesh_actor is not None:
            self.set_mesh_actor_props()

        self.set_visual_btn_values()
        for actor in self.grid_viewer_dict['actors']:
            self.set_background_mesh_actor_props(actor)

    def process_quadrics(self, proj):
        # hack to look for a value in a list of values
        def get(keys, default, args):
            for key in keys:
                v = proj.get_value(key, None, args)
                if v is not None:
                    break
            return default if v is None else v

        for qid in proj.get_key_indices('quadric_form'):
            qtype = proj.get_value('quadric_form', '', qid).lower()

            data = copy.deepcopy(DEFAULT_IMPLICIT_PARAMS)
            data['radius'] = proj.get_value('radius', 0, qid)
            data['centerx'] = proj.get_value('t_x', 0, qid)
            data['centery'] = proj.get_value('t_y', 0, qid)
            data['centerz'] = proj.get_value('t_z', 0, qid)
            xe = [get(['piece_xmax', 'clip_xmax'], float(Equation('xmax')), qid),
                  get(['piece_xmin', 'clip_xmin'], float(Equation('xmin')), qid)]
            ye = [get(['piece_ymax', 'clip_ymax'], float(Equation('ymax')), qid),
                  get(['piece_ymin', 'clip_ymin'], float(Equation('ymin')), qid)]
            ze = [get(['piece_zmax', 'clip_zmax'], float(Equation('zmax')), qid),
                  get(['piece_zmin', 'clip_zmin'], float(Equation('zmin')), qid)]

            dx = xe[0] - xe[1]
            dy = ye[0] - ye[1]
            dz = ze[0] - ze[1]

            mx = (xe[0] + xe[1])/2
            my = (ye[0] + ye[1])/2
            mz = (ze[0] + ze[1])/2

            itype = None
            if 'sphere' in qtype:
                itype = 'sphere'
            elif 'cyl' in qtype:
                itype = 'cylinder'
                if qtype.startswith('x'):
                    data['rotationz'] = 90
                    data['height'] = dx
                    data['centerx'] += mx
                elif qtype.startswith('y'):
                    data['height'] = dy
                    data['centery'] += my
                elif qtype.startswith('z'):
                    data['rotationx'] = 90
                    data['height'] = dz
                    data['centerz'] += mz

                # if the height is zero, most likely a 2D simulation, make the
                # height 1 so we can see it.
                if data['height'] == 0:
                    data['height'] = 1
            elif 'cone' in qtype:
                itype = 'cone'

            if itype is not None:
                data['type'] = itype
                self.add_implicit(data=data, name=qtype+'_'+str(qid[0]))

    # --- geometry ---
    def selected_geometry_changed(self):
        """The selected geometry changed, update UI"""
        current_selection = self.geometrytree.selectedItems()
        top_level_items = [self.geometrytree.indexOfTopLevelItem(select) > -1
                           for select in current_selection]

        implicits = ['implicit' in self.geometrydict.get(select.text(0), {}).get('geo_type', '') for select in current_selection]

        # boolean btns
        enableboolbtn = False
        if len(current_selection) == 2 and all(top_level_items) and not any(implicits):
            enableboolbtn = True
        elif len(current_selection) >= 2 and all(top_level_items) and all(implicits):
            enableboolbtn = True
        for btn in self.booleanbtndict.values():
            btn.setEnabled(enableboolbtn)

        # enable/disable delete/copy/filter button
        enables = [False]*3
        if len(current_selection) == 1:
            if all(top_level_items):
                enables = [True]*3
            else:
                enables[2] = True
        elif all(top_level_items):
            enables[0] = True

        for enable, widget in zip(enables, [self.ui.geometry.toolbutton_remove_geometry,
                                            self.ui.geometry.toolbutton_add_filter,
                                            self.ui.geometry.toolbutton_copy_geometry,]):
            widget.setEnabled(enable)

        if current_selection:
            text = str(current_selection[-1].text(0)).lower()
            data = self.geometrydict.get(text)
            type_ = data['type']
            geo_type = data.get('geo_type', None)
            if geo_type == 'implicit':
                type_ += '_implicit'

            new_index = 0
            for i in range(
                    self.ui.geometry.stackedWidgetGeometryDetails.count()):
                widget = self.ui.geometry.stackedWidgetGeometryDetails.widget(i)
                if str(widget.objectName()) == type_:
                    new_index = i
                    break

            # set the widget values
            for child in widget_iter(widget):
                name = str(child.objectName()).lower().split('_')[-1]

                value = data.get(name, None)
                if value is None:
                    continue

                if isinstance(child, LineEdit):
                    child.updateValue(None, value)
                elif isinstance(child, QtWidgets.QCheckBox):
                    child.setChecked(value)
                elif isinstance(child, ComboBox):
                    child.updateValue(None, value)

            self.ui.geometry.groupBoxGeometryParameters.setTitle(text)

        else:
            new_index = 0
            self.ui.geometry.groupBoxGeometryParameters.setTitle('Parameters')
            self.ui.geometry.toolbutton_remove_geometry.setEnabled(False)

        current_index = self.ui.geometry.stackedWidgetGeometryDetails.currentIndex()

        if self.animate:
            GUI.animate_stacked_widget(
                self.ui.geometry.stackedWidgetGeometryDetails,
                current_index,
                new_index,
                'horizontal',
                )

    def get_tree_item(self, name):
        """return the tree item with name"""
        items = self.geometrytree.findItems(name, QtCore.Qt.MatchRecursive, 0)
        if items:
            return items[0]
        else:
            return None

    def geometry_clicked(self, item):
        """Hide/Show the clicked geometry"""

        name = str(item.text(0)).lower()
        geo = self.geometrydict.get(name)
        if geo is None:
            return

        actor = geo.get('actor')
        if not actor:
            return

        if item.checkState(0) == QtCore.Qt.Checked:
            if self.visual_props['geometry']['visible']:
                actor.VisibilityOn()
            geo['visible'] = True
        else:
            actor.VisibilityOff()
            geo['visible'] = False

        self.render()

    def get_stl_extents(self, filename):
        '''given an stl file, return the extents of that file'''

        # purge solids
        if is_stl_ascii(filename):
            filename = purge_multi_solids(filename)

        # reader
        reader = vtk.vtkSTLReader()
        reader.SetFileName(filename)
        reader.MergingOn()
        reader.Update()

        return reader.GetOutput().GetBounds()

    def add_stl(self, widget, filename=None, name=None, data=None, loading=False):
        """Open browse dialog and load selected stl file"""

        proj_dir = os.path.realpath(GUI.get_project_dir())
        if filename is None:
            filename, ignore = QtWidgets.QFileDialog.getOpenFileName(
                self, 'Select an STL File',
                proj_dir,
                'STL File (*.stl)',)

            filename = str(filename)

            if len(filename) == 0:
                return

            # ask to copy stl to current project
            if proj_dir not in os.path.realpath(filename):
                btn = GUI.message(text='The STL file is not in the current project\n'
                    'directory. Copy file to project directory?', buttons=['yes', 'no'],
                    default='no')
                if btn == 'yes':
                    basename = os.path.basename(filename)
                    cfilename = os.path.join(proj_dir, basename)
                    shutil.copy(filename, cfilename)
                    filename = cfilename

        # look for '${proj_dir}' and replace with the project dir
        filename = filename.replace('${proj_dir}', proj_dir)

        # normalize the path issues/504
        filename = os.path.realpath(filename.replace('\\', '/'))

        # check to make sure the file exsits
        if not os.path.exists(filename):
            msg = 'Could not find {}.\nBrowse to new location?'.format(filename)
            btn = GUI.message(text=msg, buttons=['yes', 'no'], default='no')
            new_name = None
            if btn == 'yes':
                new_name = self.add_stl(None, filename=None, name=name, data=data, loading=loading)
            else:
                GUI.warn('Could not find {}, please check geometry'.format(filename))
            return new_name

        if filename:
            # need to handle geometry.stl because the gui will over-write it
            if os.path.basename(filename) == 'geometry.stl':
                old_geometry = filename+'.original'
                # if geometry.stl.original doesn't exist, copy geometry.stl to preserve it.
                if not os.path.exists(old_geometry):
                    shutil.copy(filename, filename+'.original')

                    if data is not None:
                        # the original stl file could have been deleted
                        GUI.warn('geometry.stl.original is missing, please make sure the stl file is transformed correctly')

                # use the original version
                filename = old_geometry

            # purge solids
            if is_stl_ascii(filename):
                filename = purge_multi_solids(filename)

            if name is None:
                name = os.path.basename(filename).lower()
                name = get_unique_string(name, list(self.geometrydict.keys()))

            if data is not None:
                self.geometrydict[name] = data
            else:
                self.geometrydict[name] = copy.deepcopy(DEFAULT_STL_PARAMS)

            geo_data = self.geometrydict.get(name)

            # save relative path if the file is in the project directory
            geo_data['filename'] = os.path.realpath(filename).replace(proj_dir, '${proj_dir}')

            # look for stl keywords, apply and remove from .mfx
            found = False
            for k, keyword in [('translationx', 'tx_stl'), ('translationy', 'ty_stl'), ('translationz', 'tz_stl'),
                               ('scale', 'scale_stl')]:
                if keyword in GUI.project.keyword_dict:
                    found = True
                    if keyword=='scale_stl':
                        geo_data['units'] = 'custom'
                    geo_data[k] = GUI.project.get_value(keyword, default= 1.0 if k=='scale' else 0.0)
                    GUI.unset_keyword(keyword)
            if found:
                geo_data['aboutorigin'] = True
                GUI.set_unsaved_flag()

            # reader
            reader = vtk.vtkSTLReader()
            reader.SetFileName(filename)
            reader.MergingOn()

            # Create transformer
            transform = vtk.vtkTransform()
            transform_filter = vtk.vtkTransformPolyDataFilter()
            transform_filter.SetTransform(transform)
            transform_filter.SetInputConnection(reader.GetOutputPort())
            transform_filter.Update()

            # find center of mass
            center_filter = vtk.vtkCenterOfMass()
            center_filter.SetInputConnection(transform_filter.GetOutputPort())
            center_filter.SetUseScalarsAsWeights(False)
            center_filter.Update()
            center = center_filter.GetCenter()

            bounds = transform_filter.GetOutput().GetBounds()
            for key, bound in zip(['extentxmin', 'extentxmax', 'extentymin', 'extentymax', 'extentzmin', 'extentzmax'],
                                  bounds):
                geo_data[key] = bound

            # mapper
            mapper = vtk.vtkPolyDataMapper()
            mapper.SetInputConnection(transform_filter.GetOutputPort())

            # actor
            actor = vtk.vtkActor()
            actor.SetMapper(mapper)

            self.set_geometry_actor_props(actor, name)
            self.vtkrenderer.AddActor(actor)

            # Add to dict
            geo_data['reader'] = reader
            geo_data['transform'] = transform
            geo_data['transformfilter'] = transform_filter
            geo_data['center_filter'] = center_filter
            geo_data['mapper'] = mapper
            geo_data['actor'] = actor
            geo_data['centerx'] = center[0]
            geo_data['centery'] = center[1]
            geo_data['centerz'] = center[2]

            self.update_transform(name)
            self.render()

            # Add to tree
            item = QtWidgets.QTreeWidgetItem([name])
            item.setIcon(0, get_icon('geometry.svg'))
            item.setFlags(item.flags() | QtCore.Qt.ItemIsUserCheckable)
            item.setCheckState(0, QtCore.Qt.Checked)
            self.geometrytree.addTopLevelItem(item)
            self.geometrytree.setCurrentItem(item)

            if not loading:
                GUI.set_unsaved_flag()

            return name

    def handle_widget_value_changed(self, widget, name=None, value=None, key=None):
        """Update the value of edited parameter in the geometrydict"""

        if name is None:
            current_selection = self.geometrytree.selectedItems()

            if current_selection:
                name = str(current_selection[-1].text(0)).lower()
            value = None

            geo_data = self.geometrydict.get(name)
            if geo_data is None: return

            widget_name = str(widget.objectName()).lower().split('_')[-1]

            if widget_name in geo_data:
                key = widget_name
            else:
                return

            # if widget is a lineedit
            if isinstance(widget, LineEdit):
                value = widget.value
            elif isinstance(widget, QtWidgets.QCheckBox):
                value = widget.isChecked()
            elif isinstance(widget, ComboBox):
                value = widget.value
        else:
            geo_data = self.geometrydict.get(name)

        prev_value = geo_data.get(key, None)
        if value is not None and (prev_value != value or str(prev_value) != str(value)):
            self.update_geometry_value(name, value, key)

    def update_geometry_value(self, name, value, key):
        geo_data = self.geometrydict.get(name)
        self.update_parameter_map(value, name, key)
        geo_data[key] = value
        type_ = geo_data['type']
        geo_type = geo_data.get('geo_type')

        if type_ in PRIMITIVE_DICT and geo_type == 'primitive':
            self.update_primitive(name)
        elif type_ in FILTER_DICT:
            self.update_filter(name)
        elif type_ in PARAMETRIC_DICT:
            self.update_parametric(name)
        elif type_ in IMPLICIT_DICT:
            self.update_implicit(name)

        if 'transform' in geo_data and not geo_type in ['filter', 'implicit']:
            self.update_transform(name)

        GUI.set_unsaved_flag()
        self.render()

    def update_primitive(self, name):
        """Update the specified primitive"""
        primtype = self.geometrydict[name]['type']
        geo = self.geometrydict.get(name)

        if 'source' in self.geometrydict[name]:
            source = geo['source']
        else:
            source = None

        # update source
        if primtype == 'sphere':
            source.SetRadius(safe_float(geo['radius']))
            source.SetThetaResolution(safe_int(geo['thetaresolution']))
            source.SetPhiResolution(safe_int(geo['phiresolution']))
        elif primtype == 'box':
            source.SetXLength(safe_float(geo['lengthx']))
            source.SetYLength(safe_float(geo['lengthy']))
            source.SetZLength(safe_float(geo['lengthz']))
        elif primtype == 'cone':
            source.SetRadius(safe_float(geo['radius']))
            source.SetHeight(safe_float(geo['height']))
            source.SetDirection(safe_float(geo['directionx']),
                                safe_float(geo['directiony']),
                                safe_float(geo['directionz']))
            source.SetResolution(safe_int(geo['resolution']))
            source.CappingOn()
        elif primtype == 'cylinder':
            source.SetRadius(safe_float(geo['radius']))
            source.SetHeight(safe_float(geo['height']))
            source.SetResolution(safe_int(geo['resolution']))
        elif primtype == 'stl':
            pass
        else:
            return

        # common props
        if source is not None:
            source.SetCenter(safe_float(geo['centerx']),
                             safe_float(geo['centery']),
                             safe_float(geo['centerz']))
            source.Update()

        return source

    def update_transform(self, name):
        """Update the specified object's transform filter."""
        geo = self.geometrydict.get(name)
        geo_type = geo['type']
        transform = geo['transform']
        transform_filter = geo['transformfilter']
        about_center_object = not geo.get('aboutorigin', False)

        # reset to Identity
        transform.Identity()
        transform.PostMultiply()

        # translate to center
        if about_center_object:
            transform.Translate(-safe_float(geo['centerx']),
                                -safe_float(geo['centery']),
                                -safe_float(geo['centerz']))

        # scale
        if 'scale' in geo:
            transform.Scale(safe_float(geo['scale']),
                            safe_float(geo['scale']),
                            safe_float(geo['scale']))

        # rotation
        transform.RotateWXYZ(safe_float(geo['rotationx']), 1, 0, 0)
        transform.RotateWXYZ(safe_float(geo['rotationy']), 0, 1, 0)
        transform.RotateWXYZ(safe_float(geo['rotationz']), 0, 0, 1)

        # back to position
        if about_center_object:
            transform.Translate(safe_float(geo['centerx']),
                                safe_float(geo['centery']),
                                safe_float(geo['centerz']))

        # translate stl files
        if geo_type == 'stl' or geo_type in PARAMETRIC_DICT:
            transform.Translate(
                safe_float(geo['translationx']),
                safe_float(geo['translationy']),
                safe_float(geo['translationz']),
                )

        # update
        transform_filter.Update()

        # update stl extents
        if geo_type == 'stl':
            bounds = transform_filter.GetOutput().GetBounds()
            ui = self.ui.geometry
            for key, bound, widget in zip(
                    ['extentxmin', 'extentxmax', 'extentymin', 'extentymax', 'extentzmin', 'extentzmax'],
                    bounds,
                    [ui.lineedit_stl_extentxmin, ui.lineedit_stl_extentxmax,
                     ui.lineedit_stl_extentymin, ui.lineedit_stl_extentymax,
                     ui.lineedit_stl_extentzmin, ui.lineedit_stl_extentzmax]):
                geo[key] = bound
                # manually update stl bounds widgets
                widget.updateValue(None, bound)

        return transform_filter

    def add_primitive(self, primtype='sphere', name=None, data=None, loading=False):
        """Add the specified primitive"""

        # create primitive
        if data is not None:
            primtype = data['type']
        if primtype in PRIMITIVE_DICT:
            source = PRIMITIVE_DICT[primtype]()
        else:
            return

        if name is None:
            name = get_unique_string(primtype, list(self.geometrydict.keys()))

        if data is None:
            self.geometrydict[name] = copy.deepcopy(DEFAULT_PRIMITIVE_PARAMS)
            self.geometrydict[name]['type'] = primtype
            GUI.set_unsaved_flag()
        else:
            self.geometrydict[name] = data
        geo = self.geometrydict.get(name)
        geo['source'] = source

        source = self.update_primitive(name)

        # Create transformer
        transform = vtk.vtkTransform()
        transform_filter = vtk.vtkTransformPolyDataFilter()
        transform_filter.SetTransform(transform)
        transform_filter.SetInputConnection(source.GetOutputPort())

        geo['transform'] = transform
        geo['transformfilter'] = transform_filter

        # update transform
        self.update_transform(name)

        # convert to triangles
        trianglefilter = vtk.vtkTriangleFilter()
        trianglefilter.SetInputConnection(transform_filter.GetOutputPort())

        # Create a mapper
        mapper = vtk.vtkPolyDataMapper()
        mapper.SetInputConnection(trianglefilter.GetOutputPort())

        # Create an actor
        actor = vtk.vtkActor()
        actor.SetMapper(mapper)

        self.set_geometry_actor_props(actor, name)

        self.vtkrenderer.AddActor(actor)

        self.render()

        # add to dict
        geo['mapper'] = mapper
        geo['actor'] = actor
        geo['trianglefilter'] = trianglefilter
        geo['source'] = source

        # Add to tree
        item = QtWidgets.QTreeWidgetItem([name])
        item.setIcon(0, get_icon('geometry.svg'))
        item.setFlags(item.flags() | QtCore.Qt.ItemIsUserCheckable)
        item.setCheckState(0, QtCore.Qt.Checked)
        self.geometrytree.addTopLevelItem(item)
        self.geometrytree.setCurrentItem(item)

        if not loading:
            GUI.set_unsaved_flag()

        return name

    def update_implicit(self, name):
        """update the implicit function"""
        implicittype = self.geometrydict[name]['type']
        geo = self.geometrydict.get(name)

        source = geo.get('source', None)
        sample = geo.get('sample', None)
        surface = geo.get('surface', None)
        transform = geo.get('transform', None)

        x, y, z = safe_float(geo['centerx']), safe_float(geo['centery']), safe_float(geo['centerz'])
        rotx, roty, rotz = safe_float(geo['rotationx']), safe_float(geo['rotationy']), safe_float(geo['rotationz'])
        r = safe_float(geo['radius'])
        h = safe_float(geo['height'])

        # update transform
        if transform:
            # reset to Identity
            transform.Identity()
            transform.PostMultiply()

            # back to position
            transform.Translate(-x, -y, -z)

            # rotation
            transform.RotateWXYZ(rotx, 1, 0, 0)
            transform.RotateWXYZ(roty, 0, 1, 0)
            transform.RotateWXYZ(rotz, 0, 0, 1)

        # update source
        bounds = [safe_float(geo[k]) for k in ['minx','maxx','miny', 'maxy', 'minz', 'maxz']]
        if implicittype == 'sphere':
            source.SetRadius(r)
            bounds = [-r, r, -r, r, -r, r]
        elif implicittype == 'box':
            dx = safe_float(geo['lengthx'])/2.0
            dy = safe_float(geo['lengthy'])/2.0
            dz = safe_float(geo['lengthz'])/2.0
            bounds = [-dx, dx, -dy, dy, -dz, dz]
            source.SetBounds(bounds)
        elif implicittype == 'cone':
            angle = np.rad2deg(np.arctan(r/h))
            geo['cone_source'].SetAngle(angle)
            geo['plane1'].SetOrigin(h, 0, 0)
            geo['plane1'].SetNormal(-1, 0, 0)
            geo['plane2'].SetOrigin(0, 0, 0)
            geo['plane2'].SetNormal(1, 0, 0)
            transform.Translate(h/2.0, 0, 0)
            bounds = [0, h, -r, r, -r, r]
        elif implicittype == 'cylinder':
            geo['cylinder_source'].SetRadius(r)
            geo['plane1'].SetOrigin(x, h/2, z)
            geo['plane1'].SetNormal(0, -1, 0)
            geo['plane2'].SetOrigin(x, -h/2, z)
            geo['plane2'].SetNormal(0, 1, 0)
            bounds = [-r, r, -h/2, h/2, -r, r]
        elif implicittype == 'quadric':
            source.SetCoefficients(
                safe_float(geo['a0']),
                safe_float(geo['a1']),
                safe_float(geo['a2']),
                safe_float(geo['a3']),
                safe_float(geo['a4']),
                safe_float(geo['a5']),
                safe_float(geo['a6']),
                safe_float(geo['a7']),
                safe_float(geo['a8']),
                safe_float(geo['a9']),
                )
        elif implicittype == 'superquadric':
            source.SetPhiRoundness(safe_float(geo['phi']))
            source.SetThetaRoundness(safe_float(geo['theta']))
            source.SetThickness(safe_float(geo['thickness']))
            source.SetSize(r)
            source.SetToroidal(safe_int(geo['toroidal']))
            bounds = [-r, r, -r, r, -r, r]
        else:
            return

        # common props
        if source is not None and hasattr(source, 'Update'):
            source.Update()

        if sample:
            # transform bounds
            t_mat = transform.GetInverse().GetMatrix()
            bounds_list = []
            for dx in bounds[:2]:
                for dy in bounds[2:4]:
                    for dz in bounds[4:]:
                        bounds_list.append(t_mat.MultiplyFloatPoint([dx, dy, dz, 1]))

            xs = [i[0] for i in bounds_list]
            ys = [i[1] for i in bounds_list]
            zs = [i[2] for i in bounds_list]

            bounds = [min(xs), max(xs), min(ys), max(ys), min(zs), max(zs)]
            geo['bounds'] = bounds
            sample.SetModelBounds(*bounds)
            sample.SetSampleDimensions(IMPLICIT_DEFAULT_RES, IMPLICIT_DEFAULT_RES, IMPLICIT_DEFAULT_RES)
            sample.Update()

        if surface:
            surface.Update()

    def add_implicit(self, implicittype=None, name=None, data=None, loading=False):
        """Add an implicit function"""

        # create implicit
        if data is not None:
            implicittype = data['type']
        if implicittype in IMPLICIT_DICT:
            source = IMPLICIT_DICT[implicittype]()
        else:
            return

        if name is None:
            name = get_unique_string(implicittype, list(self.geometrydict.keys()))

        if data is None:
            self.geometrydict[name] = copy.deepcopy(DEFAULT_IMPLICIT_PARAMS)
            self.geometrydict[name]['type'] = implicittype
            GUI.set_unsaved_flag()
        else:
            self.geometrydict[name] = data
        geo = self.geometrydict.get(name)

        # transform
        transform = vtk.vtkTransform()
        source.SetTransform(transform)

        if implicittype == 'cylinder':
            boolean = vtk.vtkImplicitBoolean()
            boolean.SetOperationTypeToDifference()
            boolean.AddFunction(source)
            geo['cylinder_source'] = source
            p1 = geo['plane1'] = vtk.vtkPlane()
            p1.SetTransform(transform)
            boolean.AddFunction(p1)
            p2 = geo['plane2'] = vtk.vtkPlane()
            p2.SetTransform(transform)
            boolean.AddFunction(p2)
            source = boolean
        elif implicittype == 'cone':
            boolean = vtk.vtkImplicitBoolean()
            boolean.SetOperationTypeToDifference()
            boolean.AddFunction(source)
            geo['cone_source'] = source
            p1 = geo['plane1'] = vtk.vtkPlane()
            p1.SetTransform(transform)
            boolean.AddFunction(p1)
            p2 = geo['plane2'] = vtk.vtkPlane()
            p2.SetTransform(transform)
            boolean.AddFunction(p2)
            source = boolean

        sample = vtk.vtkSampleFunction()
        sample.SetSampleDimensions(IMPLICIT_DEFAULT_RES, IMPLICIT_DEFAULT_RES, IMPLICIT_DEFAULT_RES)
        sample.SetImplicitFunction(source)
        sample.ComputeNormalsOff()

        # contour
        surface = vtk.vtkContourFilter()
        surface.SetInputConnection(sample.GetOutputPort())
        surface.SetValue(0, 0.0)

        geo['source'] = source
        geo['sample'] = sample
        geo['surface'] = surface
        geo['transform'] = transform

        self.update_implicit(name)

        # Create a mapper
        mapper = vtk.vtkPolyDataMapper()
        mapper.SetInputConnection(surface.GetOutputPort())
        mapper.ScalarVisibilityOff()

        # Create an actor
        actor = vtk.vtkActor()
        actor.SetMapper(mapper)

        self.set_geometry_actor_props(actor, name)
        self.vtkrenderer.AddActor(actor)
        self.render()

        geo['mapper'] = mapper
        geo['actor'] = actor

        # Add to tree
        item = QtWidgets.QTreeWidgetItem([name])
        item.setIcon(0, get_icon('function.svg'))
        item.setFlags(item.flags() | QtCore.Qt.ItemIsUserCheckable)
        item.setCheckState(0, QtCore.Qt.Checked)
        self.geometrytree.addTopLevelItem(item)
        self.geometrytree.setCurrentItem(item)

        if not loading:
            GUI.set_unsaved_flag()

        return name

    def update_parametric(self, name):
        """Update the specified parameteric object."""
        geo = self.geometrydict.get(name)
        paratype = geo['type']
        para_object = geo['parametric_object']
        source = geo['source']

        if paratype == 'torus':
            para_object.SetRingRadius(safe_float(geo['ringradius']))
            para_object.SetCrossSectionRadius(safe_float(geo['crosssectionradius']))
        elif paratype == 'boy':
            para_object.SetZScale(safe_float(geo['zscale']))
        elif paratype == 'conic_spiral':
            para_object.SetA(safe_float(geo['ascale']))
            para_object.SetB(safe_float(geo['bfunc']))
            para_object.SetC(safe_float(geo['cfunc']))
            para_object.SetN(safe_float(geo['nfunc']))
        elif paratype == 'dini':
            para_object.SetA(safe_float(geo['ascale']))
            para_object.SetB(safe_float(geo['bscale']))
        elif paratype == 'ellipsoid':
            para_object.SetXRadius(safe_float(geo['radiusx']))
            para_object.SetYRadius(safe_float(geo['radiusy']))
            para_object.SetZRadius(safe_float(geo['radiusz']))
        elif paratype == 'figure_8_klein':
            para_object.SetRadius(safe_float(geo['radius']))
        elif paratype == 'mobius':
            para_object.SetRadius(safe_float(geo['radius']))
        elif paratype == 'random_hills':
            para_object.SetHillXVariance(safe_float(geo['variancex']))
            para_object.SetXVarianceScaleFactor(safe_float(geo['scalex']))
            para_object.SetHillYVariance(safe_float(geo['variancey']))
            para_object.SetYVarianceScaleFactor(safe_float(geo['scaley']))
            para_object.SetHillAmplitude(safe_float(geo['amplitude']))
            para_object.SetAmplitudeScaleFactor(safe_float(geo['scaleamplitude']))
            para_object.SetNumberOfHills(safe_int(geo['nhills']))
            para_object.SetAllowRandomGeneration(safe_int(geo['allowrandom']))
        elif paratype == 'roman':
            para_object.SetRadius(safe_float(geo['radius']))
        elif paratype == 'super_ellipsoid':
            para_object.SetXRadius(safe_float(geo['radiusx']))
            para_object.SetYRadius(safe_float(geo['radiusy']))
            para_object.SetZRadius(safe_float(geo['radiusz']))
            para_object.SetN1(safe_float(geo['n1']))
            para_object.SetN2(safe_float(geo['n2']))
        elif paratype == 'super_toroid':
            para_object.SetXRadius(safe_float(geo['radiusx']))
            para_object.SetYRadius(safe_float(geo['radiusy']))
            para_object.SetZRadius(safe_float(geo['radiusz']))
            para_object.SetRingRadius(safe_float(geo['ringradius']))
            para_object.SetCrossSectionRadius(safe_float(geo['crosssectionradius']))
            para_object.SetN1(safe_float(geo['n1']))
            para_object.SetN2(safe_float(geo['n2']))

        source.Update()
        GUI.set_unsaved_flag()
        return source

    def add_parametric(self, paramtype=None, name=None, data=None, loading=False):
        """Add the specified parametric object"""

        if paramtype is None:
            paramtype = data['type']

        if name is None:
            name = get_unique_string(paramtype, list(self.geometrydict.keys()))

        parametric_object = PARAMETRIC_DICT[paramtype]()
        source = vtk.vtkParametricFunctionSource()
        source.SetParametricFunction(parametric_object)

        if data is None:
            self.geometrydict[name] = copy.deepcopy(DEFAULT_PARAMETRIC_PARAMS)
            self.geometrydict[name]['type'] = paramtype
            GUI.set_unsaved_flag()
        else:
            self.geometrydict[name] = data
        geo = self.geometrydict.get(name)
        geo['parametric_object'] = parametric_object
        geo['source'] = source

        source = self.update_parametric(name)

        # Create transformer
        transform = vtk.vtkTransform()
        transform_filter = vtk.vtkTransformPolyDataFilter()
        transform_filter.SetTransform(transform)
        transform_filter.SetInputConnection(source.GetOutputPort())

        geo['transform'] = transform
        geo['transformfilter'] = transform_filter

        # update transform
        self.update_transform(name)

        # convert to triangles
        trianglefilter = vtk.vtkTriangleFilter()
        trianglefilter.SetInputConnection(transform_filter.GetOutputPort())

        # mapper
        mapper = vtk.vtkPolyDataMapper()
        mapper.SetInputConnection(trianglefilter.GetOutputPort())

        # actor
        actor = vtk.vtkActor()
        actor.SetMapper(mapper)

        self.set_geometry_actor_props(actor, name)

        self.vtkrenderer.AddActor(actor)

        self.render()

        # add to dict
        geo['mapper'] = mapper
        geo['actor'] = actor
        geo['trianglefilter'] = trianglefilter
        geo['source'] = source

        # Add to tree
        item = QtWidgets.QTreeWidgetItem([name])
        item.setIcon(0, get_icon('geometry.svg'))
        item.setFlags(item.flags() | QtCore.Qt.ItemIsUserCheckable)
        item.setCheckState(0, QtCore.Qt.Checked)
        self.geometrytree.addTopLevelItem(item)
        self.geometrytree.setCurrentItem(item)

        if not loading:
            GUI.set_unsaved_flag()

        return name

    def boolean_operation(self, booltype=None, boolname=None, data=None,
                          children=None, loading=False):
        """Apply a boolean operation with the currently selected toplevel
        items."""
        if children is not None:
            current_selection = []
            for child in children:
                current_selection.append(self.get_tree_item(child))
        else:
            current_selection = self.geometrytree.selectedItems()

        if boolname is None:
            boolname = get_unique_string(booltype, list(self.geometrydict.keys()))

        # Save references
        if data is not None:
            bool_data = self.geometrydict[boolname] = data
            booltype = data['type']
        else:
            bool_data = self.geometrydict[boolname] = copy.deepcopy(DEFAULT_BOOLEAN_PARAMS)
            bool_data['type'] = booltype
            GUI.set_unsaved_flag()

        implicit = all(['implicit' in self.geometrydict.get(select.text(0)).get('geo_type') for select in current_selection])

        if implicit:
            boolean_operation = vtk.vtkImplicitBoolean()
        else:
            boolean_operation = vtk.vtkBooleanOperationPolyDataFilter()

        if booltype == 'union':
            if implicit:
                boolean_operation.SetOperationTypeToUnion()
            else:
                boolean_operation.SetOperationToUnion()
            icon = 'union'
        elif booltype == 'intersection':
            if implicit:
                boolean_operation.SetOperationTypeToIntersection()
            else:
                boolean_operation.SetOperationToIntersection()
            icon = 'intersect'
        else:
            if implicit:
                boolean_operation.SetOperationTypeToDifference()
            else:
                boolean_operation.SetOperationToDifference()
            icon = 'difference'

        union_list = []
        children = bool_data['children'] = []
        for i, selection in enumerate(current_selection):
            name = str(selection.text(0)).lower()
            children.append(name)
            child = self.geometrydict[name]
            if implicit:
                boolean_operation.AddFunction(child['source'])
                union_list.append(child['bounds'])
            else:
                geometry = self.get_input_data(name)
                boolean_operation.SetInputConnection(
                    i, geometry.GetOutputPort())

            # hide the sources
            child['actor'].VisibilityOff()
            child['visible'] = False

        mapper = vtk.vtkPolyDataMapper()

        if implicit:
            sample = vtk.vtkSampleFunction()
            sample.SetSampleDimensions(IMPLICIT_DEFAULT_RES, IMPLICIT_DEFAULT_RES, IMPLICIT_DEFAULT_RES)
            sample.SetImplicitFunction(boolean_operation)
            sample.ComputeNormalsOff()

            # union the bounds
            array = np.asarray(union_list)
            bounds = []
            for i in range(0, 6, 2):
                bounds += [min(array[:,i]), max(array[:,i+1])]
            sample.SetModelBounds(*bounds)

            # contour
            surface = vtk.vtkContourFilter()
            surface.SetInputConnection(sample.GetOutputPort())
            surface.SetValue(0, 0.0)

            bool_data['sample'] = sample
            bool_data['surface'] = surface
            bool_data['geo_type'] = 'boolean_implicit'
            bool_data['source'] = boolean_operation
            bool_data['bounds'] = bounds

            mapper.SetInputConnection(surface.GetOutputPort())
        else:
            boolean_operation.Update()
            mapper.SetInputConnection(boolean_operation.GetOutputPort())
        mapper.ScalarVisibilityOff()

        actor = vtk.vtkActor()
        actor.SetMapper(mapper)

        self.set_geometry_actor_props(actor, boolname)

        self.vtkrenderer.AddActor(actor)

        self.render()

        # save references
        bool_data['booleanoperation'] = boolean_operation
        bool_data['mapper'] = mapper
        bool_data['actor'] = actor

        # Add to tree
        toplevel = QtWidgets.QTreeWidgetItem([boolname])
        toplevel.setIcon(0, get_icon(icon+'.svg'))
        toplevel.setFlags(toplevel.flags() | QtCore.Qt.ItemIsUserCheckable)
        toplevel.setCheckState(0, QtCore.Qt.Checked)

        # remove children from tree
        for select in current_selection:
            toplevelindex = self.geometrytree.indexOfTopLevelItem(select)
            item = self.geometrytree.takeTopLevelItem(toplevelindex)
            if item:
                item.setCheckState(0, QtCore.Qt.Unchecked)
                toplevel.addChild(item)
        self.geometrytree.addTopLevelItem(toplevel)
        self.geometrytree.setCurrentItem(toplevel)
        if not loading:
            GUI.set_unsaved_flag()
        return boolname

    def clear_all_geometry(self):
        """remove all geometry"""
        self.geometrytree.clear()
        for name, geo in self.geometrydict.items():
            self.remove_from_parameter_map(name, geo)
        self.geometrydict = {}

    def remove_geometry(self):
        """Remove the currently selected geometry, filter, or boolean operation
        """
        currentSelection = self.geometrytree.selectedItems()
        self.animate = False
        for selection in currentSelection:
            text = str(selection.text(0)).lower()
            # remove tree item
            toplevelindex = self.geometrytree.indexOfTopLevelItem(
                selection)
            item = self.geometrytree.takeTopLevelItem(toplevelindex)

            # move children to toplevel, make visible
            children = item.takeChildren()
            for child in children:
                self.geometrytree.addTopLevelItem(child)
                if self.visual_props['geometry']['visible']:
                    geo = self.geometrydict.get(str(child.text(0)).lower())
                    geo['actor'].VisibilityOn()
                    geo['visible'] = True
                child.setCheckState(0, QtCore.Qt.Checked)

            # remove graphics
            geo = self.geometrydict.pop(text)
            self.remove_from_parameter_map(text, geo)
            self.vtkrenderer.RemoveActor(geo['actor'])

        self.animate = True
        if children:
            self.geometrytree.setCurrentItem(children[0])
        else:
            i = self.geometrytree.topLevelItemCount() - 1
            if i >= 0:
                item = self.geometrytree.topLevelItem(i)
                self.geometrytree.setCurrentItem(item)
            else:
                self.selected_geometry_changed()

        GUI.set_unsaved_flag()
        self.render()

    def handle_copy_geometry(self):
        """duplicate the selected geometry"""
        currentSelection = self.geometrytree.selectedItems()
        if currentSelection:
            text = str(currentSelection[-1].text(0)).lower()
            self.copy_geometry(text)

    def copy_geometry(self, name, center=None, rotation=None):
        """given a geometry name, copy it and optionaly change the center or
        rotation"""
        data = copy.deepcopy(remove_vtk_objects(self.geometrydict[name]))
        parent = self.get_tree_item(name)

        if center is not None:
            for key, val in zip(['centerx', 'centery', 'centerz'], center):
                data[key] = val

        if rotation is not None:
            for key, val in zip(['rotationx', 'rotationy', 'rotationz'], rotation):
                data[key] = val

        name = None
        geo_type = data['geo_type']
        if geo_type == 'primitive':
            name = self.add_primitive(data=data)
        elif geo_type == 'parametric':
            name = self.add_parametric(data=data)
        elif geo_type == 'filter':
            # copy the child first
            copy_child = self.copy_geometry(parent.child(0).text(0))
            name = self.add_filter(data=data, child=copy_child)
        elif geo_type == 'boolean' or geo_type == 'boolean_implicit':
            # copy children first
            copy_children = []
            for i in range(parent.childCount()):
                copy_children.append(self.copy_geometry(parent.child(i).text(0)))
            name = self.boolean_operation(booltype=data['type'], data=data,
                                          children=copy_children)
        elif geo_type == 'stl':
            name = self.add_stl(None, filename=data['filename'], data=data)
        elif geo_type == 'implicit':
            name = self.add_implicit(data=data)

        # update parameter mapping
        for key, value in data.items():
            self.update_parameter_map(value, name, key, check_old=False)

        GUI.set_unsaved_flag()
        return name

    def update_filter(self, name):
        """Update the currently selected filter"""
        geo = self.geometrydict.get(name)
        filtertype = geo['type']
        vtkfilter = geo['filter']

        if filtertype == 'clean':
            vtkfilter.SetConvertLinesToPoints(safe_int(geo['linestopoints']))
            vtkfilter.SetConvertPolysToLines(safe_int(geo['polystolines']))
            vtkfilter.SetConvertStripsToPolys(safe_int(geo['stripstopolys']))
        elif filtertype == 'fill_holes':
            vtkfilter.SetHoleSize(safe_float(geo['maximumholesize']))
        elif filtertype == 'triangle':
            vtkfilter.SetPassVerts(safe_int(geo['processvertices']))
            vtkfilter.SetPassLines(safe_int(geo['processlines']))
        elif filtertype == 'decimate':
            vtkfilter.SetTargetReduction(safe_float(geo['targetreduction']))
        elif filtertype == 'quadric_decimation':
            vtkfilter.SetTargetReduction(safe_float(geo['targetreduction']))
        elif filtertype == 'quadric_clustering':
            vtkfilter.SetNumberOfXDivisions(safe_int(geo['divisionsx']))
            vtkfilter.SetNumberOfYDivisions(safe_int(geo['divisionsy']))
            vtkfilter.SetNumberOfZDivisions(safe_int(geo['divisionsz']))
            vtkfilter.SetAutoAdjustNumberOfDivisions(safe_int(geo['autoadjustdivisions']))
        elif filtertype == 'smooth':
            vtkfilter.SetEdgeAngle(safe_float(geo['edgeangle']))
            vtkfilter.SetFeatureAngle(safe_float(geo['featureangle']))
            vtkfilter.SetNumberOfIterations(safe_int(geo['iterations']))
            vtkfilter.SetRelaxationFactor(safe_float(geo['relaxation']))
            vtkfilter.SetFeatureEdgeSmoothing(safe_int(geo['featureedgesmoothing']))
            vtkfilter.SetBoundarySmoothing(safe_int(geo['boundarysmoothing']))
        elif filtertype == 'windowed_sinc':
            vtkfilter.SetEdgeAngle(safe_float(geo['edgeangle']))
            vtkfilter.SetFeatureAngle(safe_float(geo['featureangle']))
            vtkfilter.SetNumberOfIterations(safe_int(geo['iterations']))
            vtkfilter.SetPassBand(safe_float(geo['passband']))
            vtkfilter.SetFeatureEdgeSmoothing(safe_int(geo['featureedgesmoothing']))
            vtkfilter.SetBoundarySmoothing(safe_int(geo['boundarysmoothing']))
            vtkfilter.SetNonManifoldSmoothing(safe_int(geo['manifoldsmoothing']))
            vtkfilter.SetNormalizeCoordinates(safe_int(geo['normalize']))
        elif filtertype == 'reverse_sense':
            vtkfilter.SetReverseCells(safe_int(geo['reversecells']))
            vtkfilter.SetReverseNormals(safe_int(geo['reversenormals']))
        elif filtertype == 'transform':
            transform = geo['transform']
            # reset to Identity
            transform.Identity()
            transform.PostMultiply()

            # find the center
            vtkfilter.Update()
            polydata = vtkfilter.GetInput()
            com = vtk.vtkCenterOfMass()
            com.SetInputData(polydata)
            com.SetUseScalarsAsWeights(False)
            com.Update()
            x, y, z = com.GetCenter()

            # translate to center
            transform.Translate(-x, -y, -z)

            # scale
            transform.Scale(safe_float(geo['scalex']),
                            safe_float(geo['scaley']),
                            safe_float(geo['scalez']))

            # rotation
            transform.RotateWXYZ(safe_float(geo['rotationx']), 1, 0, 0)
            transform.RotateWXYZ(safe_float(geo['rotationy']), 0, 1, 0)
            transform.RotateWXYZ(safe_float(geo['rotationz']), 0, 0, 1)

            # translate
            transform.Translate(x + safe_float(geo['translatex']),
                                y + safe_float(geo['translatey']),
                                z + safe_float(geo['translatez']))
        elif filtertype == 'sample_implicit':
            sample = geo.get('samplefunction')
            sample.SetSampleDimensions(
                safe_int(geo['samplesx']), safe_int(geo['samplesy']), safe_int(geo['samplesz']))
            bounds = [safe_float(geo[k]) for k in ['minx','maxx','miny', 'maxy', 'minz', 'maxz']]
            sample.SetModelBounds(bounds)

            # look for the mapper, if it exists, replace the surface filter
            mapper = geo.get('mapper', None)
            if mapper is not None:
                surf = geo['filter'] = vtk.vtkContourFilter()
                surf.SetInputConnection(sample.GetOutputPort())
                surf.SetValue(0, 0.0)
                mapper.SetInputConnection(surf.GetOutputPort())

        vtkfilter.Update()

    def enable_filters(self):
        """filter menu about to show, enable/disable based on geo selected"""

        current_selection = self.geometrytree.selectedItems()
        if current_selection:
            name = str(current_selection[-1].text(0)).lower()
        else:
            return

        geo_data = self.geometrydict.get(name)
        geo_type = geo_data.get('geo_type')

        enable = [False] + [True]*(len(self.filter_actions)-1)
        if 'implicit' in geo_type:
            enable = [False]*len(self.filter_actions)
            enable[0] = True

        for en, act in zip(enable, self.filter_actions):
            act.setEnabled(en)

    def add_filter(self, filtertype=None, name=None, data=None, child=None, loading=False):
        """add the selected filter with the input being the currently selected
        toplevel item"""

        if child is not None:
            selection_text = child
            current_selection = [self.get_tree_item(child)]
        else:
            current_selection = self.geometrytree.selectedItems()
            if current_selection:
                selection_text = str(current_selection[-1].text(0)).lower()

                name = get_unique_string(
                    filtertype, list(self.geometrydict.keys()))
            else:
                return

        if data is None:
            self.geometrydict[name] = copy.deepcopy(DEFAULT_FILTER_PARAMS)
            self.geometrydict[name]['type'] = filtertype
            GUI.set_unsaved_flag()
        else:
            if name is None:
                name = get_unique_string(data['type'], list(self.geometrydict.keys()))
            self.geometrydict[name] = data

        geo = self.geometrydict.get(name)

        # init filter
        if data is not None:
            filtertype = data['type']
        vtkfilter = geo['filter'] = FILTER_DICT[filtertype]()

        if filtertype == 'transform':
            t = geo['transform'] = vtk.vtkTransform()
            vtkfilter.SetTransform(t)

        # set input data
        if 'implicit' in filtertype:
            sample = vtk.vtkSampleFunction()
            source_data = self.geometrydict.get(selection_text)
            sample.SetImplicitFunction(source_data.get('source'))
            sample.ComputeNormalsOff()
            geo['samplefunction'] = sample
            extents = dict([(k, copy.deepcopy(v)) for k, v in zip(['minx', 'maxx', 'miny', 'maxy', 'minz', 'maxz'], source_data['bounds'])])
            geo.update(extents)
            vtkfilter.SetInputConnection(sample.GetOutputPort())
            vtkfilter.SetValue(0, 0.0)
        else:
            inputdata = self.get_input_data(selection_text)
            vtkfilter.SetInputConnection(inputdata.GetOutputPort())

        # update filter
        self.update_filter(name)

        # hide the source
        self.geometrydict[selection_text]['actor'].VisibilityOff()
        self.geometrydict[selection_text]['visible'] = False

        # Create a mapper
        mapper = vtk.vtkPolyDataMapper()
        mapper.SetInputConnection(vtkfilter.GetOutputPort())
        mapper.ScalarVisibilityOff()

        # Create an actor
        actor = vtk.vtkActor()
        actor.SetMapper(mapper)

        self.set_geometry_actor_props(actor, name)

        # add actor to render
        self.vtkrenderer.AddActor(actor)

        # update
        self.render()

        # save references
        geo['actor'] = actor
        geo['mapper'] = mapper

        self.animate = False
        # Add to tree
        toplevel = QtWidgets.QTreeWidgetItem([name])
        toplevel.setIcon(0, get_icon('filter.svg'))
        toplevel.setFlags(toplevel.flags() | QtCore.Qt.ItemIsUserCheckable)
        toplevel.setCheckState(0, QtCore.Qt.Checked)

        # remove children from tree
        for select in current_selection:
            toplevelindex = self.geometrytree.indexOfTopLevelItem(select)
            item = self.geometrytree.takeTopLevelItem(toplevelindex)
            item.setCheckState(0, QtCore.Qt.Unchecked)
            toplevel.addChild(item)

        self.geometrytree.addTopLevelItem(toplevel)
        self.animate = True
        self.geometrytree.setCurrentItem(toplevel)

        if not loading:
            GUI.set_unsaved_flag()

        return name

    def get_input_data(self, name):
        """based on the type of geometry, return the data"""
        geo = self.geometrydict.get(name)
        if 'trianglefilter' in geo:
            inputdata = geo['trianglefilter']
        elif 'booleanoperation' in geo:
            inputdata = geo['booleanoperation']
        elif 'reader' in geo:
            inputdata = geo['transformfilter']
        elif 'filter' in geo:
            inputdata = geo['filter']
        else:
            raise ValueError('Unknown Geometry: {}'.format(geo))
        return inputdata

    def collect_toplevel_geometry(self):
        """collect and append visible toplevel polydata"""

        append_filter = vtk.vtkAppendPolyData()
        item_count = self.geometrytree.topLevelItemCount()

        for top_num in range(item_count):
            item = self.geometrytree.topLevelItem(top_num)
            name = str(item.text(0))
            geo_type = self.geometrydict.get(name).get('geo_type', '')
            if item.checkState(0) == QtCore.Qt.Checked and not 'implicit' in geo_type:
                append_filter.AddInputData(
                    self.get_input_data(name).GetOutput())

        geo = None
        if append_filter.GetTotalNumberOfInputConnections() > 0:
            append_filter.Update()

            # clean
            clean_filter = vtk.vtkCleanPolyData()
            clean_filter.SetInputConnection(append_filter.GetOutputPort())
            clean_filter.Update()

            geo = clean_filter

        return geo

    def get_geometry_extents(self):
        """determine the extents of the visible geometry"""

        geometry = self.collect_toplevel_geometry()

        bounds = None
        if geometry:
            bounds = geometry.GetOutput().GetBounds()

        return bounds

    def set_geometry_actor_props(self, actor, name):
        """set the geometry proprerties to the others in the scene"""

        props = self.visual_props['geometry']

        self.set_representation(actor, props['rep'])
        actor.GetProperty().SetColor(props['color'].getRgbF()[:3])
        actor.GetProperty().SetEdgeColor(props['edge'].getRgbF()[:3])
        actor.GetProperty().SetOpacity(props['opacity'])

        # check visibility
        if not props['visible']:
            actor.VisibilityOff()

    # --- parameters ---
    def update_parameter_map(self, new_value, name, key, check_old=True):
        """update the mapping of parameters and keywords"""

        data = self.geometrydict
        name_key = ','.join([name, key])

        # new params
        new_params = []
        if isinstance(new_value, Equation):
            new_params = new_value.get_used_parameters()

        # old params
        old_params = []
        if check_old:
            old_value = data[name][key]
            if isinstance(old_value, Equation):
                old_params = old_value.get_used_parameters()

        add = set(new_params)-set(old_params)
        for param in add:
            if param not in self.parameter_key_map:
                self.parameter_key_map[param] = set()
            self.parameter_key_map[param].add(name_key)

        remove = set(old_params)-set(new_params)
        for param in remove:
            self.parameter_key_map[param].remove(name_key)
            if len(self.parameter_key_map[param]) == 0:
                self.parameter_key_map.pop(param)

    def update_parameters(self, params, render=True):
        """parameters have changed, update regions"""
        data = self.geometrydict
        self.defer_render = True
        for param in params:
            if param in self.parameter_key_map:
                for var in self.parameter_key_map[param]:
                    name, key = var.split(',')
                    value = data[name][key]
                    self.update_geometry_value( name, value, key)
        if render:
            self.render(defer_render=False)

    def remove_from_parameter_map(self, name, del_geo):
        for key, value in del_geo.items():
            name_key = ','.join([name, key])
            self._remove_key(name_key, value)

    def _remove_key(self, name_key, value):
        if not isinstance(value, Equation): return

        for param in value.get_used_parameters():
            self.parameter_key_map[param].remove(name_key)
            if len(self.parameter_key_map[param]) == 0:
                self.parameter_key_map.pop(param)

    # --- regions ---
    def update_region_source(self, name):
        """Update the specified primitive"""
        primtype = self.region_dict[name]['type']
        props = self.region_dict[name]

        if 'source' in self.region_dict[name]:
            source = self.region_dict[name]['source']
        else:
            source = None

        lengths = [abs(safe_float(to) - safe_float(from_)) for
                   from_, to in zip(props['from'], props['to'])]
        center = [min(map(safe_float, ft)) + l / 2.0 for ft, l in
                  zip(zip(props['from'], props['to']),
                      lengths)]

        # update source
        if primtype == 'sphere':
            source.SetRadius(min(lengths)/2.0)
        elif primtype == 'point':
            source.SetRadius(.01)
            center = props['from']
        elif primtype == 'box':
            source.SetXLength(lengths[0])
            source.SetYLength(lengths[1])
            source.SetZLength(lengths[2])
        elif primtype == 'XY-plane':
            source.SetXLength(lengths[0])
            source.SetYLength(lengths[1])
            source.SetZLength(0)
            center[2] = props['from'][2]
        elif primtype == 'XZ-plane':
            source.SetXLength(lengths[0])
            source.SetYLength(0)
            source.SetZLength(lengths[2])
            center[1] = props['from'][1]
        elif primtype == 'YZ-plane':
            source.SetXLength(0)
            source.SetYLength(lengths[1])
            source.SetZLength(lengths[2])
            center[0] = props['from'][0]
        elif primtype == 'STL':
            source.SetXLength(lengths[0])
            source.SetYLength(lengths[1])
            source.SetZLength(lengths[2])
        else:
            return #?

        # common props
        if source is not None:
            if all(isinstance(x, (float, int, Equation)) for x in center):
                source.SetCenter(*center)
                source.Update()

        return source

    def new_region(self, name, region):
        """create a new region"""
        self.region_dict[name] = deepcopy_dict(region)

        if region['type'] == 'point':
            shape = 'sphere'
        else:
            shape = 'box'

        source = PRIMITIVE_DICT[shape]()
        self.region_dict[name]['source'] = source
        self.update_region_source(name)

        # Create a mapper
        mapper = vtk.vtkPolyDataMapper()
        mapper.SetInputConnection(source.GetOutputPort())

        # Create an actor
        actor = vtk.vtkActor()
        actor.SetMapper(mapper)
        self.set_region_actor_props(actor, name, region['color'].color_float)

        self.vtkrenderer.AddActor(actor)
#        self.balloon_widget.AddBalloon(actor, name, None)

        self.region_dict[name]['actor'] = actor
        self.region_dict[name]['mapper'] = mapper
        self.select_facets(name)

        self.change_region_visibility(
            name, self.region_dict[name]['visibility'],)

    def delete_region(self, name):
        """delete a region"""
        region = self.region_dict.pop(name)
        self.vtkrenderer.RemoveActor(region['actor'])
        if 'clip_actor' in region:
            self.vtkrenderer.RemoveActor(region['clip_actor'])

    def update_region(self, name, region):
        """update a region"""
        self.region_dict[name].update(deepcopy_dict(region))
        self.update_region_source(name)
        self.select_facets(name)
        self.render()

    def change_region_color(self, name, color):
        """change the color of a region"""
        reg = self.region_dict.get(name)
        reg['color'] = copy.deepcopy(color)

        actor = reg['actor']
        actor.GetProperty().SetColor(*reg['color'].color_float)

        if 'clip_actor' in self.region_dict[name]:
            actor = reg['clip_actor']
            actor.GetProperty().SetColor(*reg['color'].color_float)

        self.render()

    def change_region_type(self, name, region):
        """change the type of a region"""

        self.region_dict[name].update(deepcopy_dict(region))

        if region['type'] == 'point':
            shape = 'sphere'
        else:
            shape = 'box'

        source = PRIMITIVE_DICT[shape]()
        region_data = self.region_dict.get(name)
        region_data['source'] = source
        self.update_region_source(name)
        self.select_facets(name)
        region_data['mapper'].SetInputConnection(source.GetOutputPort())

        self.render()

    def change_region_name(self, old_name, new_name):
        """change the name of a region"""
        region = self.region_dict.pop(old_name)
        self.region_dict[new_name] = region

    def change_region_visibility(self, name, visible):
        """change the visibility of a region"""
        reg = self.region_dict.get(name)
        if visible and self.visual_props['regions']['visible']:
            reg['actor'].VisibilityOn()
            if 'clip_actor' in reg:
                reg['clip_actor'].VisibilityOn()
        else:
            reg['actor'].VisibilityOff()
            if 'clip_actor' in reg:
                reg['clip_actor'].VisibilityOff()
        reg['visible'] = visible

        self.render()

    def set_region_actor_props(self, actor, name, color=None):
        """set the geometry properties to the others in the scene"""

        props = self.visual_props['regions']
        self.set_representation(actor, props['rep'])
        actor.GetProperty().SetOpacity(props['opacity'])

        if color:
            actor.GetProperty().SetColor(*color)
        else:
            actor.GetProperty().SetColor(props['color'].getRgbF()[:3])

        # check visibility
        if not props['visible']:
            actor.VisibilityOff()

    def select_facets(self, name):
        """select facets with an implicit function"""

        region = self.region_dict[name]
        # remove old objects
        if 'clip_actor' in region:
            self.vtkrenderer.RemoveActor(region['clip_actor'])
            for key in ['clip_actor', 'clip_mapper', 'clipper', 'implicit']:
                region.pop(key)

        if region['type'].lower() != 'stl':
            self.ui.regions.set_facet_number(name, 0)
            return

        # collect the bounds
        bounds = [0.0]*6
        # make sure the from < to
        zipped = list(zip(region['from'], region['to']))
        bounds[::2] = [min(f, t) for f, t in zipped]
        bounds[1::2] = [max(f, t) for f, t in zipped]

        lengths = [abs(safe_float(to) - safe_float(f)) for
                   f, to in zip(region['from'], region['to'])]
        center = [min(f) + l / 2.0 for f, l in
                  zip(zip(region['from'], region['to']),
                      lengths)]

        # check for geometry
        geo = self.collect_toplevel_geometry()
        n_facets = 0
        if geo is not None:

            # create the implicit
            implicit = None
            stl_select_shape = region['stl_shape']
            if stl_select_shape == 'box':
                implicit = vtk.vtkBox()
                implicit.SetBounds(bounds)
            elif stl_select_shape == 'ellipsoid':
                trans = vtk.vtkTransform()
                # for some reason, everything is inverted?
                trans.Scale([1/l if l > 0 else 1 for l in lengths])
                trans.Translate([-c for c in center])
                implicit = vtk.vtkSphere()
                implicit.SetTransform(trans)
            else:
                implicit = vtk.vtkBox()
                extents = [e+o for e, o in zip(self.get_geometry_extents(), (-1, 1, -1, 1, -1, 1))]
                implicit.SetBounds(extents)

            # Slice
            if region['slice']:
                clipper = vtk.vtkClipPolyData()
                clipper.SetClipFunction(implicit)
                clipper.SetInputConnection(geo.GetOutputPort())
                clipper.GenerateClippedOutputOn()
                clipper.Update()
                n_facets = clipper.GetClippedOutput().GetNumberOfCells()
                clipper_output = clipper.GetClippedOutputPort()
            # select
            else:
                clipper = vtk.vtkExtractPolyDataGeometry()
                clipper.SetImplicitFunction(implicit)
                clipper.SetInputConnection(geo.GetOutputPort())
                clipper.ExtractInsideOn()
                clipper.Update()
                n_facets = clipper.GetOutput().GetNumberOfCells()
                clipper_output = clipper.GetOutputPort()

            # -- filter normals --
            if region.get('filter_facets', False):
                # calculate normals
                normal_generator = vtk.vtkPolyDataNormals()
                normal_generator.SetInputConnection(clipper_output)
                normal_generator.ComputePointNormalsOff()
                normal_generator.ComputeCellNormalsOn()
                normal_generator.Update()
                normals = normal_generator.GetOutput().GetCellData().GetArray("Normals")

                if normals is not None:
                    # build list of cells to keep
                    ids = vtk.vtkIdTypeArray()
                    ids.SetNumberOfComponents(1)
                    filter_vet = np.asarray(region.get('filter', [0, 1, 0]))
                    dev_angle = region.get('deviation_angle', 10)
                    equil = region.get('equilibrant', False)
                    invert = region.get('invert', False)
                    for i in range(normals.GetNumberOfTuples()):
                        normal = normals.GetTuple3(i)
                        angle = np.degrees(angle_between(normal, filter_vet))

                        ind = None
                        if angle < dev_angle:
                            ind = i
                        elif equil:
                            angle = np.degrees(angle_between(normal, -filter_vet))
                            if angle < dev_angle:
                                ind = i

                        if ind is not None and not invert:
                            ids.InsertNextValue(ind)
                        elif ind is None and invert:
                            ids.InsertNextValue(i)

                    # build selection
                    selection_node = vtk.vtkSelectionNode()
                    selection_node.SetFieldType(vtk.vtkSelectionNode.CELL)
                    selection_node.SetContentType(vtk.vtkSelectionNode.INDICES)
                    selection_node.SetSelectionList(ids)
                    selection = vtk.vtkSelection()
                    selection.AddNode(selection_node)

                    # extract the selected cells
                    extract_selection = vtk.vtkExtractSelectedIds()
                    extract_selection.SetInputConnection(0, clipper_output)
                    extract_selection.SetInputData(1, selection)
                    extract_selection.Update()
                    n_facets = extract_selection.GetOutput().GetNumberOfCells()

                    # convert from unstructed grid to polydata
                    # This step seems strange
                    surface = vtk.vtkDataSetSurfaceFilter()
                    surface.SetInputConnection(extract_selection.GetOutputPort())
                    clipper_output = surface.GetOutputPort()

            clip_mapper = vtk.vtkPolyDataMapper()
            clip_mapper.SetInputConnection(clipper_output)
            clip_mapper.ScalarVisibilityOff()
            clip_actor = vtk.vtkActor()
            clip_actor.SetMapper(clip_mapper)

            # save
            region['implicit'] = implicit
            region['clipper'] = clipper
            region['clip_mapper'] = clip_mapper
            region['clip_actor'] = clip_actor
            region['actor'].GetProperty().SetRepresentationToWireframe()

            self.set_region_actor_props(clip_actor, name,
                                        color=region['color'].color_float)

            self.vtkrenderer.AddActor(clip_actor)

            self.ui.regions.set_facet_number(name, n_facets)

    # --- output files ---
    def export_stl(self, file_name):
        """export visible toplevel geometry"""

        # Avoid problems with Unicode in user's home directory,
        #  since VTK does not handle Unicode filenames
        file_name = os.path.relpath(file_name)
        basename = os.path.splitext(file_name)[0]

        # Export geometry
        geometry = self.collect_toplevel_geometry()
        if geometry:
            GUI.update_keyword('cartesian_grid', True)
            GUI.update_keyword('use_stl', True)
            # write file
            stl_writer = vtk.vtkSTLWriter()
            stl_writer.SetFileName(file_name)
            stl_writer.SetInputConnection(geometry.GetOutputPort())
            stl_writer.Write()
        elif GUI.project.get_value('n_quadric', None) is not None:
            GUI.update_keyword('cartesian_grid', True)
        elif GUI.project.get_value('use_msh', False):
            GUI.update_keyword('cartesian_grid', True)
        elif GUI.project.get_value('use_polygon', False):
            GUI.update_keyword('cartesian_grid', True)
        else:
            GUI.update_keyword('cartesian_grid', False)
            GUI.update_keyword('use_stl', False)

        # write name_000n.stl for stl regions
        if GUI.bcs:
            for i, bc_data in GUI.bcs.items():
                region = bc_data['region']
                data = {}

                if region in self.region_dict:
                    data = self.region_dict[region]
                else:
                    continue
                if data['type'] == 'STL' and 'clipper' in data:
                    fname = basename + '_{0:04d}.stl'.format(i)
                    stl_writer = vtk.vtkSTLWriter()
                    stl_writer.SetFileName(fname)
                    input_connection = (data['clipper'].GetClippedOutputPort() if data['slice'] else
                                        data['clipper'].GetOutputPort())
                    stl_writer.SetInputConnection(input_connection)
                    if not stl_writer.Write():
                        msg = f"Error: VTK failed to write {fname}"
                        LOG.warning(msg)
                        GUI.message(text=msg)

    def export_unstructured(self, fname, grid):
        """export an unstructured grid"""
        gw = vtk.vtkXMLUnstructuredGridWriter()
        gw.SetFileName(fname)
        gw.SetInputConnection(grid)
        gw.Write()

    # --- mesh ---
    def auto_size_mesh_extents(self):
        """collect and set the extents of the visible geometry"""
        extents = self.get_geometry_extents()

        if extents:
            for key, extent in zip(['x_min', 'x_max', 'y_min', 'y_max',
                                    'z_min', 'z_max'],
                                   extents):
                self.emitUpdatedValue(key, extent)

#            self.update_background_mesh()

    def update_background_mesh(self, spacing):
        """update the background mesh"""
        cells = [len(c) for c in spacing]
        self.rectilinear_grid.SetDimensions(*cells)

        x_coords = vtk.vtkFloatArray()
        for i in spacing[0]:
            x_coords.InsertNextValue(i)

        y_coords = vtk.vtkFloatArray()
        for i in spacing[1]:
            y_coords.InsertNextValue(i)

        z_coords = vtk.vtkFloatArray()
        for i in spacing[2]:
            z_coords.InsertNextValue(i)

        self.rectilinear_grid.SetXCoordinates(x_coords)
        self.rectilinear_grid.SetYCoordinates(y_coords)
        self.rectilinear_grid.SetZCoordinates(z_coords)

        # remove exsisting
        for actor in self.grid_viewer_dict['actors']:
            self.vtkrenderer.RemoveActor(actor)

        self.grid_viewer_dict['filters'] = []
        self.grid_viewer_dict['actors'] = []
        self.grid_viewer_dict['mappers'] = []

        # add new actors
        for i in range(3):
            filter_ = vtk.vtkRectilinearGridGeometryFilter()
            filter_.SetInputData(self.rectilinear_grid)

            if i == 0:
                filter_.SetExtent(0, 0, 0, cells[1], 0, cells[2])
            elif i == 1:
                filter_.SetExtent(0, cells[0], 0, 0, 0, cells[2])
            else:
                filter_.SetExtent(0, cells[0], 0, cells[1], 0, 0)
            filter_.Update()

            self.grid_viewer_dict['filters'].append(filter_)
            mapper = vtk.vtkPolyDataMapper()
            mapper.SetInputConnection(filter_.GetOutputPort())
            mapper.ScalarVisibilityOff()
            self.grid_viewer_dict['mappers'].append(mapper)

            actor = vtk.vtkActor()
            actor.SetMapper(mapper)
            self.set_background_mesh_actor_props(actor)
            self.grid_viewer_dict['actors'].append(actor)

            self.vtkrenderer.AddActor(actor)

        self.render()

    def set_background_mesh_actor_props(self, actor):
        """set the background mesh properties"""
        props = self.visual_props['background_mesh']
        self.set_representation(actor, props['rep'])
        actor.GetProperty().SetColor(props['color'].getRgbF()[:3])
        actor.GetProperty().SetEdgeColor(props['color'].getRgbF()[:3])
        actor.GetProperty().SetOpacity(props['opacity'])
        actor.SetVisibility(int(props['visible']))

#    def vtk_calc_distance_from_geometry(self):
#        """for every point in the mesh, calculate the distance from the
#        toplevel geometry"""
#        source = self.collect_toplevel_geometry()
#
#        if source:
#            signed_distances = vtk.vtkFloatArray()
#            signed_distances.SetNumberOfComponents(1)
#            signed_distances.SetName("SignedDistances")
#
#            implicit_poly_data_distance = vtk.vtkImplicitPolyDataDistance()
#
#            implicit_poly_data_distance.SetInput(source.GetOutput())
#
#            # Evaluate the signed distance function at all of the grid points
#            for point_id in range(self.rectilinear_grid.GetNumberOfPoints()):
#                p = self.rectilinear_grid.GetPoint(point_id)
#                signed_distance = implicit_poly_data_distance.EvaluateFunction(p)
#                signed_distances.InsertNextValue(signed_distance)
#
#            self.rectilinear_grid.GetPointData().SetScalars(signed_distances)
#
#    def vtk_mesher_table_based(self):
#        """use vtkTableBasedClipDataSet to slice the background mesh with the
#        toplevel geometry"""
#        self.vtk_calc_distance_from_geometry()
#
#        clipper = vtk.vtkTableBasedClipDataSet()
#        clipper.SetInputData(self.rectilinear_grid)
#        if self.ui.mesh.checkbox_mesh_inside.isChecked():
#            clipper.InsideOutOn()
#        else:
#            clipper.InsideOutOff()
#
#        clipper.SetMergeTolerance(
#            safe_float(self.ui.mesh.lineedit_vtk_mesh_merge.text())
#            )
#        clipper.SetValue(0.0)
#        clipper.Update()
#        self.export_mesh(clipper)
#
#        self.mesh = clipper.GetOutput()
#        self.show_mesh()
#        self.mesh_stats()
#
#    def vtk_mesher_threshold(self):
#        """use vtkThreshold to slice the background mesh with the
#        toplevel geometry"""
#        self.vtk_calc_distance_from_geometry()
#
#        thresh = vtk.vtkThreshold()
#        thresh.SetInputData(self.rectilinear_grid)
#
#        if self.ui.mesh.checkbox_threshold_inside.isChecked():
#            thresh.ThresholdByLower(0)
#        else:
#            thresh.ThresholdByUpper(0)
#
#        if self.ui.mesh.checkbox_threshold_interface.isChecked():
#            thresh.AllScalarsOff()
#        else:
#            thresh.AllScalarsOn()
#
#        thresh.Update()
#        self.export_mesh(thresh)
#
#        self.mesh = thresh.GetOutput()
#        self.show_mesh()
#        self.mesh_stats()
#
#    def export_mesh(self, mesh, name=DEFAULT_MESH_NAME):
#        """export the mesh"""
#        project_dir = GUI.get_project_dir()
#        self.export_unstructured(os.path.join(project_dir, name),
#                                 mesh.GetOutputPort())
#
#    def mesh_stats(self):
#        """calculate mesh statistics"""
#        cell_count = self.mesh.GetNumberOfCells()
#        cell_types = self.mesh.GetCellTypesArray()
#
#        if cell_types is None or cell_count is None:
#            return
#
#        cell_list = []
#        for i in range(cell_types.GetNumberOfTuples()):
#            cell_list.append(cell_types.GetTuple(i))
#
#        cell_type_counts = []
#        for cell in set(cell_list):
#            cell_type_counts.append([
#                CELL_TYPE_ENUM[int(cell[0])],
#                cell_list.count(cell)
#                ])
#
#        self.ui.mesh.lineedit_mesh_cells.setText(str(cell_count))
#
#        self.ui.mesh.plaintextedit_mesh_cell_types.setPlainText(
#            '\n'.join([':\t'.join([str(cell_type), str(cells)])
#                       for cell_type, cells in cell_type_counts])
#            )
#
#    def mesher(self):
#        """mesh the geometry"""
#        mesher = str(self.ui.mesh.combobox_mesher.currentText())
#
#        if mesher == 'vtkTableBasedClipDataSet':
#            self.vtk_mesher_table_based()
#        elif mesher == 'vtkThreshold':
#            self.vtk_mesher_threshold()
#
#    def remove_mesh(self, name=DEFAULT_MESH_NAME):
#        """remove the mesh from the vtk scene as well as the file"""
#        project_dir = GUI.get_project_dir()
#        path = os.path.join(project_dir, name)
#        if not os.path.exists(path):
#            return
#        key = GUI.message(text='Remove %s?' % name, buttons=['yes', 'no'], default='no')
#        if key == 'no':
#            return
#
#        os.remove(path)
#
#        if self.mesh_actor is not None:
#            self.vtkrenderer.RemoveActor(self.mesh_actor)
#            self.mesh_actor = None
#
#    def set_mesh_actor_props(self):
#        """change the actor properties of the mesh"""
#        props = self.visual_props['mesh']
#        self.set_representation(self.mesh_actor, props['rep'])
#        self.mesh_actor.GetProperty().SetColor(props['color'].getRgbF()[:3])
#        self.mesh_actor.GetProperty().SetEdgeColor(props['edge'].getRgbF()[:3])
#        self.mesh_actor.SetVisibility(int(props['visible']))
#
#    def show_mesh(self):
#        """show the mesh"""
#        if self.mesh is None:
#            return
#
#        if self.mesh_actor is not None:
#            self.vtkrenderer.RemoveActor(self.mesh_actor)
#        self.mesh_mapper = vtk.vtkDataSetMapper()
#        self.mesh_mapper.ScalarVisibilityOff()
#        self.mesh_mapper.SetInputData(self.mesh)
#        self.mesh_actor = vtk.vtkActor()
#        self.mesh_actor.SetMapper(self.mesh_mapper)
#        self.vtkrenderer.AddActor(self.mesh_actor)
#        self.set_mesh_actor_props()
#        self.render()

    # --- view ---
    def change_visibility(self, name, toolbutton):
        """change the visibility of one of the scene objects"""
        actors = None
        if name == 'mesh':
            actors = [self.mesh_actor]
        elif name == 'background_mesh':
            actors = self.grid_viewer_dict['actors']
        elif name == 'geometry':
            actors = [geo['actor'] for geo in self.geometrydict.values()
                      if geo['visible']]
        elif name == 'regions':
            actors = [geo['actor'] for geo in self.region_dict.values()
                      if geo['visible']]

            actors += [geo['clip_actor'] for geo in self.region_dict.values()
                       if 'clip_actor' in geo and geo['visible']]

        visible = self.visual_props[name]['visible'] = toolbutton.isChecked()
        self.set_visible_btn_image(toolbutton, visible)
        if actors is not None:
            for actor in actors:
                if actor is not None:
                    if visible:
                        actor.VisibilityOn()
                    else:
                        actor.VisibilityOff()
            self.render()

    def get_actors(self, name):
        """given a scene actor type, return the actors"""
        actors = []
        if name == 'mesh':
            actors = [self.mesh_actor]
        elif name == 'background_mesh':
            actors = self.grid_viewer_dict['actors']
        elif name == 'geometry':
            actors = [geo['actor'] for geo in self.geometrydict.values()]
        elif name == 'regions':
            actors = [geo['actor'] for geo in self.region_dict.values()]
            actors += [geo['clip_actor'] for geo in self.region_dict.values()
                       if 'clip_actor' in geo]
        for actor in actors:
            yield actor

    def set_representation(self, actor, rep):
        """set the representation of an actor"""
        if actor is None: return
        if rep == 'wire':
            actor.GetProperty().SetRepresentationToWireframe()
        elif rep == 'solid':
            actor.GetProperty().SetRepresentationToSurface()
            actor.GetProperty().EdgeVisibilityOff()
        elif rep == 'edges':
            actor.GetProperty().SetRepresentationToSurface()
            actor.GetProperty().EdgeVisibilityOn()
        elif rep == 'points':
            actor.GetProperty().SetRepresentationToPoints()

    def change_representation(self, name, combobox):
        """given a scene actor type, change the representation"""
        representation = str(combobox.currentText())

        self.visual_props[name]['rep'] = representation

        for actor in self.get_actors(name):
            self.set_representation(actor, representation)

        self.render()

    def change_color(self, name, button):
        """given a scene actor type, change the color"""
        col = QtWidgets.QColorDialog.getColor(parent=self)
        if not col.isValid():
            return

        button.setStyleSheet("QToolButton{{ background: {};}}".format(
            col.name()))

        self.visual_props[name]['color'] = col
        dark = self.visual_props[name]['edge'] = col.darker()

        for actor in self.get_actors(name):
            if actor is not None:
                actor.GetProperty().SetColor(col.getRgbF()[:3])
                actor.GetProperty().SetEdgeColor(dark.getRgbF()[:3])

        self.render()

    def change_opacity(self, opacity, name):
        """given a scene actor type, change the opacity"""
        opacity = opacity.value()
        self.visual_props[name]['opacity'] = opacity

        for actor in self.get_actors(name):
            if actor is not None:
                actor.GetProperty().SetOpacity(opacity)
        self.render()

    def set_visible_btn_image(self, btn, checked):
        """given a button, change the icon"""
        if not checked:
            btn.setIcon(get_icon('visibilityofflight.svg'))
        else:
            btn.setIcon(get_icon('visibility.svg'))

    # --- wizards ---
    def handle_cyclone_wizard(self):
        """show the cyclone wizard"""
        if self.cyclone_popup is None:
            self.cyclone_popup = CyclonePopUp(self)
        self.cyclone_popup.popup()

    def handle_distributed_wizard(self):
        """show the distributed wizard"""
        if self.distribution_popup is None:
            self.distribution_popup = DistributionPopUp(self)
        self.distribution_popup.popup()

    def handle_reactor_wizard(self):
        """show the reactor wizard"""
        if self.reactor_popup is None:
            self.reactor_popup = ReactorPopUp(self)
        self.reactor_popup.popup()

    def handle_hopper_wizard(self):
        """show the hopper wizard"""
        if self.hopper_popup is None:
            self.hopper_popup = HopperPopUp(self)
        self.hopper_popup.popup()
