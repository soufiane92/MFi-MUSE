import logging
import os
import copy
import math

from qtpy import QtWidgets
from mfixgui.tools import get_unique_string
from mfixgui.tools.qt import get_pixmap, widget_iter, get_ui
from mfixgui.widgets.vtk_constants import *

log = logging.getLogger('mfix-gui' if __name__=='__main__' else __name__)


class CyclonePopUp(QtWidgets.QDialog):
    def __init__(self, parent):
        QtWidgets.QDialog.__init__(self, parent)

        self.vtk_widget = parent

        ui = self.ui = get_ui('cyclone.ui', self)

        self.setWindowTitle('Cyclone Wizard')

        ui.pushbutton_close.clicked.connect(self.close)
        pixmap = get_pixmap('cyclone_sketch.png', 175, 306)
        ui.label_image.setPixmap(pixmap)
        ui.lineedit_dc.setFocus()

        ui.pushbutton_apply.clicked.connect(self.apply_)

        for widget in widget_iter(self.ui):
            name = str(widget.objectName()).split('_')
            if 'lineedit' in name:
                widget.dtype = float
                #widget.allow_parameters = True

    def popup(self):

        self.show()
        self.raise_()
        self.activateWindow()

    def apply_(self):

        v = {}
        for widget in widget_iter(self.ui):
            name = str(widget.objectName()).split('_')
            if 'lineedit' in name:
                v[name[1]]  = widget.value

        # create barrel
        b = copy.deepcopy(DEFAULT_PRIMITIVE_PARAMS)
        b['radius'] = v['dc']/2.0
        b['height'] = v['c']
        b['type'] = 'cylinder'
        b['resolution'] = 30
        b_name = self.vtk_widget.add_primitive(
            name=get_unique_string('barrel', self.vtk_widget.geometrydict.keys()),
            data=b)

        # create cone
        c = copy.deepcopy(DEFAULT_PRIMITIVE_PARAMS)
        c['radius'] = v['dc']/2.0
        cone_angle = math.atan(v['b']/(v['dc']/2.0 - v['e']/2.0))
        add_h = v['e']/2.0 * math.tan(cone_angle)
        c['height'] = v['b'] + add_h
        c['type'] = 'cone'
        c['resolution'] = 30
        c['rotationz'] = -90
        c['centery'] = -v['c']/2.0 - v['b']/2.0 - add_h/2.0
        c_name = self.vtk_widget.add_primitive(
            name=get_unique_string('cone', self.vtk_widget.geometrydict.keys()),
            data=c)

        union = self.vtk_widget.boolean_operation(booltype='union', children=[b_name, c_name])

        # create solids outlet
        so = copy.deepcopy(DEFAULT_PRIMITIVE_PARAMS)
        so['radius'] = v['e']/2.0
        so['height'] = v['e']*2
        so['type'] = 'cylinder'
        so['resolution'] = 30
        so['centery'] = -v['e'] - v['c']/2.0 - v['b']
        so_name = self.vtk_widget.add_primitive(
            name=get_unique_string('soilds', self.vtk_widget.geometrydict.keys()),
            data=so)

        union = self.vtk_widget.boolean_operation(booltype='union', children=[union, so_name])

        # inlet
        i = copy.deepcopy(DEFAULT_PRIMITIVE_PARAMS)
        i['lengthx'] = v['l']
        i['lengthy'] = v['k']
        i['lengthz'] = v['dc']
        i['centerx'] = v['l']/2.0 - v['dc']/2.0
        i['centery'] = -v['k']/2.0 + v['c']/2.0 - v['k']/500
        i['centerz'] = v['dc']/2.0
        i['type'] = 'box'
        i_name = self.vtk_widget.add_primitive(
            name=get_unique_string('inlet', self.vtk_widget.geometrydict.keys()),
            data=i)

        union = self.vtk_widget.boolean_operation(booltype='union', children=[union, i_name])

        # clean
        union = self.vtk_widget.add_filter(
            'clean',
            name=get_unique_string('clean', self.vtk_widget.geometrydict.keys()),
            child=union)

        # fill holes
        union = self.vtk_widget.add_filter(
            'fill_holes',
            name=get_unique_string('fill_holes', self.vtk_widget.geometrydict.keys()),
            child=union)


        # vortex finder
        v_c = copy.deepcopy(DEFAULT_PRIMITIVE_PARAMS)
        v_c['radius'] = v['m']/2.0 + v['n']
        v_c['height'] = v['f'] * 2
        v_c['type'] = 'cylinder'
        v_c['resolution'] = 30
        v_c['centery'] = v['c']/2.0
        v_c_name = self.vtk_widget.add_primitive(
            name=get_unique_string('vortex_cut', self.vtk_widget.geometrydict.keys()),
            data=v_c)

        union = self.vtk_widget.boolean_operation(booltype='diff', children=[union, v_c_name])

        # outlet
        o = copy.deepcopy(DEFAULT_PRIMITIVE_PARAMS)
        o['radius'] = v['m']/2.0
        o['height'] = v['f'] * 2 + v['f']/10.0
        o['type'] = 'cylinder'
        o['resolution'] = 30
        o['centery'] = v['c']/2.0 - v['f']/10.0
        o_name = self.vtk_widget.add_primitive(
            name=get_unique_string('outlet', self.vtk_widget.geometrydict.keys()),
            data=o)

        union = self.vtk_widget.boolean_operation(booltype='union', children=[union, o_name])

        self.close()
