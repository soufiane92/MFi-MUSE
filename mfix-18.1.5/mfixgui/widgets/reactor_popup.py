import logging
import os
import copy
import math

from qtpy import QtWidgets
from mfixgui.tools import get_unique_string
from mfixgui.tools.qt import get_pixmap, widget_iter, get_ui
from mfixgui.widgets.vtk_constants import *

log = logging.getLogger('mfix-gui' if __name__=='__main__' else __name__)


class ReactorPopUp(QtWidgets.QDialog):
    def __init__(self, parent):
        QtWidgets.QDialog.__init__(self, parent)

        self.vtk_widget = parent

        ui = self.ui = get_ui('reactor.ui', self)

        self.setWindowTitle('Reactor Wizard')

        ui.pushbutton_close.clicked.connect(self.close)
        pixmap = get_pixmap('reactor_sketch.png', 199, 349)
        ui.label_image.setPixmap(pixmap)
        ui.lineedit_db.setFocus()

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
                v[name[1]] = widget.value

        # create bed
        b = copy.deepcopy(DEFAULT_PRIMITIVE_PARAMS)
        b['radius'] = v['db']/2.0
        b['height'] = v['hb']
        b['type'] = 'cylinder'
        b['resolution'] = 30
        b_name = self.vtk_widget.add_primitive(
            name=get_unique_string('bed', self.vtk_widget.geometrydict.keys()),
            data=b)

        # create cone
        c = copy.deepcopy(DEFAULT_PRIMITIVE_PARAMS)
        c['radius'] = v['df']/2.0
        cone_angle = math.atan(v['hc']/(v['df']/2.0 - v['db']/2.0))
        add_h = v['db']/2.0 * math.tan(cone_angle)
        c['height'] = v['hc'] + add_h
        c['type'] = 'cone'
        c['resolution'] = 30
        c['rotationz'] = -90
        c['centery'] = v['hb']/2.0 + v['hc']/2.0 - add_h/2.0
        c_name = self.vtk_widget.add_primitive(
            name=get_unique_string('cone', self.vtk_widget.geometrydict.keys()),
            data=c)

        union = self.vtk_widget.boolean_operation(booltype='union', children=[b_name, c_name])

        # create freeboard
        f = copy.deepcopy(DEFAULT_PRIMITIVE_PARAMS)
        f['radius'] = v['df']/2.0
        f['height'] = v['hf']
        f['type'] = 'cylinder'
        f['resolution'] = 30
        f['centery'] = v['hb']/2.0 + v['hc'] + v['hf']/2.0
        f_name = self.vtk_widget.add_primitive(
            name=get_unique_string('freeboard', self.vtk_widget.geometrydict.keys()),
            data=f)

        union = self.vtk_widget.boolean_operation(booltype='union', children=[f_name, union])

        # create top outlet
        if self.ui.groupbox_to.isChecked():
            to = copy.deepcopy(DEFAULT_PRIMITIVE_PARAMS)
            to['radius'] = v['dto']/2.0
            to['height'] = v['hto'] * 2
            to['type'] = 'cylinder'
            to['resolution'] = 20
            to['centery'] = v['hb']/2.0 + v['hc'] + v['hf']
            to_name = self.vtk_widget.add_primitive(
                name=get_unique_string('top_outlet', self.vtk_widget.geometrydict.keys()),
                data=to)

            union = self.vtk_widget.boolean_operation(booltype='union', children=[to_name, union])

        # create side outlet
        if self.ui.groupbox_so.isChecked():
            so = copy.deepcopy(DEFAULT_PRIMITIVE_PARAMS)
            so['radius'] = v['dso']/2.0
            so['height'] = v['hso'] * 2
            so['type'] = 'cylinder'
            so['resolution'] = 20
            so['centery'] = v['hb']/2.0 + v['hc'] + v['hf'] - v['eso']
            so['centerx'] = v['df']/2.0
            so['rotationz'] = -90
            so_name = self.vtk_widget.add_primitive(
                name=get_unique_string('top_outlet', self.vtk_widget.geometrydict.keys()),
                data=so)

            union = self.vtk_widget.boolean_operation(booltype='union', children=[so_name, union])

        # create spout
        if self.ui.groupbox_spouted.isChecked():
            # create cone
            c = copy.deepcopy(DEFAULT_PRIMITIVE_PARAMS)
            c['radius'] = v['db']/2.0
            tan = math.tan(math.radians(v['as']))
            height = v['db']/2.0 * tan
            add_h = v['ds']/2.0 * tan
            c['height'] = height + add_h
            c['type'] = 'cone'
            c['resolution'] = 30
            c['rotationz'] = -90
            c['centery'] = -v['hb']/2.0 -height/2.0 - add_h/2.0
            c_name = self.vtk_widget.add_primitive(
                name=get_unique_string('spout', self.vtk_widget.geometrydict.keys()),
                data=c)

            union = self.vtk_widget.boolean_operation(booltype='union', children=[union, c_name])

            s = copy.deepcopy(DEFAULT_PRIMITIVE_PARAMS)
            s['radius'] = v['ds']/2.0
            s['height'] = v['ds'] * 2
            s['type'] = 'cylinder'
            s['resolution'] = 20
            s['centery'] = -v['hb']/2.0 - height - v['ds'] + add_h/2.0
            s_name = self.vtk_widget.add_primitive(
                name=get_unique_string('top_outlet', self.vtk_widget.geometrydict.keys()),
                data=s)

            union = self.vtk_widget.boolean_operation(booltype='union', children=[s_name, union])

        self.close()
