import logging
import os
import vtk
import numpy as np

from qtpy.QtWidgets import QDialog

from mfixgui.tools.qt import get_ui

log = logging.getLogger('mfix-gui' if __name__=='__main__' else __name__)

class DistributionPopUp(QDialog):
    def __init__(self, parent):
        QDialog.__init__(self, parent)

        self.vtk_widget = parent

        ui = self.ui = get_ui('distributed.ui', self)

        self.setWindowTitle('Distribution Wizard')

        ui.pushbutton_close.clicked.connect(self.close)
        ui.pushbutton_apply.clicked.connect(self.apply_)
        ui.combobox_distribution.currentIndexChanged.connect(self.dist_changed)
        ui.pushbutton_close.setFocus(False)


    def dist_changed(self):
        enable = self.ui.combobox_distribution.currentText() == 'random'
        if enable:
            self.ui.label_numberspacing.setText('number')
        else:
            self.ui.label_numberspacing.setText('spacing')

    def popup(self):
        geo = list(self.vtk_widget.geometrydict.keys())

        self.ui.combobox_shape.clear()
        self.ui.combobox_shape.addItems(geo)

        self.ui.combobox_volume.clear()
        self.ui.combobox_volume.addItems(geo)

        self.show()
        self.raise_()
        self.activateWindow()

    def apply_(self):

        geo = self.ui.combobox_volume.currentText()
        shape = self.ui.combobox_shape.currentText()
        packing = self.ui.combobox_distribution.currentText()
        bounds = self.get_bounds(geo)
        try:
            spacing = float(self.ui.lineedit_number.value)
        except ValueError:
            spacing = 1
        if  packing == 'random':
            self.gen_random(geo, shape, bounds)
        elif packing == 'cubic':
            self.gen_cubic(geo, shape, spacing, bounds)
        elif packing == 'body centered cubic':
            self.gen_bcc(geo, shape, spacing, bounds)

#        self.close()

    def gen_random(self, geo, shape, bounds):

        n_points = 0
        max_itr = 0
        last = None
        while n_points < int(self.ui.lineedit_number.text()) and max_itr <1000000000:
            point = np.random.random(3)
            point = [p*(t-f)+f for p, f, t in zip(point, bounds[::2], bounds[1::2])]
            for p in self.points_in_geo(geo, [point]):
                r = None
                if self.ui.checkbox_rotation.isChecked():
                    r = np.random.random(3)*360
                new = self.vtk_widget.copy_geometry(shape, p, r)
                if self.ui.checkbox_union.isChecked() and last is not None:
                    last = self.vtk_widget.boolean_operation(
                        booltype='union', children=[last, new])
                else:
                    last = new
                n_points+=1
            max_itr +=1

    def gen_cubic(self, geo, shape, spacing, bounds):

        points = []
        for i in np.arange(bounds[0], bounds[1], spacing):
            for j in np.arange(bounds[2], bounds[3], spacing):
                for k in np.arange(bounds[4], bounds[5], spacing):
                    points.append([ i, j, k])

        self.distribute(geo, shape, points)

    def gen_bcc(self,  geo, shape, spacing, bounds):
        points = []
        for i in np.arange(bounds[0], bounds[1], spacing):
            for j in np.arange(bounds[2], bounds[3], spacing):
                for k in np.arange(bounds[4], bounds[5], spacing):
                    points.append([ i, j, k])

        half_spacing = spacing/2.0
        for i in np.arange(bounds[0]+half_spacing, bounds[1], spacing):
            for j in np.arange(bounds[2]+half_spacing, bounds[3], spacing):
                for k in np.arange(bounds[4]+half_spacing, bounds[5], spacing):
                    points.append([ i, j, k])

        self.distribute(geo, shape, points)

    def distribute(self, geo, shape, points):

        last = None
        for point in self.points_in_geo(geo, points):
            r=None
            if self.ui.checkbox_rotation.isChecked():
                r = np.random.random(3)*360
            new = self.vtk_widget.copy_geometry(shape, point, r)
            if self.ui.checkbox_union.isChecked() and last is not None:
                last = self.vtk_widget.boolean_operation(
                    booltype='union', children=[last, new])
            else:
                last = new


    def get_bounds(self, geo):
        return self.vtk_widget.get_input_data(geo).GetOutput().GetBounds()

    def points_in_geo(self, geo, points):

        vtkpoints = vtk.vtkPoints()

        for point in points:
            vtkpoints.InsertNextPoint(point)

        vtkpolydata = vtk.vtkPolyData()
        vtkpolydata.SetPoints(vtkpoints)

        enclosed_points = vtk.vtkSelectEnclosedPoints()
        enclosed_points.SetInputData(vtkpolydata)
        enclosed_points.SetSurfaceData(self.vtk_widget.get_input_data(geo).GetOutput())
        enclosed_points.Update()

        for i in range(len(points)):
            if enclosed_points.IsInside(i):
                yield points[i]
