# -*- coding: utf-8 -*-
'''
This script will read the *.rgb files and produce pngs from them to be
displayed in the gui.
'''
import glob
import os
from collections import OrderedDict
import numpy as np

from qtpy import QtGui, QtCore

try:
    import vtk
except ImportError:
    vtk = None

SCRIPT_DIRECTORY = os.path.abspath(os.path.dirname(__file__))
COLOR_MAP_EXT = '.rgb'
COLOR_MAPS = glob.glob(os.path.join(SCRIPT_DIRECTORY, '*' + COLOR_MAP_EXT))

def read_rgb(path):
    """
    Given a file path, extract the rgb values.
    """
    rgb_list = []
    with open(path, encoding='ascii') as f: #Colormaps are ASCII files
        for line in f:
            rgb = line.split()
            if len(rgb) == 3:
                try:
                    rgb_list.append([float(v) for v in rgb])
                except:
                    pass
    return rgb_list

def get_color_map_dict():
    '''return a dictionary of color maps'''
    color_dict = OrderedDict()
    for f in COLOR_MAPS:
        name = os.path.basename(f).replace(COLOR_MAP_EXT, '').lower()
        rgb = read_rgb(f)
        color_dict[name] = rgb
        color_dict[name + '_reversed'] = list(reversed(rgb))
    return color_dict

def get_color_map_pngs():
    '''return a dictionary of name:path_to_png'''
    pngs = OrderedDict()
    for f in glob.glob(os.path.join(SCRIPT_DIRECTORY,'*.png')):
        name = os.path.basename(f).replace('.png', '').lower()
        pngs[name] = f
    return pngs

icon_cache = OrderedDict()
def build_qicons():
    if icon_cache:
        return icon_cache

    icons = OrderedDict()
    for name, png in get_color_map_pngs().items():
        pix = QtGui.QPixmap(png)
        icons[name] = {
            'bar': QtGui.QIcon(pix.scaled(100, 25, transformMode=QtCore.Qt.SmoothTransformation)),
            'icon': QtGui.QIcon(pix.scaled(25, 25, transformMode=QtCore.Qt.SmoothTransformation)),
            }
    icon_cache.update(icons)
    return icons

def build_vtk_lookup_tables():
    '''build and return lookup tables for vtk'''
    LUTs = OrderedDict()
    for name, colors in get_color_map_dict().items():
        lookup_table = vtk.vtkLookupTable()
        lookup_table.SetNumberOfTableValues(len(colors))
        for i, c in enumerate(colors):
            lookup_table.SetTableValue(i, c[0], c[1], c[2], 1.0)

        LUTs[name] = lookup_table
    return LUTs

def main():
    '''Use matplotlib to generate previews of the color maps and save them as
    png files so that they can be displayed in the gui'''

    # these imports belong here! If they are moved to the top of the file, then
    # matplotlib becomes a unneeded dependency
    from matplotlib import pyplot as plt
    from matplotlib.colors import LinearSegmentedColormap

    def plot_color_gradients(name, cmap, gradient):
        fig, axes = plt.subplots(1, figsize=(20, 1))
        fig.subplots_adjust(top=1, bottom=0, left=0, right=1)
        axes.imshow(gradient, aspect='auto', cmap=cmap)
        # Turn off *all* ticks & spines, not just the ones with colormaps.
        axes.set_axis_off()
        fig.savefig(name+'.png', dpi=1)

    gradient = np.linspace(0, 1, 256)
    gradient = np.vstack((gradient, gradient))

    for name, colors in get_color_map_dict().items():
        # normal
        cmap = LinearSegmentedColormap.from_list(name, colors)
        plot_color_gradients(name, cmap, gradient)

        # revered
        cmap = LinearSegmentedColormap.from_list(name, list(reversed(colors)))
        plot_color_gradients(name+'_reversed', cmap, gradient)

if __name__ == "__main__":
    main()
