"""Utility functions which depend on Qt"""

import copy
import os
import random
from collections import OrderedDict

from qtpy import uic
from qtpy.QtCore import Qt, QSize
from qtpy.QtGui import QColor, QIcon, QPixmap, QImage
from qtpy.QtWidgets import QFrame, QListWidget

from mfixgui.tools import SCRIPT_DIRECTORY


def set_item_noedit(item):
    item.setFlags(item.flags() & ~Qt.ItemIsEditable)


def set_item_enabled(item, enabled):
    """Enable/disable items which do not have a setEnabled method, like menu
    items"""
    flags = item.flags()
    if enabled:
        flags |= Qt.ItemIsEnabled
    else:
        flags &= ~Qt.ItemIsEnabled
    item.setFlags(flags)


def item_enabled(item):
    flags = item.flags()
    return bool(flags & Qt.ItemIsEnabled)


def get_combobox_item(combobox, n):
    """Return the n'th menu item from a combobox"""
    model = combobox.model()
    return model.item(n, 0)

def set_combobox_tooltip(combobox, index=None, default=''):
    if index is None:
        index = combobox.currentIndex()
    item = get_combobox_item(combobox, index)
    combobox.setToolTip(item.toolTip() if item else default)

def get_selected_row(table):
    """get index of selected row from a QTableWidget"""
    # note, currentRow can return  >0 even when there is no selection
    rows = set(i.row() for i in table.selectedIndexes())
    return None if not rows else rows.pop()


def get_selected_rows(table):
    """get index of selected row from a QTableWidget"""
    # note, currentRow can return  >0 even when there is no selection
    rows = set(i.row() for i in table.selectedIndexes())
    return sorted(list(rows))


def get_image_path(name):
    """"get path to images"""
    path = os.path.join(SCRIPT_DIRECTORY, 'images', name)
    if os.name == 'nt':
        path = path.replace('\\', '//')
    return path


pixmap_cache = {}
def get_pixmap(name, width, height):
    pixmap = pixmap_cache.get((name, width, height))
    if pixmap is None:
        pixmap = QPixmap(get_image_path(name)).scaled(
            width, height, Qt.KeepAspectRatio, Qt.SmoothTransformation)
        pixmap_cache[(name, width, height)] = pixmap
    return pixmap


icon_cache = {}
def get_icon(name, default=None, resample=True):
    """Return image inside a QIcon object
    default: default image name or icon
    resample: if True, manually resample icon pixmaps for usual sizes
    (16, 24, 32, 48, 96, 128, 256). This is recommended for QMainWindow icons
    created from SVG images on non-Windows platforms due to a Qt bug (see
    http://code.google.com/p/spyderlib/issues/detail?id=1314)."""

    icon = icon_cache.get((name, default, resample))
    if icon:
        return icon

    if default is None:
        icon = QIcon(get_image_path(name))
    elif isinstance(default, QIcon):
        icon_path = get_image_path(name)
        icon = default if icon_path is None else QIcon(icon_path)
    else:
        icon = QIcon(get_image_path(name))
    if resample:
        icon0 = QIcon()
        for size in (16, 24, 32, 48, 96, 128, 256, 512):
            icon0.addPixmap(icon.pixmap(size, size))
        icon_cache[(name, default, resample)] = icon0
        ret = icon0
    else:
        ret = icon
    icon_cache[(name, default, resample)] = ret
    return ret


def widget_iter(widget):
    """iterator to recursively iterate over widgets"""
    for child in widget.children():
        if child.children():
            for child2 in widget_iter(child):
                yield child2
        yield child


def random_pastel_color():
    """generate and return a random pastel color"""
    return [(random.randint(0, 128) + 100) / 255.0 for i in range(3)]


class CellColor(object):
    """
    A class to store color information and return '' if str or print is called
    on it. This is used to store colors in cells of a table.
    """

    def __init__(self, color=(1, 0, 0), text=''):

        if isinstance(color, (list, tuple)):
            self.color = QColor(*color)
        else:
            self.color = QColor(color)
        self.text = text

    @property
    def color_int(self):
        return [255 * c for c in self.color_float]

    @property
    def color_float(self):
        return self.color.getRgbF()[:3]

    @property
    def color_hex(self):
        return self.color.name()

    @property
    def qcolor(self):
        return self.color

    def __repr__(self):
        return self.text

    def __deepcopy__(self, memo):
        return CellColor(
            copy.deepcopy(self.color_hex), copy.deepcopy(self.text))

    def rand(self):
        self.color.setRgbF(*random_pastel_color())


def deepcopy_dict(dirty_dict, qobjects=False):
    '''deep copy a dictionary that has Qt objects in it
    Note: python 3.6+ can't copy Qt objects like QPixmap
    setting qobjects=True will copy the qt objects'''

    clean_dict = OrderedDict() if isinstance(dirty_dict, OrderedDict) else {}

    for key, value in dirty_dict.items():
        if isinstance(value, (dict, OrderedDict)):
            clean_dict[key] = deepcopy_dict(value, qobjects)
        elif isinstance(value, QPixmap):
            if qobjects:
                clean_dict[key] = QPixmap(value)
        elif isinstance(value, QColor):
            if qobjects:
                clean_dict[key] = QColor(value)
        else:
            clean_dict[key] = copy.deepcopy(value)
    return clean_dict


def clear_layout(layout):
    """given a layout, clear all widgets"""
    while layout.count():
        item = layout.takeAt(0)
        widget = item.widget()
        if widget:
            widget.deleteLater()


def get_separator(vertical=True, parent=None):
    """create a QFrame that looks like a separator"""
    f = QFrame
    line = f(parent)
    if vertical:
        line.setFrameShape(f.VLine)
    else:
        line.setFrameShape(f.HLine)
    line.setFrameShadow(f.Sunken)
    return line


_main_icon_size = QSize(24, 24)
_sub_icon_size = QSize(16, 16)


def set_main_icon_size(x):
    global _main_icon_size
    _main_icon_size = QSize(x, x)


def main_icon_size():
    return _main_icon_size


def set_sub_icon_size(x):
    global _sub_icon_size
    _sub_icon_size = QSize(x, x)


def sub_icon_size():
    return _sub_icon_size


def main_icon_height():
    return main_icon_size().height()


def sub_icon_height():
    return sub_icon_size().height()

def get_ui(ui_file, widget=None):
    """ returns the qt widget loaded from .ui file """
    ui_path = os.path.join(SCRIPT_DIRECTORY, 'uifiles', ui_file)
    if widget is None:
        return uic.loadUi(ui_path)
    return uic.loadUi(ui_path, widget)

def get_thumbnail(project_dir_path):
    thumb_path = os.path.join(project_dir_path, '.thumbnail')
    if os.path.isfile(thumb_path):
        pix = QPixmap()
        pix.convertFromImage(QImage(thumb_path, 'PNG'))
        icon = QIcon(pix)
    else:
        icon = get_icon('missing_thumbnail.png')
    return icon

def view_mode(is_tile_mode):
    return QListWidget.IconMode if is_tile_mode else QListWidget.ListMode
