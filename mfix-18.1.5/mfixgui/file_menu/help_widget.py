import os

from PyQt5.QtCore import (QUrl, Qt)
from PyQt5.QtGui import (QDesktopServices, QIcon)
from PyQt5.QtWidgets import (QLabel, QListWidgetItem)

from mfixgui.tools import (MFIX_DOCUMENTATION, SCRIPT_DIRECTORY, get_mfix_doc_html,
                           path2url)
from mfixgui.tools.qt import get_ui

from mfixgui.version_info import MFIX_VERSION

try:
    from nodeworks.tools import get_nodeworks_doc
    from nodeworks import __version__ as NODEWORKS_VERSION
    NODEWORKS_DOC_PATH = get_nodeworks_doc()
except ImportError:
    NODEWORKS_VERSION = NODEWORKS_DOC_PATH = None

def make_help_widget(mfixgui):
    """ Create widget for Help filemenu pane """
    help_widget = mfixgui.ui.file_menu_help_widget = get_ui('help.ui')
    help_widget.mfix_local_label.setText(f'<a href="file://{MFIX_DOCUMENTATION}">'
                                         f'Local MFiX Documentation (v {MFIX_VERSION})</a>')
    help_widget.nodeworks_local_label.setText(f'<a href="file://{NODEWORKS_DOC_PATH}">'
                                              f'Local Nodeworks Documentation (v {NODEWORKS_VERSION})</a>'
                                              if NODEWORKS_DOC_PATH is not None else '')
    help_widget.mfix_local_label.linkActivated.connect(lambda url: QDesktopServices.openUrl(QUrl(url)))
    help_widget.nodeworks_local_label.linkActivated.connect(lambda url: QDesktopServices.openUrl(QUrl(url)))

    tut_lw = help_widget.tutorial_listwidget
    tut_lw.setAttribute(Qt.WA_MacShowFocusRect, 0)
    tut_lw.setFrameStyle(tut_lw.NoFrame)

    for thumb, text in TUTORIALS:
        label = QLabel(text)
        label.linkActivated.connect(lambda url: QDesktopServices.openUrl(QUrl(url)))
        item = QListWidgetItem()
        item.setIcon(QIcon(thumb))
        item.setFlags(Qt.ItemIsEnabled)
        tut_lw.addItem(item)
        tut_lw.setItemWidget(item, label)

    return help_widget


def switch_tab_help(stackedwidget, mfixgui):
    """ switch to Help tab """
    stackedwidget.setCurrentWidget(mfixgui.ui.file_menu_help_widget)


def _make_tutorial(tut):
    image, title, tutorial, youtube, _ = tut
    thumb = os.path.join(SCRIPT_DIRECTORY, 'images', image)
    if not os.path.exists(thumb):
        thumb = ''

    tutorial_path = (get_mfix_doc_html(), 'user_manual', 'tutorials', tutorial)
    tutorial_url = path2url(os.path.join(*tutorial_path))
    if youtube is not None:
        text = '<b>{}</b><p><a href="{}">Text</a> | <a href="{}">Video</a></p>'.format(
            title, tutorial_url, youtube)
    else:
        text = '<b>{}</b><p><a href="{}">Text</a> | Video</p>'.format(
            title, tutorial_url)
    return (thumb, text)


# URLs and thumbnails for Tutorials in sphinx documentation
TUTORIALS = map(_make_tutorial, [
    (
        'gui_tfm_2d_thumbnail.png',
        'Two-dimensional fluid bed two-fluid model (TFM)',
        'tutorial_tfm.html',
        'https://youtu.be/rZgdGH2pkx4',
        ['2d', 'tfm']
    ),
    (
        'gui_dem_2d_thumbnail.png',
        'Two-dimensional fluid bed discrete-element model (DEM)',
        'tutorial_dem.html',
        None,
        ['2d', 'dem']
    ),
    (
        'gui_sphere_thumbnail.png',
        '3D Single-phase flow over a sphere',
        'tutorial_sphere.html',
        None,
        ['3d', 'single', 'cartesian']
    ),
    (
        'gui_3d_fluidbed_thumbnail.png',
        '3D Fluid Bed (TFM & DEM)',
        'tutorial_3d_fluidbed.html',
        None,
        ['3d', 'tfm', 'cartesian']
    ),
    (
        'gui_hopper_thumbnail.png',
        '3D Hopper discrete-element model (DEM)',
        'tutorial_hopper.html',
        'https://youtu.be/kEiPs-phndg',
        ['3d', 'dem', 'cartesian']
    ),
    (
        'gui_dem_chutes_thumbnail.png',
        'DEM Granular Flow Chutes',
        'tutorial_dem_chutes.html',
        None,
        ['3d', 'dem', 'cartesian']
    ),
    ])
