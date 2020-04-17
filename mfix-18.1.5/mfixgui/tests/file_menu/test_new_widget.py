from collections import OrderedDict
from os.path import join

from PyQt5.QtWidgets import QSizePolicy

from mfixgui.file_menu.new_widget import make_new_widget, get_thumbnail, collect_templates

import mfixgui.file_menu.new_widget


def test_make_new_widget(mocker, qtbot):
    app = mocker.Mock()
    mocker.patch('mfixgui.gui.MfixGui.add_extra_keyword_doc')
    gui = mfixgui.gui.MfixGui(app)
    mocker.patch.object(gui, 'settings')
    label_policy = QSizePolicy(QSizePolicy.Minimum, QSizePolicy.Maximum)

    gui.settings.value.return_value = 'icon'
    make_new_widget(gui)

    gui.settings.value.return_value = 'list'
    make_new_widget(gui)

    gui.settings.value.return_value = None
    make_new_widget(gui)


def test_collect_templates():

    templates = collect_templates()
    expected = (
        ('tutorials', [
            'blank',
            'FluidBed_2D',
            'FluidBed_3D',
            'FluidBed_DES',
            join('Hopper', 'Hopper_DEM_3D'),
            join('Hopper', 'Hopper_DEM_Pseudo_2D'),
            join('Hopper', 'Hopper_TFM_3D'),
            join('Hopper', 'Hopper_TFM_Pseudo_2D'),
            'Loopseal_DEM_Psuedo_2d',
            'Silane_Pyrolysis',
            'Spouted/Spouted_DEM_Pseudo_2d',
            'Vortex_Shedding']
        ),
        ('benchmarks',
         [
             join('dem', '3d-box'),
             join('dem', 'mini-cfb'),
             join('tfm', 'ParallelBenchmarkCases', 'A_CFB_06'),
             join('tfm', 'ParallelBenchmarkCases', 'B_O3_06'),
             join('tfm', 'ParallelBenchmarkCases', 'C_COM_06'),
         ]),
        ('tests', (
            [join('dem', 'DEM0{}'.format(i)) for i in range(1, 7)] +
            [join('fluid', 'FLD0{}'.format(i)) for i in range(1, 9)] +
            [join('mms', 'MMS0{}'.format(i)) for i in range(1, 6)]
        )
        ))

    assert templates == OrderedDict(expected)

def test_get_icon(qtbot):
    get_thumbnail('foo')
    get_thumbnail('123')
