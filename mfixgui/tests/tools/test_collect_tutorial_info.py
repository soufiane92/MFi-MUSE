import json
import os
import shutil
import tempfile

import pytest

import mfixgui.gui
import mfixgui.project_manager

from mfixgui.tools import get_mfix_templates, SCRIPT_DIRECTORY
from mfixgui.tools.collect_tutorial_info import make_info_dict


@pytest.mark.xfail(run=False,
                    reason=("Test passes when run by itself, but segfaults "
                            "when run with all other tests. Perhaps VTK issue?"))
def test_template_data(mocker):

    mocker.patch('mfixgui.gui.MfixGui.warning')
    tutorials_dir = os.path.join(get_mfix_templates(), "tutorials")
    tutorials = [
        os.path.join('FluidBed_2D', 'FB2D.mfx'),
        os.path.join('FluidBed_3D', 'FB3D.mfx'),
        os.path.join('FluidBed_DES', 'DES_FB1.mfx'),
        os.path.join('Hopper', 'Hopper_DEM_3D', 'HOPPER_DEM_3D.mfx'),
        os.path.join('Hopper', 'Hopper_DEM_Pseudo_2D', 'HOPPER_DEM_2D.mfx'),
        os.path.join('Hopper', 'Hopper_TFM_3D', 'HOPPER_TFM_3D.mfx'),
        os.path.join('Hopper', 'Hopper_TFM_Pseudo_2D', 'HOPPER_TFM_2D.mfx'),
        os.path.join('Silane_Pyrolysis', 'SP2D.mfx'),
        os.path.join('Vortex_Shedding', 'VORTEX_SHEDDING.mfx'),
    ]
    tmpdir = tempfile.mkdtemp()
    for tut in [os.path.join(tutorials_dir, tutorial) for tutorial in tutorials]:
        tmptut = os.path.join(tmpdir, os.path.basename(tut))
        print(tut, tmptut)
        shutil.copyfile(tut, tmptut)
        mfixgui.gui.sys.argv = ['-linfo', '-t', '-ct', tmptut]
        mfixgui.gui.main()

    actual_info_dict = make_info_dict()
    template_data_json = os.path.join(SCRIPT_DIRECTORY,
                                      "tools",
                                      "template_data.json")
    with open(template_data_json, encoding='utf-8', errors='replace') as f:
        expected_info_dict = json.load(f)

    assert expected_info_dict == actual_info_dict
