import pytest

from PyQt5.QtWidgets import QMainWindow

import mfixgui.graphic_tabs

@pytest.fixture
def mfix_gui(qtbot, mocker):
    """ define MfixGui fixture """
    qmain = QMainWindow()
    mfixgui_fixture = mfixgui.gui.MfixGui(qmain)
    mocker.patch.object(mfixgui_fixture, 'confirm_close', return_value=True)
    qtbot.addWidget(mfixgui_fixture)
    return mfixgui_fixture


def test_create_plot_widget(mfix_gui):
    """ test adding each type of plot tab """
    new_tab = mfix_gui.add_tab()
    new_tab.create_plot_widget("DT")
    assert len(mfix_gui.plot_dict) == 1
    assert mfix_gui.graphics_to_str() == '{"order": ["DT"], "data": {"DT": {"plot": true, "vtk": false, "monitors": false}}}'

    new_tab = mfix_gui.add_tab()
    new_tab.create_plot_widget("Elapsed Time")
    assert len(mfix_gui.plot_dict) == 2
    assert mfix_gui.graphics_to_str() == '{"order": ["DT", "Elapsed Time"], "data": {"DT": {"plot": true, "vtk": false, "monitors": false}, "Elapsed Time": {"plot": true, "vtk": false, "monitors": false}}}'

    new_tab = mfix_gui.add_tab()
    new_tab.create_plot_widget("NIT")
    assert len(mfix_gui.plot_dict) == 3
    assert mfix_gui.graphics_to_str() == '{"order": ["DT", "Elapsed Time", "NIT"], "data": {"DT": {"plot": true, "vtk": false, "monitors": false}, "Elapsed Time": {"plot": true, "vtk": false, "monitors": false}, "NIT": {"plot": true, "vtk": false, "monitors": false}}}'

    new_tab = mfix_gui.add_tab()
    new_tab.create_plot_widget("Residuals")
    assert len(mfix_gui.plot_dict) == 4
    assert mfix_gui.graphics_to_str() == '{"order": ["DT", "Elapsed Time", "NIT", "Residuals"], "data": {"DT": {"plot": true, "vtk": false, "monitors": false}, "Elapsed Time": {"plot": true, "vtk": false, "monitors": false}, "NIT": {"plot": true, "vtk": false, "monitors": false}, "Residuals": {"plot": true, "vtk": false, "monitors": false}}}'

    for index in range(5, 1, -1):
        mfix_gui.remove_tab(index)

    assert not mfix_gui.plot_dict
    assert mfix_gui.graphics_to_str() == '{"order": [], "data": {}}'

def test_load_plot_widget(mfix_gui):
    """ test loading plots from saved JSON """
    json_str = '{"order": ["toolButton_dt"], "data": {"toolButton_dt": {"plot": true, "vtk": false, "monitors": false}}}'
    mfix_gui.graphics_from_str(json_str)
