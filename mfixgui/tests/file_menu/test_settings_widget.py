from PyQt5.QtWidgets import QMainWindow, QStyleFactory

from mfixgui.file_menu.settings_widget import make_settings_widget

def test_fusion_app_style(mocker, qtbot):
    qmain = mocker.Mock()
    test_settings = {'developer_mode':1,
                     'collapse_qsplitter':1,
                     'show_resources':1,
                     'animation_speed':123,
                     'app_style':'fusion',
    }
    qmain.settings.value = test_settings.get

    widget = make_settings_widget(qmain)
    qtbot.addWidget(widget)
    widget.show()

    assert widget.developer_mode.isChecked()
    assert widget.show_resources.isChecked()
    assert widget.collapse_splitters.isChecked()
    assert widget.animation_speed.value() == 123
    assert widget.appstyle_combobox.currentText() == 'fusion'
    widget.appstyle_combobox.setCurrentIndex(0)
    qmain.app.setStyle.assert_called()

def test_windows_app_style(mocker, qtbot):
    qmain = mocker.Mock()
    test_settings = {'developer_mode':0,
                     'collapse_qsplitter':0,
                     'show_resources':0,
                     'animation_speed':321,
                     'app_style':'windows',
    }
    qmain.settings.value = test_settings.get

    widget = make_settings_widget(qmain)
    qtbot.addWidget(widget)
    widget.show()

    assert not widget.developer_mode.isChecked()
    assert not widget.show_resources.isChecked()
    assert not widget.collapse_splitters.isChecked()
    assert widget.animation_speed.value() == 321
    assert widget.appstyle_combobox.currentText() == 'windows'
    widget.appstyle_combobox.setCurrentIndex(1)
    qmain.app.setStyle.assert_called()
