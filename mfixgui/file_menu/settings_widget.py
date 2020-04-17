from PyQt5.QtWidgets import QStyleFactory, QTabBar

from mfixgui.tools.qt import get_ui
from mfixgui.stylesheets import DEFAULT_STYLESHEET, OSX_STYLESHEET


def make_settings_widget(mfixgui):
    """ Create widget for Settings filemenu pane """
    widget = mfixgui.ui.file_menu_settings_widget = get_ui('settings.ui')
    settings = mfixgui.settings

    widget.developer_mode.setChecked(int(settings.value('developer_mode', 0)))
    widget.developer_mode.stateChanged.connect(mfixgui.handle_enable_developer_mode)

    widget.collapse_splitters.setChecked(int(settings.value('collapse_qsplitter', 0)))
    widget.collapse_splitters.stateChanged.connect(mfixgui.collapse_splitters)

    widget.show_resources.setChecked(int(settings.value('show_resources', 0)))
    widget.show_resources.stateChanged.connect(mfixgui.resource_monitor.show)

    widget.animation_speed.setValue(int(settings.value('animation_speed', 400)))
    widget.animation_speed.valueChanged.connect(
        lambda n: settings.setValue('animation_speed', n))

    widget.appstyle_combobox.addItems([s.lower() for s in QStyleFactory.keys()])
    widget.appstyle_combobox.currentIndexChanged.connect(
        lambda: _change_app_style(widget.appstyle_combobox.currentText(), mfixgui))
    widget.appstyle_combobox.setCurrentText(settings.value('app_style'))

    return widget


def switch_tab_settings(stackedwidget, mfixgui):
    """ switch to Settings tab """
    stackedwidget.setCurrentWidget(mfixgui.ui.file_menu_settings_widget)


def _change_app_style(style, mfixgui):
    stylesheet = DEFAULT_STYLESHEET
    if 'macintosh' in style.lower():
        stylesheet += OSX_STYLESHEET
    mfixgui.app.setStyleSheet(stylesheet)
    mfixgui.app.setStyle(QStyleFactory.create(style))
    mfixgui.settings.setValue('app_style', style)


def enable_developer_mode(mfixgui, enable):
    """ Enable or Disabled developer mode """
    mfixgui.change_mode('modeler')
    mfixgui.ui.pushButtonDeveloper.setVisible(enable)
    mfixgui.ui.pushButtonInterpreter.setVisible(enable)

    tab = mfixgui.ui.tabWidgetGraphics
    response = mfixgui.ui.mfix_response
    if enable:
        tab.insertTab(1, response, 'MFiX status')
        for side in (QTabBar.RightSide, QTabBar.LeftSide):
            button = tab.tabBar().tabButton(1, side)
            if button:
                button.resize(0, 0)
    else:
        tab.removeTab(tab.indexOf(response))

    mfixgui.settings.setValue('developer_mode', int(enable))
