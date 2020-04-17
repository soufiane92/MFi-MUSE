import os

from PyQt5.QtCore import Qt
from PyQt5.QtWidgets import QListWidgetItem

from mfixgui.tools.qt import get_ui, get_thumbnail, view_mode

def make_open_widget(mfixgui):
    """ Create widget for Open filemenu pane """
    is_tile_mode = mfixgui.settings.value('open_list_mode', 'icon') == 'icon'

    open_widget = mfixgui.ui.file_menu_open_widget = get_ui('open.ui')
    open_widget.browse_button.clicked.connect(mfixgui.handle_open)
    open_widget.list_button.clicked.connect(lambda: _switch_to(mfixgui, False))
    open_widget.tile_button.clicked.connect(lambda: _switch_to(mfixgui, True))
    open_widget.template_list.itemDoubleClicked.connect(mfixgui.handle_file_menu_open_project)
    open_widget.template_list.setAttribute(Qt.WA_MacShowFocusRect, 0)
    open_widget.template_list.setFrameStyle(open_widget.template_list.NoFrame)
    open_widget.template_list.setViewMode(view_mode(is_tile_mode))
    open_widget.clear_recent.pressed.connect(mfixgui.handle_clear_recent)

    _switch_to(mfixgui, is_tile_mode)

    return open_widget


def switch_tab_open(stackedwidget, mfixgui):
    """ switch to Open tab """
    stackedwidget.setCurrentWidget(mfixgui.ui.file_menu_open_widget)
    _populate_recent_projects(mfixgui)


def _switch_to(mfixgui, is_tile_mode):
    mfixgui.settings.setValue('open_list_mode', 'icon' if is_tile_mode else 'list')
    mfixgui.ui.file_menu_open_widget.list_button.setChecked(not is_tile_mode)
    mfixgui.ui.file_menu_open_widget.template_list.setViewMode(view_mode(is_tile_mode))
    mfixgui.ui.file_menu_open_widget.tile_button.setChecked(is_tile_mode)


def file_menu_open_project(mfixgui, item):
    """Open the project of the selected item"""
    if not item:
        return

    if mfixgui.check_unsaved_abort():
        return

    project_path = item.full_path
    if os.path.exists(project_path):
        mfixgui.open_project(project_path)
    else:
        mfixgui.message(text="File does not exist: %s" % project_path)


def _populate_recent_projects(mfixgui):
    projects = mfixgui.settings.value('recent_projects', '').split('|')
    if not projects:
        return

    template_list = mfixgui.ui.file_menu_open_widget.template_list
    template_list.clear()
    for project in projects:
        if not os.path.exists(project):
            continue
        item = _project_list_item(project)
        template_list.addItem(item)

    template_list.verticalScrollBar().setValue(0)


def _project_list_item(project):
    name = os.path.basename(project)
    dir_ = os.path.dirname(project)
    icon = get_thumbnail(dir_)

    (name, _) = os.path.splitext(name)
    description = _project_description(project)
    text = '\n'.join([name, description, project])

    item = QListWidgetItem(icon, text)
    item.full_path = project
    return item


def _project_description(project):
    """ read description from project file """
    description = ''
    with open(project, encoding='utf-8', errors='replace') as project_file:
        for line in project_file:
            clean = line.lower().split('#')[0].split('!')[0].strip()
            if 'description' in clean:
                toks = [tok.strip() for tok in line.split('=')]
                toks_lower = [tok.lower() for tok in toks]
                des_ind = toks_lower.index('description')
                description = toks[des_ind + 1].replace('"', '').replace("'", '')
                break
    return description

def clear_recent(mfixgui):
    mfixgui.settings.setValue('recent_projects', '|'.join([]))
    mfixgui.ui.file_menu_open_widget.template_list.clear()
