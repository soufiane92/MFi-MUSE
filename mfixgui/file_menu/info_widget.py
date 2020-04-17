import json

from mfixgui.tools.qt import get_ui

def make_info_widget(mfixgui):
    """ Create widget for Info filemenu pane """
    widget = mfixgui.ui.file_menu_info_widget = get_ui('info.ui')
    return widget


def switch_tab_info(stackedwidget, mfixgui):
    stackedwidget.setCurrentWidget(mfixgui.ui.file_menu_info_widget)


def update_info(info_widget, project):
    """ Update info fields """
    comments = project.mfix_gui_comments
    info_widget.created_by.setText(comments.get('author', 'Unknown'))
    info_widget.created_time.setText(comments.get('created_date', 'Unknown'))
    info_widget.gui_version.setText(str(comments.get('gui_version', 'Unknown')))
    info_widget.modified_by.setText(comments.get('modified_by', '').replace('|', ', '))
    info_widget.modified_time.setText(project.modified_time)
    info_widget.project_notes.setText(json.loads(comments.get('project_notes', '""')))
    info_widget.project_version.setText(str(comments.get('project_version', 'Unknown')))
