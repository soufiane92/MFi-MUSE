import os

from qtpy.QtCore import Qt, QUrl
from qtpy.QtGui import QTextCursor, QDesktopServices

from mfixgui.tools.qt import get_ui, get_pixmap

from mfixgui.version_info import get_version_info


def make_about_widget(mfixgui, label_policy):
    """ Create widget for About filemenu pane """
    about_widget = mfixgui.ui.file_menu_about_widget = get_ui('about.ui')
    about_widget.about_label.setSizePolicy(label_policy)
    about_widget.copy_btn.clicked.connect(lambda: _copy_version_info(mfixgui))
    about_widget.mailinglist_lbl.linkActivated.connect(lambda url: QDesktopServices.openUrl(QUrl(url)))

    about_widget.version_info_txtbrowser.setText(os.linesep.join(get_version_info()))

    splash = about_widget.label_splash
    pixmap = get_pixmap('splash.png', 600, 338)
    splash.setPixmap(pixmap)
    splash.setAlignment(Qt.AlignCenter)

    return about_widget

def switch_tab_about(stackedwidget, mfixgui):
    """ switch to About tab """
    stackedwidget.setCurrentWidget(mfixgui.ui.file_menu_about_widget)

def _copy_version_info(mfixgui):
    ''' copy contents of TextBrowser to clipboard '''

    txtbox = mfixgui.ui.file_menu_about_widget.version_info_txtbrowser
    txt = txtbox.toPlainText()
    mfixgui.app.clipboard().setText(txt)

    # select the text (to give visual feedback that it was copied)
    cursor = txtbox.textCursor()
    cursor.setPosition(0)
    cursor.setPosition(len(txt), QTextCursor.KeepAnchor)
    txtbox.setTextCursor(cursor)
