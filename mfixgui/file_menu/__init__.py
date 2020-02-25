"""the file menu bar"""

from PyQt5.QtCore import QPoint, QPropertyAnimation, QSize, Qt
from PyQt5.QtWidgets import (QGridLayout, QLabel, QListWidget, QListWidgetItem,
                             QSizePolicy, QStackedWidget, QToolButton, QWidget,
                             QSplitter)

from mfixgui.file_menu.about_widget import make_about_widget, switch_tab_about
from mfixgui.file_menu.help_widget import make_help_widget, switch_tab_help
from mfixgui.file_menu.info_widget import make_info_widget, switch_tab_info, update_info
from mfixgui.file_menu.new_widget import (make_new_widget, switch_tab_new,
                                          file_menu_new_project, filter_new)
from mfixgui.file_menu.open_widget import (make_open_widget, switch_tab_open, clear_recent,
                                           file_menu_open_project)
from mfixgui.file_menu.settings_widget import (make_settings_widget,
                                               enable_developer_mode, switch_tab_settings)
from mfixgui.tools.qt import get_icon, get_pixmap, get_separator, widget_iter
from mfixgui.widgets.nodeworks import NODEWORKS_AVAILABLE


NODEWORKS_ITEMS = [
    ('Import Workflow', 'import'),
    ('Export Workflow', 'open_in_new'),
    ('sep', ''),
] if NODEWORKS_AVAILABLE else []


MENU_ITEMS = [
    ('Project Info', 'infooutline'),
    ('New', 'newfolder'),
    ('Open', 'openfolder'),
    ('Save', 'save'),
    ('Save As', 'save'),
    ('Export Project', 'open_in_new'),
    ('sep', ''),] + NODEWORKS_ITEMS + [
        ('Settings', 'settings'),
        ('Help', 'help'),
        ('About', 'infooutline'),
        ('Quit', 'close'),
    ]


class FileMenu(object):
    """File menu mixin for the gui."""

    file_menu_animation_speed = 150

    # size policies
    label_policy_minmax = QSizePolicy(QSizePolicy.Minimum, QSizePolicy.Maximum)
    label_policy_maxmin = QSizePolicy(QSizePolicy.Maximum, QSizePolicy.Maximum)

    def init_file_menu(self):
        """Build the file menu."""

        ui = self.ui

        self.file_menu = QWidget(self)
        self.file_menu.setObjectName('file_menu')
        self.file_menu.setStyleSheet(
            'QWidget#file_menu{background-color: #E0E0E0;}')
        self.file_menu.hide()

        layout = ui.file_menu_layout = QGridLayout(self.file_menu)
        layout.setContentsMargins(0, 0, 0, 0)

        # return
        ui.file_menu_return = self._make_back_button()
        layout.addWidget(ui.file_menu_return, 0, 0)

        # list widget
        lw = ui.file_menu_list = QListWidget()
        lw.setMaximumWidth(200)
        lw.setFrameStyle(lw.NoFrame)
        lw.setSizePolicy(QSizePolicy(QSizePolicy.Maximum, QSizePolicy.Minimum))
        lw.setAttribute(Qt.WA_MacShowFocusRect, 0)
        lw.setStyleSheet('''QListView{background-color: #E0E0E0;}
                            QListView::item:selected{background: #64B5F6;
                                color: white;}
                            QListView::item:hover{background:#BBDEFB;}
                         ''')
        lw.selectionModel().selectionChanged.connect(
            self.handle_file_menu_selection_changed)

        for name, icon in MENU_ITEMS:
            if name == 'sep':
                li = QListWidgetItem()
                li.setFlags(Qt.NoItemFlags)
                li.setSizeHint(QSize(0, 10))
                lw.addItem(li)
                sep = get_separator(vertical=False)
                sep.setEnabled(False)
                lw.setItemWidget(li, sep)
            else:
                li = QListWidgetItem(get_icon(icon + '.svg'), name)
                lw.addItem(li)
        layout.addWidget(lw, 1, 0)

        layout.addWidget(_make_logo_label(), 2, 0)

        ui.file_menu_stackedwidget = QStackedWidget()
        layout.addWidget(ui.file_menu_stackedwidget, 0, 1, -1, 1)
        self._add_stacked_widgets(ui.file_menu_stackedwidget, ui)

    def _make_back_button(self):
        btn = QToolButton(self.file_menu)
        btn.setIcon(get_icon('left.svg'))
        btn.setText('Back')
        btn.setToolButtonStyle(Qt.ToolButtonTextBesideIcon)
        btn.setAutoRaise(True)
        btn.clicked.connect(self.handle_file_menu_hide)
        return btn

    def _add_stacked_widgets(self, st, ui):
        ui.file_menu_blank_widget = _make_blank_widget()
        st.addWidget(ui.file_menu_blank_widget)
        st.addWidget(make_new_widget(self))
        st.addWidget(make_open_widget(self))
        st.addWidget(make_info_widget(self))
        st.addWidget(make_settings_widget(self))
        st.addWidget(make_help_widget(self))
        st.addWidget(make_about_widget(self, self.label_policy_minmax))

    def handle_clear_recent(self):
        clear_recent(self)

    def handle_file_menu_selection_changed(self, selected, _):
        """ unused second argument 'deselected' """
        if selected and selected.indexes():
            selected_row = selected.indexes()[0].row()
            selection = self.ui.file_menu_list.item(selected_row)
            text = str(selection.text()).lower()
            self._change_file_menu_selection(text)

    def _change_file_menu_selection(self, text):
        sw = self.ui.file_menu_stackedwidget
        if text == 'project info':
            switch_tab_info(sw, self)
        elif text == 'new':
            switch_tab_new(sw, self)
        elif text == 'open':
            switch_tab_open(sw, self)
        elif text == 'save':
            self.handle_save()
        elif text == 'save as':
            self.handle_save_as()
        elif text == 'export project':
            self.handle_export()
        elif text == 'export workflow':
            self.ui.nodeworks_widget.handle_export()
        elif text == 'import workflow':
            self.ui.nodeworks_widget.handle_import()
        elif text == 'settings':
            switch_tab_settings(sw, self)
        elif text == 'help':
            switch_tab_help(sw, self)
        elif text == 'about':
            switch_tab_about(sw, self)
        elif text == 'quit':
            self.close()

    def handle_open_shortcut(self):
        """handle Ctrl+O shortcut"""
        self.handle_file_menu()
        switch_tab_open(self.ui.file_menu_stackedwidget, self)

    def handle_new_shortcut(self):
        """handle Ctrl+N shortcut"""
        self.handle_file_menu()
        switch_tab_new(self.ui.file_menu_stackedwidget, self)

    def handle_file_menu(self):
        """Show the file menu"""

        ui = self.ui
        project_file = self.get_project_file()
        if project_file is None:
            if self.settings.value('recent_projects', ''):
                ui.file_menu_list.setCurrentRow(2)
            else:
                ui.file_menu_list.setCurrentRow(1)
            items = [
                'project info',
                'save',
                'save as',
                'export project',
                'export workflow',
                'import workflow',
            ]
            self.disable_file_menu_items(items)
        else:
            update_info(ui.file_menu_info_widget, self.project)
            self.disable_file_menu_items([], ['save'])
            ui.file_menu_list.setCurrentRow(0)

        tw = ui.toolbutton_file.width()
        th = ui.toolbutton_file.height()
        ui.file_menu_return.setMinimumWidth(tw)
        ui.file_menu_return.setMinimumHeight(th)

        # hide the current widget, issue #291, VTK widget overlay issues
        ui.tabWidgetGraphics.currentWidget().hide()

        # animate
        if not self.file_menu.isVisible():
            w, h = self.width(), self.height()
            self.file_menu.setGeometry(-w / 2, 0, w, h)
            self.file_menu.show()
            self.file_menu.raise_()
            ani = self._create_animation(self.file_menu, (-w / 4, 0), (0, 0))
            self.file_menu_animation = ani
            ani.finished.connect(self.file_menu_animation_finished)
            ani.start()

    def _create_animation(self, target, start, end):
        (x_start, y_start) = start
        (x_end, y_end) = end
        animation = QPropertyAnimation(target, "pos".encode('utf-8'))
        animation.setDuration(self.file_menu_animation_speed)
        animation.setStartValue(QPoint(x_start, y_start))
        animation.setEndValue(QPoint(x_end, y_end))
        return animation

    def handle_file_menu_hide(self):
        """Show the file menu"""
        # animate
        width = self.width()
        ani = self._create_animation(self.file_menu, (0, 0), (-width / 4, 0))
        self.file_menu_animation = ani
        ani.finished.connect(self.file_menu_animation_finished_hide)
        ani.start()

    def file_menu_animation_finished(self):
        """callback when the show animation is finished"""
        w, h = self.width(), self.height()
        self.file_menu.setGeometry(0, 0, w, h)
        self.file_menu.show()
        self.file_menu.raise_()

    def file_menu_animation_finished_hide(self):
        """callback when the hide animation is finished"""
        # show the current widget, issue #291, VTK widget overlay issues
        self.ui.tabWidgetGraphics.currentWidget().show()
        self.file_menu.hide()

    def disable_file_menu_items(self, items, except_items=()):
        for r in range(self.ui.file_menu_list.count()):
            i = self.ui.file_menu_list.item(r)
            text = str(i.text()).lower()
            if text in except_items:
                continue
            if text in items:
                new_flags = i.flags() & ~Qt.ItemIsEnabled
            else:
                new_flags = i.flags() | Qt.ItemIsEnabled

            i.setFlags(new_flags)

    def handle_enable_developer_mode(self, enable):
        """ handler for 'developer mode' checkbox """
        enable_developer_mode(self, enable)

    def collapse_splitters(self, enable):
        for widget in widget_iter(self):
            if isinstance(widget, QSplitter):
                widget.setChildrenCollapsible(enable)

        self.settings.setValue('collapse_qsplitter', int(enable))

    def handle_file_menu_open_project(self, item):
        """Open the project of the selected item"""
        file_menu_open_project(self, item)

    def handle_file_menu_new_project(self, item):
        file_menu_new_project(self, item)

    def check_unsaved_abort(self):
        """ Popup confirmation dialog if project is unsaved """
        if self.unsaved_flag:
            confirm = self.message(
                text="Project not saved\nData will be lost!\nProceed?",
                buttons=['yes', 'no'],
                default='no')
            if confirm != 'yes':
                return True
            self.clear_unsaved_flag()
        return False

    def handle_filter_new(self, _checked):
        filter_new(self)


def _make_logo_label():
    logo_label = QLabel()
    pixmap = get_pixmap('mfix.png', 84, 84)
    logo_label.setPixmap(pixmap)
    logo_label.setAlignment(Qt.AlignCenter)
    return logo_label


def _make_blank_widget():
    bw = QWidget()
    bw.setObjectName('default')
    bw.setStyleSheet('QWidget{background-color: white;}')
    return bw
