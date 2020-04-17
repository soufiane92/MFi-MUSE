from collections import OrderedDict
from glob import glob
from os.path import basename, dirname, join, relpath

from PyQt5.QtWidgets import QListWidgetItem
from PyQt5.QtCore import Qt

from mfixgui.tools import get_mfix_templates
from mfixgui.tools.collect_tutorial_info import get_template_info
from mfixgui.tools.qt import get_ui, get_thumbnail, view_mode


def make_new_widget(mfixgui):
    """ Create widget for New filemenu pane """
    new_widget = get_ui('new.ui')
    new_widget.template_list.itemDoubleClicked.connect(mfixgui.handle_file_menu_new_project)
    new_widget.template_list.setAttribute(Qt.WA_MacShowFocusRect, 0)
    new_widget.template_list.setFrameStyle(new_widget.template_list.NoFrame)
    new_widget.search_bar.textChanged.connect(lambda: mfixgui.handle_filter_new(None))
    new_widget.list_button.clicked.connect(lambda: _switch_to(mfixgui, False))
    new_widget.tile_button.clicked.connect(lambda: _switch_to(mfixgui, True))

    new_widget.single_chk.toggled.connect(mfixgui.handle_filter_new)
    new_widget.tfm_chk.toggled.connect(mfixgui.handle_filter_new)
    new_widget.dem_chk.toggled.connect(mfixgui.handle_filter_new)
    new_widget.cutcell_chk.toggled.connect(mfixgui.handle_filter_new)
    new_widget.chemistry_chk.toggled.connect(mfixgui.handle_filter_new)
    new_widget.tutorials_chk.toggled.connect(mfixgui.handle_filter_new)
    new_widget.benchmarks_chk.toggled.connect(mfixgui.handle_filter_new)
    new_widget.tests_chk.toggled.connect(mfixgui.handle_filter_new)

    new_widget.single_chk.tag = 'single'
    new_widget.tfm_chk.tag = 'tfm'
    new_widget.dem_chk.tag = 'dem'
    new_widget.cutcell_chk.tag = 'cutcell'
    new_widget.chemistry_chk.tag = 'chemistry'
    new_widget.tutorials_chk.tag = 'tutorials'
    new_widget.benchmarks_chk.tag = 'benchmarks'
    new_widget.tests_chk.tag = 'tests'

    mfixgui.ui.file_menu_new_widget = new_widget
    new_widget.setObjectName('new')

    is_tile_mode = mfixgui.settings.value('open_list_mode', 'icon') == 'icon'
    _switch_to(mfixgui, is_tile_mode)

    return new_widget


def switch_tab_new(stackedwidget, mfixgui):
    """ switch to New tab """
    stackedwidget.setCurrentWidget(mfixgui.ui.file_menu_new_widget)
    _populate_templates(mfixgui.ui.file_menu_new_widget.template_list)


def _switch_to(mfixgui, is_tile_mode):
    mfixgui.settings.setValue('new_list_mode', 'icon' if is_tile_mode else 'list')
    mfixgui.ui.file_menu_new_widget.list_button.setChecked(not is_tile_mode)
    mfixgui.ui.file_menu_new_widget.template_list.setViewMode(view_mode(is_tile_mode))
    mfixgui.ui.file_menu_new_widget.tile_button.setChecked(is_tile_mode)


def _populate_templates(template_list):
    """ create templates on new tab (if not populated already) """
    if template_list.count():
        # already populated
        return
    templates = collect_templates()
    temp_info = get_template_info()
    for template_dir, paths in templates.items():
        for path in paths:
            item = _make_listwidget_item(path, template_dir, temp_info)
            template_list.addItem(item)
            item.setHidden(template_dir in ['tests'])


def collect_templates():
    ''' look for template files '''
    template_dict = OrderedDict()
    for template_category in ('tutorials', 'benchmarks', 'tests'):
        template_dir = join(get_mfix_templates(), template_category)
        template_list = sorted((dirname(relpath(mfx, template_dir)) for mfx in (
            glob(join(template_dir, '**', '*.mfx'), recursive=True) +
            glob(join(template_dir, '**', '*mfix.dat'), recursive=True)
        )), key=lambda y: y.lower())
        template_dict[template_category] = template_list
    return template_dict


def _make_listwidget_item(path, template_dir, temp_info):
    name = basename(path)
    full_path = join(get_mfix_templates(), template_dir, path)

    # extract info from the template info file
    info = temp_info.get(name, {})
    description = info.get('description', ' '*50)
    solver = info.get('solver', 'single')
    geo = info.get('geometry', 'false')
    chem = info.get('chemistry', 'false')

    # build a list of booleans for filtering templates
    # [single, tfm, pic, dem, hybrid, geometry, chemistry, tutorials, benchmarks, tests]
    template_classes = ['single', 'tfm', 'pic', 'dem', 'hybrid']
    enable_list = [solver == t for t in template_classes]
    enable_list.extend([i.lower() == 'true' for i in [geo, chem]])
    enable_list.extend([template_dir == t for t in ['tutorials', 'benchmarks', 'tests']])

    name = '\n'.join([name, description])

    item = QListWidgetItem(get_thumbnail(full_path), name)
    item.full_path = full_path
    item.tags = [t for t in template_classes if solver == t]
    if geo.lower() == 'true':
        item.tags.append('geometry')
    if chem.lower() == 'true':
        item.tags.append('chemistry')
    item.template_dir = template_dir
    return item


def file_menu_new_project(mfixgui, item):
    if mfixgui.check_unsaved_abort():
        return
    mfx_files = glob(join(item.full_path, '*.mfx'))
    if not mfx_files:
        path = join(item.full_path, 'mfix.dat')
    else:
        path = mfx_files[0]
    mfixgui.open_new_from_template(path)


def filter_new(mfixgui):
    for index in range(mfixgui.ui.file_menu_new_widget.template_list.count()):
        item = mfixgui.ui.file_menu_new_widget.template_list.item(index)
        item.setHidden(not _matches_filter(mfixgui, item))


def _matches_filter(mfixgui, item):
    name = item.text().lower()
    if name == 'blank':  # always show the blank proj
        return True
    new = mfixgui.ui.file_menu_new_widget
    label_boxes = [new.single_chk, new.tfm_chk, new.dem_chk, new.cutcell_chk, new.chemistry_chk]
    source_boxes = [new.tutorials_chk, new.benchmarks_chk, new.tests_chk]
    return (any(box.isChecked() and box.tag in item.tags for box in label_boxes) and
            any(box.isChecked() and box.tag == item.template_dir for box in source_boxes) and
            new.search_bar.text().lower() in name)
