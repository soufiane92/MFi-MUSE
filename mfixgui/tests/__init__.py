# -*- coding: utf-8 -*-

import sys

from unittest.mock import Mock

import mfixgui.widgets.vtkwidget

sys.modules['vtk'] = Mock()
mfixgui.widgets.vtkwidget.VTK_AVAILABLE = False
mfixgui.widgets.vtkwidget.VTK_IMPORT_INFO = None
