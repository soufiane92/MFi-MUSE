#!/usr/bin/env python

from distutils import spawn
import os
from os.path import join, dirname, realpath
from glob import glob
import platform
import sys

from mfixgui.tools import get_mfix_home, get_mfix_src
from mfixgui.widgets.run_popup import MFIXSOLVER_GLOB_NAMES
from mfixgui.version import __version__ as MFIX_VERSION

try:
    from qtpy import QT_VERSION
except ImportError:
    QT_VERSION = 'Unavailable'

try:
    from qtpy import API_NAME
    from qtpy import __version__ as QTPY_VERSION
except ImportError:
    API_NAME = QTPY_VERSION = 'Unavailable'

try:
    import numpy as np
    NUMPY_VERSION = np.__version__
except ImportError:
    NUMPY_VERSION = 'Unavailable'

try:
    from vtk import vtkVersion
    VTK_VERSION = vtkVersion.GetVTKVersion()
except ImportError:
    VTK_VERSION = 'Unavailable'

OPENGL_VERSION = None
try:
    from vtk import vtkRenderingOpenGL
    OPENGL_VERSION = 1
except ImportError:
    pass
try:
    from vtk import vtkRenderingOpenGL2
    OPENGL_VERSION = 2
except ImportError:
    pass

try:
    from nodeworks import __version__ as NODEWORKS_VERSION
except ImportError:
    NODEWORKS_VERSION = 'Unavailable'

try:
    from flask import __version__ as FLASK_VERSION
except ImportError:
    FLASK_VERSION = 'Unavailable'

try:
    from sphinx import __version__ as SPHINX_VERSION
except ImportError:
    SPHINX_VERSION = 'Unavailable'

try:
    from psutil import __version__ as PSUTIL_VERSION
except ImportError:
    PSUTIL_VERSION = 'Unavailable'

UNAME = platform.uname()
SYSTEM_INFO = '%s release %s running on %s' % (
    UNAME.system, UNAME.release, UNAME.machine)


def _find_default_solver():
    solvers = [spawn.find_executable(solver)
               for g in MFIXSOLVER_GLOB_NAMES
               for solver in glob(join(dirname(realpath(sys.executable)), g))]
    default_solver = solvers[0] if solvers else None
    return default_solver

LIBRARY_VERSIONS = [
    ('MFiX version', MFIX_VERSION),
    ('Python version', sys.version.replace(os.linesep, ' ')),
    ('Qt Wrapper', API_NAME),
    ('Qt Version', QT_VERSION),
    ('qtpy Version', QTPY_VERSION),
    ('Numpy Version', NUMPY_VERSION),
    ('Nodeworks Version', NODEWORKS_VERSION),
    ('Flask Version', FLASK_VERSION),
    ('Sphinx Version', SPHINX_VERSION),
    ('psutil Version', PSUTIL_VERSION),
    ('VTK Version', VTK_VERSION),
    ('OpenGL Backend', OPENGL_VERSION),
    ('System info', SYSTEM_INFO),
    ('Install location', get_mfix_home()),
    ('Default Solver', _find_default_solver()),
    ('Fortran source', get_mfix_src()),
]

def get_version_info():
    """ return version info as an list (generator) of lines """
    for label, version in LIBRARY_VERSIONS:
        spaces = ' '*int(20-len(label))
        yield "%s:%s%s" % (label, spaces, str(version).strip())

def main():
    for line in get_version_info():
        print(line)

if __name__ == '__main__':
    main()
