"""Define the __version__ string imported by other code both at
buildtime and runtime. version numbering from PEP 440"""

import os
import subprocess
import pkg_resources

from mfixgui.tools import SCRIPT_DIRECTORY

PKG_VERSION = os.environ.get('PKG_VERSION', None)
PKG_BUILDNUM = os.environ.get('PKG_BUILDNUM', None)

if PKG_VERSION:
    if PKG_BUILDNUM:
        __version__ = '%s-%s' % (PKG_VERSION, PKG_BUILDNUM)
    else:
        __version__ = '%s' % PKG_VERSION
else:
    try:
        # if mfix is installed with pip, return installed version
        __version__ = pkg_resources.get_distribution("mfix").version
    except pkg_resources.DistributionNotFound:
        # running from development version
        DEFAULT_VERSION = u"unknown"
        try:
            __version__ = subprocess.check_output(['git',
                                                   '-C', SCRIPT_DIRECTORY,
                                                   'describe', '--always'],
                                                  stderr=subprocess.STDOUT).strip().decode('utf-8')
        except subprocess.CalledProcessError:
            __version__ = DEFAULT_VERSION
        except OSError:
            __version__ = DEFAULT_VERSION
