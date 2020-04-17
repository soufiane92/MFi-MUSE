"""A setuptools based setup module for the MFiX GUI.

See:
https://packaging.python.org/en/latest/distributing.html
https://mfix.netl.doe.gov/
"""

import argparse
import io
import platform
import subprocess
import sys

from os import path, walk, environ

from setuptools import setup, find_packages
from setuptools.command.build_ext import build_ext
from setuptools.extension import Extension

from mfixgui.build_mfixsolver import (generate_makefile, build_target,
                                      get_ext_suffix, clobber_copy)
from mfixgui.namelistparser import build_keywords_rst
from mfixgui.version import __version__

# Get the long description from the README file
with io.open(path.join(path.abspath(path.dirname(__file__)),
                       'README.rst'),
             encoding='utf-8') as readme:
    LONG_DESCRIPTION = readme.read()

PARSER = argparse.ArgumentParser()
PARSER.add_argument('--package', choices=['gui', 'solver', 'src', 'doc'])
ARGS, sys.argv = PARSER.parse_known_args(sys.argv)

LEGACY = ARGS.package is None
INCLUDE_GUI = ARGS.package in ('gui', None)
INCLUDE_SOLVER = ARGS.package in ('solver', None)

for var in ['FFLAGS', 'FORTRANFLAGS', 'DEBUG_FFLAGS', 'DEBUG_FORTRANFLAGS']:
    if var in environ:
        environ[var] = environ[var].replace('-fopenmp', '')
        environ[var] = environ[var].replace('-ftree-vectorize', '')

def build_ext_decorator(command_subclass):
    """A decorator for classes subclassing one of the setuptools commands.

    It modifies the run() method to run the mfixsolver extension with CMake and Make
    """

    def build_ext_modified(self):
        """ Overrides the build_ext command"""

        gcc = ['-DCMAKE_C_COMPILER=gcc'] if platform.system() == 'Darwin' else []
        generate_makefile(['-DENABLE_PYMFIX=ON'] + gcc)
        build_target("mfixsolver_ext", 1)
        suffix = get_ext_suffix()
        clobber_copy(f"mfixsolver{suffix}", self.build_lib)

    command_subclass.run = build_ext_modified
    return command_subclass

@build_ext_decorator
class BuildExtCommand(build_ext):
    """ Override build_ext class to modify build_ext setuptools command """
    pass

EXT_MODULES = [Extension(name='mfixsolver', sources=[])] if INCLUDE_SOLVER else []
CMDCLASS = {'build_ext': BuildExtCommand,} if INCLUDE_SOLVER else {}


NAME = 'mfix'

def get_doc_files():
    """ returns list of (DIR, [BASENAME]) to include doc files """

    # Generate *_kw.rst files:
    build_keywords_rst(path.join('doc', 'user_manual', 'reference'))

    sphinx_cmd = [
        'sphinx-build',
        '-M', 'html',
        '.', '_build',
        '-t', 'release'
    ]
    subprocess.check_call(sphinx_cmd, cwd='doc')

    return [
        (path.join(NAME, root), [path.join(root, f)
                                 for f in files])
        for root, _, files in walk(path.join('doc', '_build', 'html'))]


def get_src_files():
    """ returns list of (DIR, [BASENAME]) to include src files """

    return [
        (path.join(NAME, root), [path.join(root, f)
                                 for f in files
                                 if 'CMakeFiles' not in root and not f.endswith('mod')])
        for subdir in [
            'build-aux',
            'model',
            'post_mfix',
        ]
        for root, _, files in walk(subdir)
    ] + [(NAME, ['CMakeLists.txt'])]


def get_template_files():
    """ returns list of (DIR, [BASENAME]) to include non-Python GUI files """

    return [
        (path.join(NAME, root), [path.join(root, f)
                                 for f in files
                                 if 'CMakeFiles' not in root and not f.endswith('mod')])
        for subdir in [
            'queue_templates',
            'tests',
            'tutorials',
        ]
        for root, _, files in walk(subdir)
    ]

setup(
    name=NAME,
    cmdclass=CMDCLASS,

    # Versions should comply with PEP440.  For a discussion on single-sourcing
    # the version across setup.py and the project code, see
    # https://packaging.python.org/en/latest/single_source_version.html
    version=__version__,
    description='MFiX computational fluid dynamics software',
    long_description=LONG_DESCRIPTION,

    # The project's main homepage.
    url='https://mfix.netl.doe.gov/',

    # Author details
    author='Multiflow Science Group at NETL',
    platforms=["any"],

    # Choose your license
    license='public domain',

    # To avoid installing in an .egg subdirectory
    zip_safe=False,

    # See https://pypi.python.org/pypi?%3Aaction=list_classifiers
    classifiers=[
        'Development Status :: 5 - Production/Stable',
        'Intended Audience :: Developers',
        'Topic :: Computational Fluid Dynamics :: GUI',
        'License :: public domain',
        'Programming Language :: Python :: 3.6',
    ],

    packages=find_packages() if INCLUDE_GUI else [],

    # List run-time dependencies here.  These will be installed by pip when
    # your project is installed. For an analysis of "install_requires" vs pip's
    # requirements files see:
    # https://packaging.python.org/en/latest/requirements.html
    install_requires=([
        'matplotlib',
        'numpy',
        'psutil',
        'pyqtgraph',
        'qtpy>=1.2.1',
        'simplejson',
    ]) + (['flask'] if INCLUDE_SOLVER else []),

    ext_modules=EXT_MODULES,

    package_data={
        'mfixgui.colormaps': ['*'],
        'mfixgui.images': ['*.png', '*.svg'],
        'mfixgui.tools': ['template_data.json'],
        'mfixgui.uifiles': ['*'],
        'mfixgui.widgets': ['burcat.pickle'],
    } if INCLUDE_GUI else {},

    # Although 'package_data' is the preferred approach, in some case you may
    # need to place data files outside of your packages. See:
    # https://docs.python.org/3.4/distutils/setupscript.html#installing-additional-files # noqa
    # In this case, 'data_file' will be installed into '<sys.prefix>/my_data'
    data_files=((get_template_files() + get_src_files() + get_doc_files()) if LEGACY else []),

    # To provide executable scripts, use entry points in preference to the
    # "scripts" keyword. Entry points provide cross-platform support and allow
    # pip to create the appropriate form of executable for the target platform.

    entry_points={
        'console_scripts': (
            (['mfixsolver=mfixgui.pymfix:main'] if INCLUDE_SOLVER else [])
            + ['build_mfixsolver=mfixgui.build_mfixsolver:main',
               'mfixversioninfo=mfixgui.version_info:main'],
        ),

        'gui_scripts': [
            'mfix=mfixgui.gui:main',
        ],
    } if not environ.get('CONDA_BUILD') else {},
)
