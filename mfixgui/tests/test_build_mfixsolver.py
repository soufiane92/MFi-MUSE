# -*- coding: utf-8 -*-
"""
run with pytest
"""


import os
import platform
import shutil

import pytest

import mfixgui.build_mfixsolver
from mfixgui.version import __version__


def winpath(path):
    """ Windows path """
    return path.replace('/', os.sep)

GENERATOR = [
    '-G',
    'MSYS Makefiles' if shutil.which('sh') else 'MinGW Makefiles',
] if platform.system() == 'Windows' else [
    '-G',
    'Unix Makefiles',
]
MAKE = 'mingw32-make' if platform.system() == 'Windows' and not shutil.which('sh') else 'make'

def test_check_mfxs(mocker):
    glob = mocker.patch('mfixgui.build_mfixsolver.glob.glob')
    glob.return_value = ['one.mfx', 'two.mfx']
    args = []
    with pytest.raises(SystemExit):
        mfixgui.build_mfixsolver.main_args(args)


def test_clean(mocker):
    cwd, exists, os_remove, rmtree = (
        mocker.patch('mfixgui.build_mfixsolver.os.getcwd'),
        mocker.patch('mfixgui.build_mfixsolver.exists'),
        mocker.patch('mfixgui.build_mfixsolver.os.remove'),
        mocker.patch('mfixgui.build_mfixsolver.shutil.rmtree'),
    )

    mocker.patch('mfixgui.build_mfixsolver.os.chdir'),

    cwd.return_value = 'foo_dir'
    exists.return_value = True
    args = ['clean']
    mfixgui.build_mfixsolver.main_args(args)
    rmtree.assert_has_calls([
        mocker.call(winpath('foo_dir/build'), ignore_errors=True),
        mocker.call(winpath('foo_dir/.build'), ignore_errors=True),
        mocker.call(winpath('foo_dir/lib'), ignore_errors=True),
    ])
    os_remove.assert_has_calls([
        mocker.call(winpath('foo_dir/Makefile')),
        mocker.call(winpath('foo_dir/Makefile.msys')),
        mocker.call(winpath('foo_dir/mfixsolver')),
        mocker.call(winpath('foo_dir/mfixsolver.bat')),
        mocker.call(winpath('foo_dir/mfixsolver.exe')),
        mocker.call(winpath('foo_dir/postmfix')),
        mocker.call(winpath('foo_dir/postmfix.exe')),
    ])


def test_postmfix(mocker):
    get_mfix_src, link, _, _, _, check_call = (
        mocker.patch('mfixgui.build_mfixsolver.get_mfix_src'),
        mocker.patch('mfixgui.build_mfixsolver.os.link'),
        mocker.patch('mfixgui.build_mfixsolver.os.remove'),
        mocker.patch('mfixgui.build_mfixsolver.os.symlink'),
        mocker.patch('mfixgui.build_mfixsolver.shutil.copy'),
        mocker.patch('mfixgui.build_mfixsolver.subprocess.check_call'),
    )
    args = ['postmfix']
    get_mfix_src.return_value = 'MFIX_HOME'
    mfixgui.build_mfixsolver.main_args(args)
    link.assert_called_once()

    check_call.assert_has_calls([
        mocker.call([
            'cmake',
        ] + GENERATOR + [
            '-DVERSION={}'.format(__version__),
            'MFIX_HOME',
        ]),
        mocker.call([MAKE, 'postmfix']),
    ])


def test_batch(mocker):
    get_mfix_src, glob, link, check_call = (
        mocker.patch('mfixgui.build_mfixsolver.get_mfix_src'),
        mocker.patch('mfixgui.build_mfixsolver.glob.glob'),
        mocker.patch('mfixgui.build_mfixsolver.os.link'),
        mocker.patch('mfixgui.build_mfixsolver.subprocess.check_call'),
    )

    mocker.patch('mfixgui.build_mfixsolver.os.remove')
    mocker.patch('mfixgui.build_mfixsolver.os.symlink')
    mocker.patch('mfixgui.build_mfixsolver.shutil.copy')

    glob.return_value = ['foo.mfx']
    args = ['--batch', '-DCMAKE_foo="bar baz"']
    get_mfix_src.return_value = 'MFIX_HOME'
    mfixgui.build_mfixsolver.main_args(args)
    link.assert_called_once()

    check_call.assert_has_calls([
        mocker.call([
            'cmake',
            '-DCMAKE_foo="bar baz"',
        ] + GENERATOR + [
            '-DVERSION={}'.format(__version__),
            'MFIX_HOME',
            '-DUDF_DIR={}'.format(os.getcwd()),
        ]),
        mocker.call([MAKE, 'mfixsolver']),
    ])


def test_interactive(mocker):
    get_mfix_src, stat, check_call = (
        mocker.patch('mfixgui.build_mfixsolver.get_mfix_src'),
        mocker.patch('mfixgui.build_mfixsolver.os.stat'),
        mocker.patch('mfixgui.build_mfixsolver.subprocess.check_call'),
    )

    mocker.patch('mfixgui.build_mfixsolver.open')
    mocker.patch('mfixgui.build_mfixsolver.os.link')
    mocker.patch('mfixgui.build_mfixsolver.os.chmod')
    mocker.patch('mfixgui.build_mfixsolver.os.makedirs')
    mocker.patch('mfixgui.build_mfixsolver.os.remove')

    args = ['-DCMAKE_foo="bar baz"', '-j']
    get_mfix_src.return_value = 'MFIX_HOME'
    stat.return_value.st_mode = 0

    mfixgui.build_mfixsolver.main_args(args)

    check_call.assert_has_calls([
        mocker.call([
            'cmake',
            '-DCMAKE_foo="bar baz"',
            '-DENABLE_PYMFIX=ON',
        ] + GENERATOR + [
            '-DVERSION={}'.format(__version__),
            'MFIX_HOME',
            '-DUDF_DIR={}'.format(os.getcwd()),
        ]),
        mocker.call([MAKE, '-j', '4', 'mfixsolver_ext']),
    ])


def test_interactive_smp(mocker):
    get_mfix_src, stat, check_call = (
        mocker.patch('mfixgui.build_mfixsolver.get_mfix_src'),
        mocker.patch('mfixgui.build_mfixsolver.os.stat'),
        mocker.patch('mfixgui.build_mfixsolver.subprocess.check_call'),
    )

    mocker.patch('mfixgui.build_mfixsolver.open')
    mocker.patch('mfixgui.build_mfixsolver.os.chmod')
    mocker.patch('mfixgui.build_mfixsolver.os.link')
    mocker.patch('mfixgui.build_mfixsolver.os.makedirs')
    mocker.patch('mfixgui.build_mfixsolver.os.remove')

    args = ['--smp']
    get_mfix_src.return_value = 'MFIX_HOME'
    stat.return_value.st_mode = 0

    mfixgui.build_mfixsolver.main_args(args)

    check_call.assert_has_calls([
        mocker.call([
            'cmake',
            '-DENABLE_OpenMP=1',
            '-DENABLE_PYMFIX=ON',
        ] + GENERATOR + [
            '-DVERSION={}'.format(__version__),
            'MFIX_HOME',
            '-DUDF_DIR={}'.format(os.getcwd()),
        ]),
        mocker.call([MAKE, 'mfixsolver_ext']),
    ])


def test_interactive_dmp(mocker):
    get_mfix_src, check_call = (
        mocker.patch('mfixgui.build_mfixsolver.get_mfix_src'),
        mocker.patch('mfixgui.build_mfixsolver.subprocess.check_call'),
    )

    mocker.patch('mfixgui.build_mfixsolver.os.link')
    mocker.patch('mfixgui.build_mfixsolver.open')
    mocker.patch('mfixgui.build_mfixsolver.set_exe')

    args = ['--dmp']
    get_mfix_src.return_value = 'MFIX_HOME'
    mfixgui.build_mfixsolver.main_args(args)

    check_call.assert_has_calls([
        mocker.call([
            'cmake',
            '-DENABLE_MPI=1',
            '-DENABLE_PYMFIX=ON',
        ] + GENERATOR + [
            '-DVERSION={}'.format(__version__),
            'MFIX_HOME',
            '-DUDF_DIR={}'.format(os.getcwd()),
        ]),
        mocker.call([MAKE, 'mfixsolver_ext']),
    ])


def test_interactive_udfs(mocker):

    get_mfix_src, check_call = (
        mocker.patch('mfixgui.build_mfixsolver.get_mfix_src'),
        mocker.patch('mfixgui.build_mfixsolver.subprocess.check_call'),
    )

    mocker.patch('mfixgui.build_mfixsolver.open')
    mocker.patch('mfixgui.build_mfixsolver.os.link')
    mocker.patch('mfixgui.build_mfixsolver.set_exe')

    args = ['--dmp']
    get_mfix_src.return_value = 'MFIX_HOME'
    mfixgui.build_mfixsolver.main_args(args)
    check_call.assert_has_calls([
        mocker.call([
            'cmake',
            '-DENABLE_MPI=1',
            '-DENABLE_PYMFIX=ON',
        ] + GENERATOR + [
            '-DVERSION={}'.format(__version__),
            'MFIX_HOME',
            '-DUDF_DIR={}'.format(os.getcwd()),
        ]),
        mocker.call([MAKE, 'mfixsolver_ext']),
    ])
