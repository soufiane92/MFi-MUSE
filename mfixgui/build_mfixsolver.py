#!/usr/bin/env python
""" Build the mfixsolver as a Python extension library. Used
by setup.py when building the mfix distribution package, and
can be run as a standalone command to build the custom mfixsolvers.
"""


import argparse
import glob
import os
from os.path import basename, exists, isdir, isfile, join, normpath
import platform
import shutil
import subprocess
import sys
import sysconfig

from mfixgui.tools import get_mfix_src, SOURCE_DIR
from mfixgui.version import __version__

def main():
    """ build_mfixsolver command: build a custom mfixsolver from the command line """
    try:
        main_args(sys.argv[1:])
    except subprocess.CalledProcessError as err:
        print("==============================================================",
              "                      BUILD UNSUCCESSFUL",
              sep=os.linesep)
        if err.stdout:
            print("Output:")
            print(err.stdout)
        if err.stderr:
            print("Error messages:")
            print(err.stderr)
        print("==============================================================")
        sys.exit(err.returncode)

def main_args(args):
    """ Parse command line args, and build target """

    config, cmake_args = _parse_args(args)
    rundir = os.getcwd()
    _check_rundir(rundir)

    if config.clean:
        removed_stuff = do_clean(rundir)
        if not removed_stuff:
            print('Nothing to clean.')
        else:
            print(removed_stuff)
        return
    _check_for_mfx_file()

    builddir = _get_builddir(rundir)
    os.chdir(builddir)
    _remove_cmakecache(builddir)

    if config.postmfix:
        exe = _build_post(builddir, config, cmake_args)
        clobber_copy(exe, rundir)

    elif config.batch or config.crow or config.server in ("none", "crow"):
        exe = _build_batch(rundir, builddir, config, cmake_args)
        clobber_copy(exe, rundir)

    elif config.server == "pymfix":
        exe = build_pymfix(rundir, builddir, config, cmake_args)
        solver_path = _make_solver_path(rundir)
        clobber_copy(exe, solver_path)
        _make_wrapper(rundir, solver_path, config.dmp)

    else:
        raise "Bad arguments; should not get here"
    os.chdir(rundir)
    sys.stdout.flush()


def _get_builddir(rundir):
    builddir = join(rundir, 'build')
    if not exists(builddir):
        os.makedirs(builddir)
    return builddir


def clobber_copy(exe, destdir):
    """ copy exe to destdir, creating directories and overwriting if needed """
    if not exists(destdir):
        os.makedirs(destdir)
    destination = join(destdir, basename(exe))
    if exists(destination):
        os.remove(destination)
    os.link(exe, destination)


def _remove_cmakecache(builddir):
    """remove CMakeCache.txt between build to not confuse the user with cached
    values for compiler, compiler flags, etc"""
    cmake_cache = join(builddir, 'CMakeCache.txt')
    if isfile(cmake_cache):
        os.remove(cmake_cache)


def _check_for_mfx_file():
    mfx_files = glob.glob('*.mfx')
    if len(mfx_files) > 1:
        print("ERROR, multiple .mfx files found:")
        for mfx_file in mfx_files:
            print(mfx_file)
        sys.exit(-1)
    elif len(mfx_files) == 1:
        print(f"Building custom solver for {mfx_files[0]}")
    elif glob.glob('mfix.dat'):
        print("Building custom solver for mfix.dat")
    sys.stdout.flush()


def _check_rundir(rundir):
    # The '=' character in the builddir path causes errors, because CMake Makefile target is based on the builddir path
    invalid_path_chars = ['=']
    for char in invalid_path_chars:
        if char in rundir:
            print("Unable to build solver because the build path:")
            print()
            print(rundir)
            print()
            print(f"Contains the {char} character. Move or rename your build path to remove the character {char}.")
            sys.exit(-1)


def _parse_args(args):

    parser = argparse.ArgumentParser(description='Wrapper script to build MFiX Solver for running CMake and GNU Make')
    parser.add_argument('-v', '--version', action='version', version=__version__)
    parser.add_argument('--clean', action='store_true', help='Clean build artifacts (instead of building solver)')
    parser.add_argument('--postmfix', action='store_true', help='Build postmfix (instead of the solver)')
    parser.add_argument('--dmp', '--enable-dmp', action='store_true', help='Build with DMP (MPI) support')
    parser.add_argument('--smp', '--enable-smp', action='store_true', help='Build with SMP (OpenMP) support')
    parser.add_argument('-j', '--jobs', nargs='?', type=int, const=4, default=1, action='store',
                        help='Run make with multiple parallel jobs')
    group = parser.add_mutually_exclusive_group()
    group.add_argument('--batch', action='store_true',
                       help='Build solver without support for GUI interaction (same as --server=none)')
    group.add_argument('--crow', action='store_true',
                       help='Build solver with C++ implementation of GUI interaction (same as --server=crow)')
    group.add_argument('-s', '--server', default="pymfix",
                       choices=["none", "pymfix", "crow"],
                       help="Support for MFiX GUI interactivity (default is --server=pymfix)")

    args, cmake_args = parser.parse_known_args(args)

    if 'clean' in cmake_args:
        cmake_args.remove('clean')
        args.clean = True

    if 'postmfix' in cmake_args:
        cmake_args.remove('postmfix')
        args.postmfix = True

    fortran_compiler = '-DMPI_Fortran_COMPILER=' if args.dmp else '-DCMAKE_Fortran_COMPILER='
    cmake_args = [
        arg.replace('FC=', fortran_compiler)
        if arg.startswith('FC=') else arg for arg in cmake_args
    ]

    cmake_args = [
        arg.replace('FCFLAGS=', '-DCMAKE_Fortran_FLAGS=')
        if arg.startswith('FCFLAGS=') else arg for arg in cmake_args
    ]
    cmake_args = [
        arg.replace('CC=', '-DCMAKE_C_COMPILER=')
        if arg.startswith('CC=') else arg for arg in cmake_args
    ]

    if args.crow or args.server == 'crow':
        cmake_args.append('-DENABLE_CROW=ON')

    if args.dmp:
        cmake_args.append('-DENABLE_MPI=1')

    if args.smp:
        cmake_args.append('-DENABLE_OpenMP=1')

    return args, cmake_args


def generate_makefile(args, rundir=None):
    """ Run CMake to generate Makefile(s) """

    if platform.system() != 'Windows':
        generator = 'Unix Makefiles'
    elif shutil.which('sh'):
        generator = 'MSYS Makefiles'
    else:
        generator = 'MinGW Makefiles'

    args.extend([
        '-G', generator,
        '-DVERSION={}'.format(__version__),
        get_mfix_src(),
    ])
    if rundir is not None:
        args.append('-DUDF_DIR={}'.format(rundir))
    cmd = ['cmake'] + args
    print("Running cmake command:")
    print()
    print(*cmd)
    print()
    subprocess.check_call(cmd)


def do_clean(basedir):
    """ delete all build artifacts: build/ lib/ mfixsolver mfixsolver.so etc. """
    removed_stuff = ''
    dirs_to_clean = [
        join(basedir, name)
        for name in ('build', '.build', 'lib')
        if exists(join(basedir, name))
    ]
    for dir_to_clean in dirs_to_clean:
        removed_stuff += 'Removing directory: {}\n'.format(dir_to_clean)
        shutil.rmtree(dir_to_clean, ignore_errors=True)

    files_to_clean = [
        join(basedir, name)
        for name in (
            'Makefile',
            'Makefile.msys',
            'mfixsolver',
            'mfixsolver.bat',
            'mfixsolver.exe',
            'postmfix',
            'postmfix.exe',
        )
        if exists(join(basedir, name))
    ]
    for file_to_clean in files_to_clean:
        removed_stuff += 'Removing: {}\n'.format(file_to_clean)
        try:
            os.remove(file_to_clean)
        except OSError:
            pass
    return removed_stuff

def _build_post(builddir, config, cmake_args):
    generate_makefile(cmake_args)
    build_target("postmfix", config.jobs)
    print(
        "==============================================================",
        "                      BUILD SUCCESSFUL",
        "",
        "   To run solver from command line:  ./postmfix",
        "==============================================================",
        sep=os.linesep)
    exe = '.exe' if platform.system() == 'Windows' else ''
    return join(builddir, f'postmfix{exe}')

def get_ext_suffix():
    """ Extension module file suffix (for example .cpython-36m-x86_64-linux-gnu.so) """
    return sysconfig.get_config_var('EXT_SUFFIX') or sysconfig.get_config_var('SO')

def build_pymfix(rundir, builddir, config, cmake_args):
    """ build pymfix extension module is builddir """
    cmake_args.append('-DENABLE_PYMFIX=ON')
    generate_makefile(cmake_args, rundir)
    build_target("mfixsolver_ext", config.jobs)
    print(
        "==============================================================",
        "                      BUILD SUCCESSFUL",
        "",
        "   To run solver from command line:  ./mfixsolver",
        "",
        "   To run solver from GUI:  Select [project]/mfixsolver in the Run dialog",
        "==============================================================",
        sep=os.linesep)
    suffix = get_ext_suffix()
    return join(builddir, f'mfixsolver{suffix}')


def _build_batch(rundir, builddir, config, cmake_args):
    generate_makefile(cmake_args, rundir)
    build_target("mfixsolver", config.jobs)
    print(
        "==============================================================",
        "                      BUILD SUCCESSFUL",
        "",
        "   To run solver from command line:  ./mfixsolver",
        "==============================================================",
        sep=os.linesep)
    exe = '.exe' if platform.system() == 'Windows' else ''
    return join(builddir, f'mfixsolver{exe}')


def build_target(target, jobs):
    """ Run Make """

    if platform.system() == 'Windows' and not shutil.which('sh'):
        make = 'mingw32-make'
    else:
        make = 'make'

    jobs = [] if jobs == 1 else ['-j', str(jobs)]
    cmd = [make] + jobs + [target]

    print("Running make command:")
    print()
    print(*cmd)
    print()
    subprocess.check_call(cmd)

def _make_solver_path(rundir):
    solver_path = join(
        rundir,
        'lib',
        'python%d.%d' % sys.version_info[:2],
        'site-packages')
    os.environ['PYTHONPATH'] = solver_path

    if not isdir(solver_path):
        os.makedirs(solver_path)

    return solver_path

def _make_wrapper(rundir, solver_path, dmp):

    # if building with mfixgui/build.sh, add source dir to PYTHONPATH
    solver_path = os.pathsep.join([solver_path, SOURCE_DIR]) if SOURCE_DIR is not None else solver_path

    # Creates a wrapper script which prepends SOLVER_PATH to PYTHONPATH
    # so that the correct solver module will be found at runtime
    sh_template = '\n'.join([
        r'#!/bin/sh',
        r'',
        r'env {LD_PRELOAD_MPI} PYTHONPATH="{SOLVER_PATH}"${PYTHONPATH:+:$PYTHONPATH} {PYEXE} -m mfixgui.pymfix "$@"',
        r'',
    ])

    bat_template = '\n'.join([
        r'@echo on',
        r'',
        r'setlocal',
        r'set PYTHONPATH={SOLVER_PATH};%PYTHONPATH%',
        r'call "{PYEXE}" -m mfixgui.pymfix %*',
        r'endlocal',
        r'',
    ])

    if platform.system() == 'Windows':
        mfixsolver = join(rundir, 'mfixsolver.bat')
        template = bat_template
    else:
        mfixsolver = join(rundir, 'mfixsolver')
        template = sh_template

    contents = template.replace('{SOLVER_PATH}', solver_path).replace('{PYEXE}', sys.executable)

    if dmp:
        replacement = 'LD_PRELOAD=libmpi.so'
        prefix = normpath(sys.prefix)
        # On some configurations (eg Anaconda5 on Ubuntu 16.04) the solver can't
        #  find libpython.so for DMP-enabled builds, so we reluctantly and carefully
        #  set LD_LIBRARY_PATH
        # See https://stackoverflow.com/questions/9631228/how-to-smart-append-ld-library-path-in-shell-when-nounset
        if prefix != '/usr':
            # Append to LD_LIBRARY_PATH
            libpath = join(prefix, 'lib')
            replacement += ' LD_LIBRARY_PATH=${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}%s'%libpath

        contents = contents.replace('{LD_PRELOAD_MPI}', replacement)
    else:
        contents = contents.replace('{LD_PRELOAD_MPI}', '')

    with open(mfixsolver, 'wt') as wrapper:
        wrapper.write(contents)

    set_exe(mfixsolver)

    return mfixsolver

def set_exe(mfixsolver):
    """ set executable permission for wrapper """
    mode = os.stat(mfixsolver).st_mode
    os.chmod(mfixsolver, mode | 0o111)


if __name__ == '__main__':
    main()
