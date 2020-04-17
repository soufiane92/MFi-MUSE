set CC=gcc
set CXX=g++
set FC=gfortran
"%PYTHON%" setup.py install --package=solver
cd "%SP_DIR%"\mfix*
strip *.pyd
if errorlevel 1 exit 1
