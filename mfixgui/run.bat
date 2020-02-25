@echo off
:: Wrapper script to run MFiX GUI from source directory

setlocal

pushd %~dp0
cd ..
set MFIX_HOME=%CD%
set PYTHONPATH=%MFIX_HOME%
popd

python -m mfixgui -d %*

endlocal
