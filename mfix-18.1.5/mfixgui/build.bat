@echo off
:: wrapper script for running build_mfixsolver from source on Windows

setlocal

pushd %~dp0
cd ..
set MFIX_HOME=%CD%
set PYTHONPATH=%MFIX_HOME%
popd

python -m mfixgui.build_mfixsolver %*

endlocal
