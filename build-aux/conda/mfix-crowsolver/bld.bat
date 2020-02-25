set CC=gcc
set CXX=g++
set FC=gfortran

rmdir /S /Q build
mkdir build
cd build
cmake .. -G "MinGW Makefiles" -DENABLE_CROW=ON -DBOOST_ROOT=%CONDA_PREFIX% || exit /b 1
mingw32-make || exit /b 1
copy mfixsolver.exe %LIBRARY_BIN%\mfixsolver-crow.exe || exit /b 1
