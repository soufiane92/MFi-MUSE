@echo on
setlocal EnableDelayedExpansion

cd %SRC_DIR%

bash bootstrap.sh

b2.exe -j 2 --layout=system variant=debug link=static runtime-link=static threading=multi --libdir=%LIBRARY_LIB% install
b2.exe -j 2 --layout=system variant=release link=static runtime-link=static threading=multi --libdir=%LIBRARY_LIB% install