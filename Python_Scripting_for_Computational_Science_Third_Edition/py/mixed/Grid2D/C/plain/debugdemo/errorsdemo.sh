#!/bin/sh
# demonstrate the errors in the C files
./make_module_1.sh gridloop1
./make_module_1.sh gridloop2
./make_module_1.sh gridloop3 -DDEBUG
./make_module_1.sh gridloop4
./make_module_1.sh gridloop5

