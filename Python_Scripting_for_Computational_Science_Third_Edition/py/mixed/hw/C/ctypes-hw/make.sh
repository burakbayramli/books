#!/bin/sh
# make shared library:
gcc -shared -o hw.so ../hw.c

# run Python script:
python hwa.py

