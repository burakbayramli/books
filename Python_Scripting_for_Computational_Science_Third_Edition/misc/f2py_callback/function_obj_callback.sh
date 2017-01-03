#!/bin/sh -x
# build extension module with f2py
f2py -m tmp -c --build-dir tmp_build someotherfile.f
