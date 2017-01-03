#!/usr/bin/env python
from numpy.distutils.core import Extension, setup
import os

name = 'ext_gridloop'

# make pyf file:
os.system('f2py -m ext_gridloop -h ext_gridloop.pyf '\
          '--overwrite-signature signatures.f')
    
setup(name=name,
      ext_modules=[
    Extension(name=name,
              sources=['ext_gridloop.pyf', 'gridloop.c'],
#              f2py_options=["--build-dir tmp1",
#                            "-DF2PY_REPORT_ON_ARRAY_COPY=1"],
      )])
