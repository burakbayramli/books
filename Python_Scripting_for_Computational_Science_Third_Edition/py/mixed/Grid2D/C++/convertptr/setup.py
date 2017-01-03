import os, commands, numpy
from numpy.distutils.core import setup, Extension
name = 'ext_gridloop'
sources = ['ext_gridloop.i', 'gridloop.cpp', 'convert.cpp']
setup(name=name,
      include_dirs=[os.curdir,
                    numpy.get_include()],
      ext_modules=[Extension('_' + name,  # SWIG requires _
                             sources=sources)])
