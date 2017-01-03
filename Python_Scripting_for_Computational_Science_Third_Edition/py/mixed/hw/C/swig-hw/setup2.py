# use numpy.distutils to simplify setup.py scripts involving SWIG
from numpy.distutils.core import setup, Extension
import os

name = 'hw'           # name of the module
version = 1.0         # the module's version number
sources = ['hw.i', '../hw.c']

setup(name=name, version=version,
      ext_modules = [Extension('_' + name,  # SWIG requires _
                               sources,
                               include_dirs=[os.pardir])
                     ])

 
     
     
