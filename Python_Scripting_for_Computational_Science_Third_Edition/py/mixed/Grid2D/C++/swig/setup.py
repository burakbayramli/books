import os, numpy
from numpy.distutils.core import setup, Extension
name = 'ext_gridloop'
setup(name=name,
      include_dirs=[numpy.get_include(),
                    os.curdir,
                    '../plain',
                    ],
      ext_modules=[Extension(name,
                             sources=['gridloop.cpp'])])
