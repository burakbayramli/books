import os, numpy
from distutils.core import setup, Extension
name = 'ext_gridloop'
setup(name=name,
      include_dirs=[numpy.get_include(),
                    os.curdir,  # need this because if include <NumPyArray.h>
                    ],
      ext_modules=[Extension(name,
                             sources=['gridloop.cpp'])])
