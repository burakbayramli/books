from distutils.core import setup, Extension
import os, numpy

name = 'ext_gridloop'
setup(name=name,
      include_dirs=[os.path.join(os.environ['scripting'],
                                 'src', 'C'),
                    numpy.get_include()],
      ext_modules=[Extension(name, ['gridloop_C.c', 'gridloop_wrap.c'])])
