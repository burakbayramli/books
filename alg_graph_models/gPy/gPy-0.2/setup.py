#!/usr/bin/env python

from distutils.core import setup, Extension

setup(name='gPy',
      version='0.2',
      description='Modules for graphical modules',
      author='James Cussens',
      author_email='jc@cs.york.ac.uk',
      url='http://www-users.cs.york.ac.uk/~jc/teaching/agm/gPy/',
      packages=['gPy'],
      ext_modules=[Extension('gPy.gPyC', ['gPyC.c'],
                             define_macros=[('HAVE_GSL',None)],
                             libraries=['gsl','gslcblas'])])
