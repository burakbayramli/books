#!/usr/bin/env python
from numpy.distutils.core import Extension, setup

setup(name='hw',
      description='Simple example on calling F90 from Python',
      author='Hans Petter Langtangen',
      author_email='hpl@simula.no',
      ext_modules=[Extension(name='hw', sources=['../hw.f90'])],
      )
