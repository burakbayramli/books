#!/usr/bin/env python
from numpy.distutils.core import Extension, setup

name = 'gridloop_ext'
setup(name=name,
      ext_modules=[
    Extension(name=name,
              sources=['gridloop.f'],
              f2py_options=["--fcompiler='Gnu'",
                            "--build-dir tmp1",
                            "-DF2PY_REPORT_ON_ARRAY_COPY=1")],
      )
