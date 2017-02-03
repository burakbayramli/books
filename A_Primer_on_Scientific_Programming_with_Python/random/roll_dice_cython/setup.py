from distutils.core import setup
from distutils.extension import Extension
from Cython.Distutils import build_ext
import sys

setup(
  name='Monte Carlo simulation',
  ext_modules=[Extension('roll_dice_cy', ['roll_dice.pyx'],)],
  cmdclass={'build_ext': build_ext},
)
