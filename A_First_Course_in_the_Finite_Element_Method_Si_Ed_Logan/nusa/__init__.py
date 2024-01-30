"""
NuSA (Numerical Structural Analysis in Python)
Author: Pedro Jorge De Los Santos
E-mail: delossantosmfq@gmail.com
Blog: jorgedelossantos.github.io // numython.github.io
License: MIT License
"""
from .version import __version__

__author__ = "P.J. De Los Santos"
__email__ = "delossantosmfq@gmail.com"

from .core import *
from .element import *
from .model import *
from ._experimental import *
from .mesh import *
from .io import *

import matplotlib as mpl
mpl.rc("figure", facecolor="#FAFAFA", titleweight="bold")
mpl.rc("axes", facecolor="#FFFFFF")
mpl.rc("font", size=9)
