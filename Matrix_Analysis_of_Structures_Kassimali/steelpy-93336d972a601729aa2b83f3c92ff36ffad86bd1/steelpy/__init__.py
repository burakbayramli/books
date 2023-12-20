# Copyright (c) 2009-2023 steelpy
#
# TODO: host until sure no longer needed
#
from steelpy.design.codes.main import CodeCheck
from steelpy.utils.roarks.main import RoarksFormulas
from steelpy.design.main import ClampDesign as Clamp
from steelpy.metocean.main import Metocean
from steelpy.trave.main import Trave2D, Trave3D, Beam
#from steelpy.beam.main import Beam #, SimpleBeam
from steelpy.f2uModel.main import f2uModel
#
from steelpy.material.main import Materials
from steelpy.sections.main import Sections
from steelpy.utils.units.main import Units

from steelpy.utils.spreadsheet.main import Spreadsheet
#from steelpy.vibration.main import Vibration

# constants
__major__ = 0.  # for major interface/format changes
__minor__ = 5  # for minor interface/format changes
__release__ = 1  # for tweaks, bug-fixes, or development

__version__ = '%d.%d.%d' % (__major__, __minor__, __release__)

__author__ = 'Salvador Vargas-Ortega'
__license__ = 'MIT'
__author_email__ = 'svortega@gmail.com'
#__maintainer_email__ = 'steelpy_users@googlegroups.com'
#__url__ = 'http://steelpy.readthedocs.org'
__downloadUrl__ = "https://github.com/svortega/steelpy"

# header
print('{:}'.format(52*'-'))
print('{:}   steelpy'.format(18*' '))
print('{:}   Version {:}'
      .format(14*' ',__version__))
print('{:}'.format(52*'-'))