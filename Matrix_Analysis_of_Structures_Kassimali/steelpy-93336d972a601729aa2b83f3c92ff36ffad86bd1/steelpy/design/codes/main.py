# Copyright (c) 2019-2020 steelpy

# Python stdlib imports

# package imports
#from steelpy.codes.aisc.aisc360 import AISC_360_16
#from steelpy.codes.aisc.aisc335 import AISC_335_89
#from steelpy.codes.iso.ISO19902 import ISOCodeCheck
from steelpy.design.codes.piping.pipeline import PipelineDesign
#from steelpy.codes.api.wsd_22ed import APIwsd22ed
from steelpy.design.codes.dnv.pannel import CodeCheckPanel
#
#from steelpy.process.units.main import Units
#from steelpy.material.material import Material
#from steelpy.sections.tubular import Tubular

from steelpy.design.codes.api.main import API_design

class CodeCheck:
    """
    """
    def __init__(self):
        """"""
        #self._units = Units()
        pass
    
    #@property
    #def units(self):
    #    """
    #    """
    #    return self._units    
    #
    @property
    def API(self):
        """
        """
        return API_design()
    #
    # @property
    def pipeline(self, code: str = 'PD8010'):
        """ """
        return PipelineDesign(code)
    #
    def DNV_pannel(self):
        """ """
        return CodeCheckPanel()

