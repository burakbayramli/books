# Copyright (c) 2019-2020 steelpy

# Python stdlib imports
#import math
#import datetime
#from typing import ClassVar # NamedTuple, List, 

# package imports
#from steelpy.process.units.main import Units
#from steelpy.f2uModel.material.main import Materials

#
class Neoprene:
    """
    """
    __slots__ = ['_material', #'_units', 
                 'Ln', 'tn', 'Mu', 'IRHD', 'Kn', 'E_alpha', 
                 'neoprene_type', 'name', 'gamma', 'strain_limit']
    
    def __init__(self, material):
        """
        Ln : Neoprene Length
        tn : Neoprene Thickness
        Mu : Coefficient of friction between steel and Neoprene
        IRHD : Rubber Hardness  (degrees)
        -----------------------
        Eo : Elastic Moduli (N/mm2)
        Gn : Shear Modulus (N/mm2)
        Kn : Numerical Factor
        Ealpha : Bulk Modulus (N/mm2)
        neoprene_type : Plain/Ribbed
        gamma : factor of safety 
        """
        #self._units = Units()
        #_material = Materials()
        #_material[1] = 'plastic'
        self._material = material
        self._material['neoprene'] = 'linear'
        #
        #self.Ln = 0 * self._units.mm
        #self.tn = 0 * self._units.mm
        self.Mu = 0.10
        self.IRHD = 65.0 #* self._units.Mpa
        #self.Kn = 0
        #self.E_alpha = 0 * self._units.MPa
        self.neoprene_type = "PLAIN"
        self.name = 'neoprene'
        self.gamma = 1.0
        # % of compressive strain in neoprene to avoid non-linear behaviour
        self.strain_limit = 10
    #
    #@property
    #def units(self):
    #    """
    #    """
    #    return self._units
    #
    @property
    def material(self):
        """
        """
        return self._material['neoprene']
    # 
#