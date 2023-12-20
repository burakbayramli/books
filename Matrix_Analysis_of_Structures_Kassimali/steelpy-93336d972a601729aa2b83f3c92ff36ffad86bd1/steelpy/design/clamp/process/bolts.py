# Copyright (c) 2019-2020 steelpy

# Python stdlib imports
#import math
#import datetime
from typing import ClassVar # NamedTuple, List, 

# package imports
from steelpy.utils.units.main import Units
from steelpy.material.main import Materials

#-------------------------------------------------
#
#-------------------------------------------------
#
class BoltData:
    """
    """
    __slots__ = ('ID', 'type', 'grade', 'd', '_material',
                 'tensile_area', 'proof_load', 'proof_load_80percent',
                 'spherical_washer_A', 'spherical_washer_B', 
                 'spherical_washer_C', 'spherical_washer_D',
                 'tensioner_A', 'units')
    #
    def __init__(self, Name = 'M20', BoltType = 'STUD', BoltGrade = 'ML7',
                 Diameter = 20, Fyb = 725, Fub = 860, E = 205000,
                 TensileArea = 245.0, ProofLoad  = 178.0, ProofLoad80 = 142.0,
                 SWA = 84, SWB = 22, SWC = 94, SWD = 35, Ten_A = 66):
        """
        """
        self.units = Units()
        # Stud Bolt ID
        self.ID = Name.upper()
        self.type = BoltType.upper()
        self.grade = BoltGrade
        # Bolt Section
        self.d = Diameter * self.units.mm
        # Stud-Bolt Material Properties
        #_material = Materials()
        #_material[1] = 'plastic'
        #self._material = _material[1]
        #self._material.Fy = Fyb * self.units.newton / self.units.mm**2
        #self._material.Fu = Fub * self.units.newton / self.units.mm**2
        #self._material.E = E * self.units.newton / self.units.mm**2
        # Typical Stud-Bolt Preloads
        self.tensile_area = TensileArea * self.units.mm**2
        self.proof_load = ProofLoad * self.units.newton
        self.proof_load_80percent = ProofLoad80 * self.units.newton
        # Spherical Washers Data
        self.spherical_washer_A = SWA * self.units.mm
        self.spherical_washer_B = SWB * self.units.mm
        self.spherical_washer_C = SWC * self.units.mm
        self.spherical_washer_D = SWD * self.units.mm
        # Tensioner Data
        self.tensioner_A = Ten_A  * self.units.mm
    @property
    def material(self):
        """
        """
        return self._material    
#
#
class Bolts:
    """
    """
    __slots__ = ['_units', '_material', '_bolts', 'name',
                 'total_Pmax', 'Lsb', 'fp', 'db', 'bolt_data']
    
    def __init__(self):
        """
        total_Pmax : Inital and long term losses factor
        fp : Pretension Factor
        Lsb : Stressed length of the studbolts
        """
        self._units = Units()
        #_material = Materials()
        #_material[1] = 'plastic'
        #self._material = _material[1]
        #
        # Bolt Data
        self._bolts:dict = {}
        #
        self._bolts['M20'] = BoltData('M20', 'STUD', 'ML7', 20, 725, 860, 205000, 
                                      245, 178, 142, 84, 22, 94, 35, 65)
        self._bolts['M24'] = BoltData('M24', 'STUD', 'ML7', 24, 725, 860, 205000, 
                                      353, 256, 205, 84, 27, 94, 39, 70)
        self._bolts['M30'] = BoltData('M30', 'STUD', 'ML7', 30, 725, 860, 205000, 
                                      561, 407, 325, 100, 33, 110, 45, 85)
        self._bolts['M36'] = BoltData('M36', 'STUD', 'ML7', 36, 725, 860, 205000, 
                                      817, 592, 474, 104, 39, 114, 51, 100)
        self._bolts['M42'] = BoltData('M42', 'STUD', 'ML7', 42, 725, 860, 205000, 
                                      1120, 812, 650, 110, 45, 120, 57, 115)
        # Inital and long term losses factor
        self.total_Pmax = 0.15
        #self.Lsb = 0 * self.units.mm
        # Pretension Factor
        self.fp = 0.70
        #self.db = 0 * self.units.mm
        self.bolt_data = False
    #
    @property
    def units(self):
        """
        """
        return self._units
    
    @property
    def material(self):
        """
        """
        return self._material
    #
    @property
    def bolts(self):
        """
        """
        return self._bolts[self.name]
#