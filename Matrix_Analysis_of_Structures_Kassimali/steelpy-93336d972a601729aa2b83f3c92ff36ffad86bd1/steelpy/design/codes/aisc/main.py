# 
# Copyright (c) 2019-2021 steelpy
#
#
# Python stdlib imports
#import math
#import datetime
#from dataclasses import dataclass
#from typing import NamedTuple, List, Tuple

#
# package imports
#
from steelpy.codes.aisc.aisc360 import AISC_360_16
from steelpy.codes.aisc.aisc335 import AISC_335_89
#from iLift.codes.AISC360.aisc360 import AISC_360_10
#from iLift.codes.EN1993.EN1993_6 import EN1993_6
from steelpy.sections.stress import Stress, BeamStress, PlateStress
from steelpy.process.load.actions import Actions
#from iLift.codes.process.process import CodeResults


#
class DesignProperties:
    
    def __init__(self, units, item_name:str = "N/A"):
        """
        """
        # ----- Ini -----
        self._units = units
        self.item_name = str(item_name)
        #
        # ----- General Data -----
        #
        # ----- Stiffener Spacing ----- 
        self.stiffned_element = False
        self._a = None
        self.tension_field_action = 'no'
        self.tst = None
        self.Fyst = None        
        #
        # ----- Unbraced Length -----
        self.L = None
        self.Lb = None
        #
        # ----- StabilityFactors ----- 
        self.Kx = 1.0
        self.Ky = 1.0
        self.Kz = 1.0
        #
        # ----- Moment Modifiers -----
        self.Cb = 1.0
        #
        #  ----- element Forces -----
        self.actions = Actions()
        #
        #
        # ----- Load offsets -----
        # self.Xe = 0 * units.m # load offset from support 1
        #self.m = 0 * units.m  # vertical load offset from centre line
        #self.l_eff = 0 * units.m # l effective
        #
        # ----- Headers -----
        self.Header = 1
        #
        self.user_factor_t = 1.0
        self.user_factor_c = 1.0
        self.user_factor_B = 1.0
        self.user_factor_V = 1.0
        #
        self.shear_stress = "maximum"
        #self.shear_stress = "average"
        #
        # ----- Output file ----
        #self.file_out = print_header() # 'AISC_14ed.out'
        #
        #self._aisc = AISC_360_10(self.item_name)
        #
        self.file_out = []
    #
    #-------------------------------------------------
    # General Data
    #
    @property
    def material(self):
        """
        Input:/n
        name/number  : material name or fe number
        Fy :
        Emodulus : Elastic modulus
        poisson  :
        density  : material density
        alpha    : thermal
        """     
        #
        return self._material
    @material.setter
    def material(self, material):
        """
        Input:/n
        name/number  : material name or fe number
        Fy :
        Emodulus : Elastic modulus
        poisson  :
        density  : material density
        alpha    : thermal
        """     
        #
        self._material = material
        # self.file_out.extend(material._print_material())
        #for row in self.file_out:
        #    print(row.rstrip())
    #
    @property
    def section(self):
        """
        """
        return self._section
    @section.setter
    def section(self, section):
        """
        """
        #
        if "bar" in section.type:
            self.stress = PlateStress(0 * self._units.MPa, 0 * self._units.MPa, 0 * self._units.MPa, 
                                      0 * self._units.MPa, 0 * self._units.MPa, 0* self._units.MPa)
        else:
            self.stress = BeamStress(0 * self._units.MPa, 0 * self._units.MPa, 0 * self._units.MPa, 
                                     0 * self._units.MPa, 0 * self._units.MPa, 0* self._units.MPa)
        
        self._section = section
    #
    #-------------------------------------------------
    # Analysis data
    @property
    def member_length(self):
        """
        L = Beam Efective (unbeaced) Length
        """
        return self.L
    @member_length.setter
    def member_length(self, L: float):
        """
        L = Beam Efective (unbeaced) Length
        """
        self.L = L
    #
    @property
    def laterally_unbraced_length(self):
        """
        Lb  : Lateral braced Length
        
        Beam Length between points that are
        either braced against lateral displacement
        of compression flange or braced against
        twist of the cross section
        """
        return self.Lb
    @laterally_unbraced_length.setter
    def laterally_unbraced_length(self, Lb: float):
        """
        Lb  : Lateral braced Length
        
        Beam Length between points that are
        either braced against lateral displacement
        of compression flange or braced against
        twist of the cross section
        """
        self.Lb = Lb
    #
    def stability_factors(self, Kx:float, Ky:float = 1.0, Kz:float = 1.0):
        """
        Kx : Effective length factor for torsional buckling
        Ky : Effective length factor for flexural buckling about y-axis
        Kz : Effective length factor for flexural buckling about z-axis 
        """
        # Torsion
        self.Kx = float(Kx)
        self.Ky = float(Ky)
        self.Kz = float(Kz)
    #
    @property
    def moment_modifier(self):
        """
        Cb : Lateral-torsional buckling modification factor for nonuniform
             moment diagrams
        """
        return self.Cb
    @moment_modifier.setter
    def moment_modifier(self, Cb:float):
        """
        Cb : Lateral-torsional buckling modification factor for nonuniform
             moment diagrams
        """
        self.Cb = Cb 
    #
    def transverse_stiffener(self, a:float, tst:float = 0, Fyst:float = 0, tfa:bool = False):
        """
        a    : Clear distance between transverse stiffeners
        tst  : Thickness of web stiffener
        Fyst : Specified minimum yield stress of the stiffener material
        tfa  : Tension field action (off)
        """
        #
        self._a = a
        self.tst = tst
        self.Fyst = Fyst   
        self.tension_field_action = tfa
        self.stiffned_element = True
    #
    @property
    def a(self):
        """
        a = distance_between_transverse_stiffeners
        """
        return self._a
    #
    @a.setter
    def a(self, value):
        """
        a = distance_between_transverse_stiffeners
        """
        # FIXME : wrong simbol a
        self._a = value
    #-------------------------------------------------
    # Acting Forces
    #
    #@property
    #def member_forces(self):
    #    """
    #    """
        #if not self.actions:
        #    return Actions(0, 0, 0, 0, 0, 0)
        #return self.actions
    #
    #def member_actions(self, **kwargs):
    #    """
    #    """
    #    assign_force_item(self, kwargs)
    #    print('??????')
    # 
    #def member_stress(self, **kwargs):
    #    """
    #    """
    #    assign_stress_item(self, kwargs)
    #    #print('stress')
    #  
    #
    #-------------------------------------------------
    # User Defined
    #
    def user_defined_factors(self, FactorT:float =1.0, FactorC:float =1.0, 
                             FactorB:float =1.0, FactorV:float =1.0):
        """
        """
        self.user_factor_t = abs(float(FactorT))
        self.user_factor_c = abs(float(FactorC))
        self.user_factor_B = abs(float(FactorB))
        self.user_factor_V = abs(float(FactorV))
        #
        self.design_method = 'user_defined'
    #
    #-------------------------------------------------
    # Code check
    #
    def AISC_WSD(self):
        """
        """
        #print('aisc asd')
        code = AISC_360_10(self)
        code.get_AISC_results('asd')
        return code
    #
    def AISC_bending_WSD(self):
        """
        """
        code = AISC_360_10(self)
        code.AISC_bending_check('asd')
        return code
    #
    def AISC_shear_WSD(self):
        """
        """
        code = AISC_360_10(self)
        code.AISC_shear_check('asd')
        return code
    #
    def AISC_LRFD(self):
        """
        """
        #print('aisc lrfd')
        code = AISC_360_10(self)
        code.get_AISC_results('lrfd')
        return code
    #
    #
    def EN1993_6(self, trolley, section):
        """
        """
        code = EN1993_6(self)
        code.trolley = trolley
        code.I_section = section
        code.get_EN1993_6_results()
        return code
#