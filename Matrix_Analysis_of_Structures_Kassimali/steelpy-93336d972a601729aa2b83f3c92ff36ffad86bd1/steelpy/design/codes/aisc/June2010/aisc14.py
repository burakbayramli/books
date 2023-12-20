# 
# Copyright (c) 2018 steelpy
#
# *******************************************
#                 Bug History
#
# Bug fixed on section (E4-3b) 18/07/10 - SVO
# Bug fixed on section (F4)    21/07/10 - SVO
# Mayor program upgrade to
# allow re-design of members   30/07/10 - SVO
# Include box sections         14/01/11 - SVO
#
#
# *******************************************
#
#
# Python stdlib imports
import math
import sys
import datetime
import re

#
# package imports
#
import steelpy.units.control as units
#import steelpy.material as material
from steelpy.material.material import(assign_material_item)
from steelpy.codes.aisc.aisc2010.AISC36010_2010 import *
#
#
#
def match_line(word_in, key):
    """
    search key word at the begining of the string
    """
    word_out = False
    
    for _key, _item in key.items():
        rgx = re.compile(_item, re.IGNORECASE)
        keys = rgx.match(word_in)
        
        if keys: 
            word_out = _key        
    
    return word_out

#
def find_force_item(word_in):
    """
    """
    _key = {"P": r"\b((p|(axial\s*)?force)(\s*a(xial)?)?)\b",
            "Vx": r"\b((v|shear)(x|i(n)?\s*p(lane)?))\b",
            "Vy": r"\b((v|shear)(y|o(ut)?\s*p(lane)?))\b",
            "Vt": r"\b((v|shear)\s*t(orsion(al)?)?)\b",
            "Mx": r"\b((bending\s*)?m(oment)?(x|i(n)?\s*p(lane)?))\b",
            "My": r"\b((bending\s*)?m(oment)?(y|o(ut)?\s*p(lane)?))\b",
            "Mt": r"\b((bending\s*)?m(oment\s*)?t(orsion(al)?)?)\b"}
    
    _match = match_line(word_in, _key)
    
    if not _match:
        print('  **  erorr material item {:} not recognized'.format(word_in))
        print('      process terminated')
        sys.exit()

    return _match
#
def assign_force_item(_force, mat_items):
    """
    Assign material's input data to a dictionary by name/number
    """
    
    for key, value in mat_items.items():
        
        _item = find_force_item(key)
        #_mat[_item] = value
        if _item == 'P':
            _force.P = float(value)
        
        elif _item == 'Vx':
            _force.Vx = float(value)
        
        elif _item == 'Vy':
            _force.Vy = float(value)
        
        elif _item == 'Vt':
            _force.Vt = float(value)
        
        elif _item == 'Mx':
            _force.Mx = float(value)
        
        elif _item == 'My':
            _force.My = float(value)
        
        elif _item == 'Mt':
            _force.Mt = float(value)

        #
        else:
            print('error Force item : {:} not recognized'
                  .format(key))
            sys.exit()
    # 
    #print('ok')
#
def print_header():
    """
    """
    today = datetime.date.today()
    output = [] # open(self.FileOut,'w')
    #
    #output.append(" "+"\n")
    output.append("***************************************************************************************\n")
    output.append("*                                  CODE CHECK TOOL                                    *\n")
    output.append("*                                      AISC 14                                        *\n")
    output.append("*                     Specification for Structural Steel Buildings                    *\n")
    output.append("*                                   BETA Version                             14/01/10 *\n")            
    output.append("***************************************************************************************\n")
    output.append("DATE: {:8s} {:>59}".format(str(today), ""))
    # 
    #output=open(self.FileOut,'a+')
    return output
#
#-------------------------------------------------
#
class AISC_360_10:
    #
    def __init__(self, beamID: str = "N/A"):
        """
        """
        # ----- Ini -----
        self.beam_name = str(beamID)
        #
        # ----- General Data -----
        #
        self.stiffned_element = False
        self.a = None
        #
        # ----- Unbraced Length -----
        self.L = None
        self.Lb = None
        #
        #  ----- Material -----
        self.units_in = ["", "", "second", "", "", ""]
        #
        # ----- Stiffener Spacing ----- 
        self.tension_field_action = 'no'
        #
        # ----- StabilityFactors ----- 
        self.Kx = 1.0
        self.Ky = 1.0
        self.Kz = 1.0
        #
        # ----- Moment Modifiers -----
        self.Cb = 1.0
        #
        #  ----- Forces -----
        #  
        # ----- Headers -----
        self.Header = 1
        #
        #self.user_factor_t = 1.0
        #self.user_factor_c = 1.0
        #self.user_factor_B = 1.0
        #self.user_factor_V = 1.0
        #
        self.shear_stress = "maximum"
        self.shear_stress = "average"
        #
        # ----- Output file ----
        self.file_out = print_header() # 'AISC_14ed.out'
        #for row in self.file_out:
        #    print(row.rstrip())        
        #
    #
    #-------------------------------------------------
    # General Data
    #
    def units_input(self, **kwargs):
        """
        Input:\n
        length : [mandatory]\n
        mass   : [mandatory]\n
        force  : [mandatory]\n
        time   : sec [default]\n
        temperature : \n
        gravity     : [default : 9.81ms^2]\n

        ------
        units [length, mass, time, temperature, force, pressure/stress]/n
        """

        for key, value in kwargs.items(): 
            _unit = units.find_unit_case(key)
            self.units_in = units.units_module(_unit, value, 
                                               self.units_in)
        
        if self.units_in[0] and self.units_in[1] and self.units_in[4]:
            pass
        else:
            raise IOError('error length, mass and force units must be provided')
        
        #  pressure
        if not self.units_in[5]:
            self.units_in[5] = str(self.units_in[4]) + '/' + str(self.units_in[0]) + '^2'    
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
        self.file_out.extend(material._print_material())
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
        #try:
        if self.units_in[0]:
                section.units_output(length = self.units_in[0],
                                     mass = self.units_in[1])
                section._get_properties()
        else:
            raise IOError('  **  input units not provided')
        
        (self.compacness_compression, 
         self.compacness_flexure, comp_out) = chapter_B(self, section, self._material)
        #
        self.file_out.extend(section._print_section_properties())
        self.file_out.extend(comp_out)
        for row in self.file_out:
            print(row.rstrip())
        
        #except AttributeError:
        #    raise IOError('  * section must be provided')
        
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
    def lateral_braced_length(self):
        """
        Lb  : Lateral braced Length
        
        Beam Length between points that are
        either braced against lateral displacement
        of compression flange or braced against
        twist of the cross section
        """
        return self.Lb
    @lateral_braced_length.setter
    def lateral_braced_length(self, Lb: float):
        """
        Lb  : Lateral braced Length
        
        Beam Length between points that are
        either braced against lateral displacement
        of compression flange or braced against
        twist of the cross section
        """
        self.Lb = float(Lb)    
    #
    def stability_factors(self, Kx :float, Ky: float = 1.0, Kz: float = 1.0):
        """
        Kx : Effective length factor for flexural buckling about x-axis
        Ky : Effective length factor for flexural buckling about y-axis
        Kz : Effective length factor for torsional buckling
        """
        # Main Axis
        self.Kx = float(Kx)
        # Weak Axis
        self.Ky = float(Ky)
        # Torsion
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
    def moment_modifier(self, Cb: float):
        """
        Cb : Lateral-torsional buckling modification factor for nonuniform
             moment diagrams
        """
        self.Cb = Cb 
    #
    def stiffener(self, a: float, tst: float = 0, Fyst: float = 0, tfa: float = None):
        """
        a    : Clear distance between transverse stiffeners
        tst  : Thickness of web stiffener
        Fyst : Specified minimum yield stress of the stiffener material
        tfa  : Tension field action (off)
        """
        #
        #Clear distance between transverse stiffeners,
        self.a = float(a)
        # Thickness of web stiffener
        self.tst = float(tst)
        # Specified minimum yield stress of the stiffener material
        self.Fyst = float(Fyst)      
        #
        self.tension_field_action = tfa
        #
        self.stiffned_element = True
    #    
    #-------------------------------------------------
    # Acting Forces
    #
    def member_forces(self, **kwargs):
        """
        """
        assign_force_item(self, kwargs)
    # 
    #-------------------------------------------------
    # User Defined
    #
    def user_defined_factors(self, FactorT: float =1.0, FactorC: float =1.0, 
                             FactorB: float =1.0, FactorV: float =1.0):
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
    # Print Results
    #
    def units_output(self, **kwargs):
        """
        Input:\n
        length : [mandatory]\n
        mass   : [mandatory]\n
        force  : [mandatory]\n
        time   : sec [default]\n
        temperature : \n
        gravity     : [default : 9.81ms^2]\n

        ------
        units [length, mass, time, temperature, force, pressure/stress]/n
        """
        _units_in = copy.deepcopy(self.units_in)
        for key, value in kwargs.items(): 
            _unit = units.find_unit_case(key)
            self.units_out = units.units_module(_unit, value, _units_in)
        
        if self.units_out[0] and self.units_out[1] and self.units_out[4]:
            pass
        
        else:
            print('error length, mass and force units must be provided')
            print('      program aborted')
            sys.exit()
        
        #  pressure
        if not self.units_out[5]:
            self.units_out[5] = str(self.units_out[4]) + '/' + str(self.units_out[0]) + '^2'
        #print('ok')  
    #    
    def get_results(self, design_method: str ='asd'):
        """
        """
        #
        print("{:_<87}\n".format(""))
        #
        try:
            self.L
        except AttributeError:
            raise IOError("   ** member length must be provided")
        
        self.design_method = design_method
        print('Design Method {:}'.format(self.design_method.upper()))
        # Units
        try:
            _units_input = self.units_in
            _units_output = self.units_out
        except AttributeError:
            _units_input = self.units_in
            _units_output = self.units_in
        #
        factors, gravity = units.get_factors_and_gravity(_units_input, 
                                                         _units_output)
        #
        # get section streses
        self._section.stress(self.P, self.Vy, self.Vx, self.Mt, self.Mx, self.My)
        #
        # Axial check
        if self.P < 0.0:
            self.FAxial = 'compression'
            Chapter_E(self, self._section, self._material)
        else:
            self.FAxial = 'tension'
            Chapter_D(self, self._section, self._material)
        # 
        # Check Lateral Unbraced Length
        if not self.Lb:
            self.Lb = self.L
        #
        # Bending check
        ChapterF(self, self._section, self._material)
        #
        # Shear check 
        ChapterG(self, self._section, self._material)
        #
        # Shear check
        ChapterH(self, self._section, self._material)
        #
        #print('ok')
    #
    def print_results(self):
        """
        """
        self.Header = 0
        #
        printHeader()
        #
        # Print Results
        self._section.print_file(self.file_out)
        #print_section_compacness(self.beam_name)
        #AISC.PrintProperty(self)
        #Shape.PrintProperty(self)
        #Shape.PrintPropertyShort(self)
        #PrintSummary(self)
        
        self.Header = self.Header + 1
        #
        #  
    #    
#   
#
#
#