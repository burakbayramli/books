# Copyright (c) 2019-2023 steelpy

# Python stdlib imports
#import math
#import datetime
from typing import ClassVar # NamedTuple, List, 

# package imports
from steelpy.utils.units.main import Units
#from steelpy.f2uModel.material.main import Materials
#from steelpy.sections.tubular import Tubular
#from steelpy.f2uModel.sections.main import Sections
#from steelpy.process.load.actions import Actions
#from steelpy.codes.api.design import API_design
#
#
#
class ClampShell:
    """
    """
    __slots__ = ['_units', '_material', 'Dci', 'tc', 'Lc',
                 'theta_C', 'Nbp', 'Dc', '_section', # 'add_on_flag',
                 'annular_gap', 'Hgap', '_stiffener'] 
                 #, #'Lst']    
    
    def __init__(self, material, section):
        """
        Dci : Shell internal diameter
        tc  : Shell thickness
        Lc  : length of clamp
        theta_C : Clamp shell enclosed angle
        Nbp : number of bolts rows
        add_on : additionla brace member
        annular_gap : aanular gap to define shell internal diameter (%)
        Hgap :  pull-up gap
        """
        #self._units = Units()
        #_material = Materials()
        #_material[1] = 'elastic'
        self._material = material
        self._material["clamp_shell"] = 'elastic'
        self._section = section
        #
        #self.Dci = 0 * self.units.mm
        #self.tc = 0 * self.units.mm
        #self.Lc = 0 * self.units.mm
        # 
        self.theta_C = 0 #* self.units.degrees
        self.Nbp:int = 4
        #
        # 2% default
        self.annular_gap = 0.02 
        # pull-up gap
        self.Hgap = 0 #* self.units.mm
        #
        self._stiffener = ClampStiffener(self)
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
        return self._material["clamp_shell"]
    #
    #
    @property
    def stiffener(self):
        """
        """
        return self._stiffener
    #   
#
#
#
#
class ClampStiffener:
    """
    """
    __slots__ = [ 'cls','e', 'Bsp', 'Cf','eBolt',
                 'tbp', 'Bbp', 'h', 'tsp', 'Tsection']
    
    def __init__(self, cls):
        """
        Cf  : Edge distance from centreline of bolts to edge of flange plate
        Bsp : Bolt spacing
        e   : The distance between the end bolt and the end of the clamp
        eBolt : Bolt eccentricity
        --------------------------
        Flange plate
        tbp : flange thickness
        Bbp : Total width of flange plate
        -------------------------
        Plate
        h : Height of stiffener
        """
        #self._units = Units()
        #_material = Materials()
        #_material[1] = 'elastic'
        #self._material = _material[1]
        #
        self.cls = cls
        #
        # plate
        #self.h = 0 * self.units.mm
        #
        # Flange plate
        #self.tsp = 0 * self.units.mm
        #self.Bbp = 0 * self.units.mm
        # flange thickness
        #self.tbp = 0 * self.units.mm
        #
        self.e = 0 #* self.units.mm
        #self.Bsp = 0 * self.units.mm
        #self.Cf = 0 * self.units.mm
        #self.eBolt = 0 * self.units.mm
        #
        #db_file = "beam_f2u.db"
        #mesh_type = 'inmemory'
        #self.Tsection = Sections(mesh_type=mesh_type,
        #                         db_file=db_file)        
        self.cls._section["Tsection"] = "T"
        self.cls._material["Tsection"] = 'elastic'
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
        return self.cls._material["Tsection"]
    
    @property
    def T_section(self):
        """
        """
        return self.cls._section["Tsection"]
#
#
#class Substrate:
    #"""
    #"""
    #__slots__ = ['_units', '_material', '_tubular',]
    #
    #def __init__(self):
    #    """
    #    """
    #    self._units = Units()
    #    #self._material = Material(self._units)
    #    #self._chord = API_design()
    #    #self._chord:ClassVar = TubularMember(self._units, 
    #    #                                     name='chord', number=1)
    #    self._tubular:dict = {} #
    ##
    #def __setitem__(self, member_name:str, case):
    #    """
    #    """
    #    self._tubular[member_name] = API_design()
    #    self._tubular[member_name].member_name = case
    ##
    #def __getitem__(self, member_name:str):
    #    """
    #    """
    #    try:
    #        return self._tubular[member_name]
    #    except KeyError:
    #        raise KeyError('Invalid member name : {:}'
    #                       .format(member_name))    
    #
    #
    #@property
    #def chord(self):
    #    """
    #    """
    #    return self._chord
    ##
    #@property
    #def member(self):
    #    """
    #    """
    #    return self._chord
    ##
    #@property
    #def brace(self):
    #    """
    #    """
    #    _number = len(self._brace) + 1
    #    #self._brace[_number] =  TubularMember(self._units, 
    #    #                                      name='brace', number=_number)
    #    self._brace[_number] = API_design()
    #    return self._brace[_number]
    ##
    
#