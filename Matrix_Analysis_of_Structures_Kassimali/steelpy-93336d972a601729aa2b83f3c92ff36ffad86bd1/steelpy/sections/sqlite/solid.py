# 
# Copyright (c) 2019-2023 steelpy
#

# Python stdlib imports
from __future__ import annotations
#from array import array
from dataclasses import dataclass
#from typing import NamedTuple, List, Union
import re

# package imports
from ..inmemory.solid import RectangleBasic, CircleBasic, Trapeziod
from ..process.operations import get_sect_properties
from .operations import SectionSQLite
#from ..inmemory.operations import ShapeBasic

# ----------------------------------------
#      Basic Solid Shapes
# ----------------------------------------
#
#
@dataclass
class SolidSectionSQL(SectionSQLite):
    
    def __init__(self, db_file:str):
        """ """
        #super().__init__()
        #self._d: array = array('f', [])
        #self._wb: array = array('f', [])
        #self._wt: array = array('f', [])
        #
        self._type = []
        self.db_file = db_file
        # push data to sqlite table
        #SectionSQLite.__init__(self, db_file=self.db_file,
        #                       section=section)
        super().__init__(db_file=self.db_file)        
    #
    #
    def __setitem__(self, shape_name: int|str, parameters: list) -> None:
        """
        parameters = [node1, node2, material, section, roll_angle]
        """
        try:
            self._labels.index(shape_name)
            raise Exception('element {:} already exist'.format(shape_name))
        except ValueError:
            shape_type = parameters.pop(0)
            self._labels.append(shape_name)
            #self._title.append('NULL')
            #mnumber = next(self.get_number())
            #self._number.append(mnumber)
            #
            d = parameters[0]
            FAvy = 1
            FAvz = 1
            shear_stress:str = 'maximum'
            build:str = 'welded'            
            compactness = None
            #
            if re.match(r"\b((solid|bar(\_)?)?circular|round)\b", shape_type, re.IGNORECASE):
                self._type.append('round')
                #self._wb.append(0)
                #self._wt.append(0)
                #
                section = (shape_name, 
                           None,            # title
                           "Circular Bar",  # shape type
                           d, None,         # diameter, wall_thickess
                           None, None, # height, web_thickness
                           None, None, # top_flange_width, top_flange_thickness
                           None, None, # bottom_flange_width, bottom_flange_thickness
                           None,       # root radius
                           FAvy, FAvz,
                           shear_stress, build,
                           compactness,)               

            elif re.match(r"\b((solid|bar(\_)?)?rectangle)\b", shape_type, re.IGNORECASE):
                self._type.append('rectangle')
                w = parameters[1]
                #self._wb.append(parameters[1])
                #self._wt.append(parameters[1])
                #
                section = (shape_name,
                           None,  # title
                           "Rectangle",   # shape type
                           None, None,    # diameter, wall_thickess
                           d, None,       # height, web_thickness
                           w, None,       # top_flange_width, top_flange_thickness
                           w, None,       # bottom_flange_width, bottom_flange_thickness
                           None,       # root radius
                           FAvy, FAvz,
                           shear_stress, build,
                           compactness,)                 

            elif re.match(r"\b((solid|bar(\_)?)?trapeziod)\b", shape_type, re.IGNORECASE):
                self._type.append('trapeziod')
                wb = parameters[1]
                wt = parameters[2]
                #self._wb.append(parameters[1])
                #self._wt.append(parameters[2])
                section = (shape_name,
                           None,  # title
                           "trapeziod",   # shape type
                           None, None,    # diameter, wall_thickess
                           d, None,       # height, web_thickness
                           wb, None,       # top_flange_width, top_flange_thickness
                           wt, None,       # bottom_flange_width, bottom_flange_thickness
                           None,       # root radius
                           FAvy, FAvz,
                           shear_stress, build,
                           compactness,)                 

            else:
                raise Exception(f" section type {shape_type} not recognized")
            #
            number = self.push_section(section)
            self._number.append(number)
    #
    def __getitem__(self, shape_name: str | int):
        """
        """
        try:
            index = self._labels.index(shape_name)
            #number = self._number[index]
        except ValueError:
            raise Exception(f" section name {shape_name} not found")


        shape_type = self._type[index]
        row = self.get_section(shape_name)
        
        if re.match(r"\b((solid|bar(\_)?)?circular|round)\b", shape_type, re.IGNORECASE):
            d = row[3]
            return CircleBasic(name=shape_name, d=d, type=shape_type)

        elif re.match(r"\b((solid|bar(\_)?)?rectangle)\b", shape_type, re.IGNORECASE):
            d = row[3]
            wb = row[7]
            return RectangleBasic(name=shape_name, depth=d, width=wb,
                                  type=shape_type)

        elif re.match(r"\b((solid|bar(\_)?)?trapeziod)\b", shape_type, re.IGNORECASE):
            d = row[5]
            wb = row[7]
            wt = row[9]            
            c = abs(wt - wb) / 2.0
            return Trapeziod(name=shape_name, depth=d, width=wb,
                             a=wt, c=c, type=shape_type)

        else:
            raise Exception(f" section type {shape_type} not recognized")
    #    
#
#
@dataclass
class RectangleSQLite(SectionSQLite):
    __slots__ = ['_properties', 'name', 'number', 'db_file']

    def __init__(self, name:Union[str, int],
                 d: Union[float, Units], w: Union[float, Units],
                 db_file:str,
                 build:str = 'welded',
                 shear_stress:str = 'maximum',
                 FAvy:float = 1.0, FAvz:float = 1.0):
        """
        Parameters
        ----------
        d : Height
        w : Width
        """
        #RectangleBasic.__init__(self)
        self.name = name
        self._properties = None
        self.db_file = db_file
        compactness = None
        section = (self.name,
                   None,  # title
                   "Rectangle",   # shape type
                   None, None,    # diameter, wall_thickess
                   d, None,       # height, web_thickness
                   w, None,       # top_flange_width, top_flange_thickness
                   w, None,       # bottom_flange_width, bottom_flange_thickness
                   FAvy, FAvz,
                   shear_stress, build,
                   compactness,)
        # push data to sqlite table
        SectionSQLite.__init__(self, db_file=self.db_file, section=section)
    #
    #
    @property
    def d(self):
        return self.get_item(item="height")

    @d.setter
    def d(self, value:Union[Units,float]):
        """ """
        value = get_sect_properties([value])
        self.update_item(item='height', value=value[0])
        self.push_property()
    #
    #
    @property
    def w(self):
        return self.get_item(item="top_flange_width")

    @w.setter
    def w(self, value:Union[Units,float]):
        """ """
        value = get_sect_properties([value])
        self.update_item(item='top_flange_width', value=value[0])
        self.push_property()
    #
    #def __setattr__(self, shape_type:str, value:Union[Units,float]):
    #    """ """
    #    value = get_sect_properties([value])
    #    if re.match (r"\b(d(epth)?|h(eight)?)\b", shape_type, re.IGNORECASE):
    #        self.update_item(item='height', value=value[0])
    #        self.push_property()
    #    elif re.match (r"\b(w(idth)?)\b", shape_type, re.IGNORECASE):
    #        self.update_item(item='top_flange_width', value=value[0])
    #        self.push_property()
    #    else:
    #        # in python3+ you can omit the arguments to super:
    #        super().__setattr__(shape_type, value[0])
    #
    #def __getattr__(self, shape_type:str):
    #    """ """
    #    if re.match (r"\b(d(epth)?|h(eight)?)\b", shape_type, re.IGNORECASE):
    #        return self.get_item(item="height")
    #    elif re.match (r"\b(w(idth)?)\b", shape_type, re.IGNORECASE):
    #        return self.get_item(item="top_flange_width")
    #    else:
    #        raise AttributeError(shape_type)
#
#
@dataclass
class CircleSQLite(SectionSQLite):
    __slots__ = ['_properties', 'name', 'number', 'db_file']
    
    def __init__(self, name:str|int,
                 d:float|None, 
                 db_file:str,
                 build:str = 'welded', 
                 shear_stress:str = 'maximum',
                 FAvy:float = 1.0, FAvz:float = 1.0):
        """
        Parameters
        ----------
        d : diametre
        Shear Stress: MAXIMUM / AVERAGE
        """
        #CircleBasic.__init__(self)
        self.name = name
        self._properties = None
        self.db_file = db_file
        compactness = None
        section = (self.name, 
                   None,            # title
                   "Circular Bar",  # shape type
                   d, None,         # diameter, wall_thickess
                   None, None, # height, web_thickness
                   None, None, # top_flange_width, top_flange_thickness
                   None, None, # bottom_flange_width, bottom_flange_thickness
                   FAvy, FAvz,
                   shear_stress, build,
                   compactness,)        
        # push data to sqlite table
        #SectionSQLite.__init__(self, db_file=self.db_file,
        #                       section=section)
        super().__init__(db_file=self.db_file, section=section)
    #
    #
    @property
    def d(self):
        """
        D: diameter
        """
        return self.get_item(item="diameter")

    @d.setter
    def d(self, diameter: float):
        """
        """
        diameter = get_sect_properties([diameter])
        self.update_item(item='diameter', value=diameter[0])
        self.push_property()
    #    
#
#

#
#
#