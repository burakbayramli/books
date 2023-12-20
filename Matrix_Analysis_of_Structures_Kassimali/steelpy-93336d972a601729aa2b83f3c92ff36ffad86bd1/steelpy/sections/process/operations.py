# 
# Copyright (c) 2018-2023 steelpy


# Python stdlib imports
from __future__ import annotations
#from array import array
#from dataclasses import dataclass
#from collections.abc import Mapping
from typing import NamedTuple
#import re
#

# package imports
#

#
#
#
class ShapeProperty(NamedTuple):
    """ """
    area:float
    Zc:float
    Yc:float
    Iy:float
    Zey:float
    Zpy:float
    ry:float
    Iz:float
    Zez:float
    Zpz:float
    rz:float
    J:float
    Cw:float
    #
    def __str__(self) -> str:
        """ """
        #print('--->')
        output = "{:1.4e} {:1.4e} {:1.4e} {:1.4e} {:1.4e} {:1.4e}\n"\
            .format(self.area, self.Iy, self.Iz, self.Zc, self.ry, 1)
        output += "{:25s} {:1.4e} {:1.4e} {:1.4e} {:1.4e} {:1.4e}\n"\
            .format("", self.Zey, self.Zez, 1, self.rz, self.Cw)
        output += "{:25s} {:1.4e} {:1.4e} {:1.4e} {:1.4e}\n"\
            .format("", self.Zpy, self.Zpz, 1, self.area*0)
        return output
#
#
class SectionProperty(NamedTuple):
    """
    area: Section area
    Zc  : Elastic neutral centre
    Yc  : Elastic neutral centre
    
    Iy  : Second moment of area about mayor axis
    Zy : Elastic modulus about mayor axis
    Sy : Plastic modulus about mayor axis
    Avy : Shear area mayor axis
    ry  : Radius of gyration about mayor Axis
    
    Iz  : Second moment of area about minor axis
    Zz : Elastic modulus about minor axis
    Sz : Plastic modulus about minor axis
    Avz : Shear area minor axis
    rz  : Radius of gyration about minor Axis
    
    SCz  : Shear centre about z axis
    SCy  : Shear centre about y axis
    
    Cw  : Warping constant
    """
    area: float
    Zc  : float
    Yc  : float
    Iy  : float
    Zy : float
    Sy : float
    SFy : float
    ry  : float
    Iz  : float
    Zz : float
    Sz : float
    SFz : float
    rz  : float
    SCy  : float
    SCz  : float
    Cw  : float
    #
    def __str__(self) -> str:
        """ """
        #print('--->')
        output = "{:<14s} {:1.4e} {:1.4e} {:1.4e} {:1.4e} {:1.4e} {:1.4e}\n"\
            .format(self.area, self.Iy, self.Iz, self.Zc, self.ry, 1)
        output += "{:<14s} {:1.4e} {:1.4e} {:1.4e} {:1.4e} {:1.4e}\n"\
            .format(" "*14, self.Zy, self.Zz, self.SCy, self.rz, self.Cw)
        output = "{:<14s} {:1.4e} {:1.4e} {:1.4e} {:1.4e}\n"\
            .format(self.area*0, self.Sy, self.Sz, self.SCz)        
        return output
#
#
#
# ---------------------------------
#
def get_sect_properties(properties:list[float], steps:int=10):
    """ """
    #sect_prop = [None for _ in range(steps)]
    sect_prop = []
    for x, item in enumerate(properties):
        try:
            #sect_prop[x] = item.value
            sect_prop.append(item.value)
        except AttributeError:
            #sect_prop[x] = item
            sect_prop.append(item)
            raise IOError('units required')
    return sect_prop
#
#
def get_sect_prop_df(df):
    """ """
    for cname in df:
        try:
            df[cname] = df[cname].apply(lambda x: x.convert('metre').value)
        except AttributeError:
            pass
    #print('---')
    return df   
#
#
#
def get_Isection(parameters: list):
    """ [d, tw, bf, tf, bfb, tfb, r, title] """
    # basic information
    section = parameters[:4] # d, tw, bf, tf
    # check if str title at the end
    if isinstance(parameters[-1], str):
        title = parameters.pop()
    else:
        title = 'NULL'
    #
    # check if root radius
    if len(parameters) == 4:
        section.append(parameters[2]) # bfb
        section.append(parameters[3]) # tfb
        section.append(0)             # root radius
        
    elif len(parameters) == 5:
        r = parameters.pop()
        section.append(parameters[2]) # bfb
        section.append(parameters[3]) # tfb
        section.append(r)
        
    elif len(parameters) == 6:
        section.append(0)
        
    else:
        if len(parameters) != 7:
            raise IOError('Error Ibeam input data ')
    #
    section.append(title)
    #  
    return section

