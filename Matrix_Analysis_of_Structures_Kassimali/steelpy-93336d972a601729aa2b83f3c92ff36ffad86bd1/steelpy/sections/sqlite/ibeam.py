# 
# Copyright (c) 2019-2023 steelpy
#

# Python stdlib imports
from __future__ import annotations
from collections import namedtuple
#from typing import Union, NamedTuple
#

# package imports
from ..inmemory.ibeam import IbeamBasic
from .operations import SectionSQLite
from steelpy.sections.process.operations import get_sect_properties
#
#

class IbeamSQLite(SectionSQLite):
    __slots__ = ['_labels', '_number', '_title', '_type', '_default', 
                 'db_file']

    def __init__(self, db_file:str):
        """ 
        Parameters
        ----------
        d   : Section Height   
        tw  : Web thickness   
        bf  : flange base (top)
        tf  : flange thickness (top)
        bfb : Bottom flange base   
        tfb : Bottom flange thickness
        r   : root radius
        """
        self.db_file = db_file
        super().__init__(db_file=self.db_file)
    #
    #
    #
    def __setitem__(self, shape_name: int|str, parameters: list) -> None:
        """
        parameters = []
        """
        try:
            self._labels.index(shape_name)
            raise Exception('element {:} already exist'.format(shape_name))
        except ValueError:
            self._labels.append(shape_name)
            #
            d = parameters[0]
            tw = parameters[1]
            bf = parameters[2]
            tf = parameters[3]
            bfb = parameters[4]
            tfb = parameters[5]
            r = parameters[6]
            title = parameters[7]
            #
            FAvy = 1
            FAvz = 1
            shear_stress:str = 'maximum'
            build:str = 'welded'
            compactness = None
            section = (shape_name, title, 
                       "I section",   # shape type
                       None, None,    # diameter, wall_thickess
                       d, tw,         # height, web_thickness
                       bf, tf,        # top_flange_width, top_flange_thickness
                       bfb, tfb,      # bottom_flange_width, bottom_flange_thickness
                       r,             # fillet_radius
                       FAvy, FAvz,
                       shear_stress, build,
                       compactness,)
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
        #
        row = self.get_section(shape_name)
        #
        return IbeamBasic(name=row[0], 
                          d=row[5], tw=row[6],
                          bft=row[7], tft=row[8],
                          bfb=row[9], tfb=row[10],
                          root_radius=row[11])
    #
    #
    @property
    def d(self):
        return self.get_item(item="height")

    @d.setter
    def d(self, value:float):
        """ """
        value = get_sect_properties([value])
        self.update_item(item='height', value=value[0])
        self.push_property()
    #
    @property
    def tw(self):
        return self.get_item(item="web_thickness")

    @tw.setter
    def tw(self, value:float):
        """ """
        value = get_sect_properties([value])
        self.update_item(item='web_thickness', value=value[0])
        self.push_property()
    #
    #
    @property
    def bf(self):
        return self.get_item(item="top_flange_width")

    @bf.setter
    def bf(self, value:float):
        """ """
        value = get_sect_properties([value])
        self.update_item(item='top_flange_width', value=value[0])
        self.push_property()
    #
    @property
    def tf(self):
        return self.get_item(item="top_flange_thickness")

    @tf.setter
    def tf(self, value:float):
        """ """
        value = get_sect_properties([value])
        self.update_item(item='top_flange_thickness', value=value[0])
        self.push_property()
    #
    #
    @property
    def bfb(self):
        return self.get_item(item="bottom_flange_width")

    @bfb.setter
    def bfb(self, value:float):
        """ """
        value = get_sect_properties([value])
        self.update_item(item='bottom_flange_width', value=value[0])
        self.push_property()
    #
    @property
    def tfb(self):
        return self.get_item(item="bottom_flange_thickness")

    @tfb.setter
    def tfb(self, value:float):
        """ """
        value = get_sect_properties([value])
        self.update_item(item='bottom_flange_thickness', value=value[0])
        self.push_property()
    #    
#
