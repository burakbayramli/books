# Copyright (c) 2019-2023 steelpy

# Python stdlib imports
from __future__ import annotations
#import datetime
#from typing import NamedTuple, Dict, List, Tuple, Union

# package imports
from steelpy.design.codes.api.main import API_design
from steelpy.utils.units.main import Units
##from steelpy.design.beam.process import BeamDesignParameters
from steelpy.f2uModel.load.process.actions import Actions
from steelpy.sections.process.stress import BeamStress
#

class BeamDesign: #(BeamDesignParameters):
    
    __slots__ = ['_L', '_actions', "_material", "_section",
                 '_api', '_iso', '_aisc', '_stress', 
                 'beam_name', 'component']
    
    def __init__(self, name:str, component:str):
        """
        """
        self.beam_name = name
        self.component = component
        #BeamDesignParameters.__init__(self)
        self._actions = Actions()
        self._api = API_design(self)
        # internal stress 
        self._stress = BeamStress(0, 0, 0, 0, 0, 0)
        
    #
    @property
    def L(self):
        """ """
        return self._L
    
    @L.setter
    def L(self, beam_lenght: Units|float):
        """ """
        try:
            self._L = beam_lenght.value
        except AttributeError:
            self._L = beam_lenght
    #
    #
    #
    @property
    def material(self):
        """
        """
        return self._material
    
    @material.setter
    def material(self, value):
        """
        """
        self._material = value
    
    @property
    def section(self):
        """
        """
        return self._section
    
    @section.setter
    def section(self, value):
        """
        """
        self._section = value    
    #
    @property
    def stress(self):
        """
        """
        return self._stress
    #
    @property
    def actions(self):
        """
        """
        return self._actions
    
    @actions.setter
    def actions(self, value):
        """
        """
        self._actions = value
    #
    #
    def applied_loads(self, Px, Vy, Vz, BMx, BMy, BMz):
        """
        """
        # Axial        
        self.actions.Fx = Px
        # Shear        
        self.actions.Fz = Vy # ip
        self.actions.Fy = Vz # op
        # Bending
        self.actions.Mx = BMx
        self.actions.My = BMy # ip
        self.actions.Mz = BMz # op
    #    
    #
    @property
    def API(self):
        """
        """
        return self._api
    #
    #
    def __str__(self) -> str:
        """ """
        print('----')
        #print(self._api)
        #return self._header()