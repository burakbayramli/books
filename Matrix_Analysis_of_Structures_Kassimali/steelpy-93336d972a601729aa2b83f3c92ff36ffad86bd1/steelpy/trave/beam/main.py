# 
# Copyright (c) 2019-2023 steelpy
# 

# Python stdlib imports
from __future__ import annotations
#from array import array
from dataclasses import dataclass
#from collections.abc import Mapping
#from typing import NamedTuple
from collections import defaultdict


# package imports
# 
from steelpy.trave.beam.load.main import BeamBasicLoad
from steelpy.trave.beam.pilkey.main import BeamBasic
#
from steelpy.sections.main import SectionIM
from steelpy.material.main import Materials
#
#import pandas as pd
from steelpy.utils.dataframe.main import DBframework
#

# 
#
class Beam:
    __slots__ = ['_support', 'load', '_sections', '_materials',
                 'steps', 'length', '_response',
                 '_load_combination', '_design', 'name', '_db']

    def __init__(self, name:int|str):
        """
        beam_length : total length of the beam 
        section_type:str
        """
        print("-- module : Beam Version 0.25dev")
        print('{:}'.format(52*'-'))
        #
        self.name = name
        self.steps: int = 10
        #
        self._materials = Materials()
        #
        self._sections = SectionIM()
        #
        self._db = DBframework()
        #
        self.load = BeamBasicLoad(beam=self)
    #
    #
    #
    @property
    def L(self):
        """ """
        return self.length
    
    @L.setter
    def L(self, beam_length):
        """ """
        try:
            self.length = beam_length.value
        except AttributeError: # unit must be in metres
            self.length = beam_length
    #
    # -----------------------------------------------------
    #    
    @property
    def section(self):
        """
        """
        return self._sections['beam']

    @section.setter
    def section(self, section):
        """
        section_type : select beam's cross section shape (tubular/rectangular,I_section)
        """
    #    self._section = section
        self._sections["beam"] = section
    #
    @property
    def material(self):
        """
        """
        return self._materials['beam']

    @material.setter
    def material(self, value):
        """
        """
        self._materials['beam'] = value
    #
    # -----------------------------------------------------
    #
    @property
    def support(self):
        """
        """
        return self._support

    @support.setter
    def support(self, value:list):
        """
        [end1, end2, torsion1, torsion2, k1, k2]
        """
        support = [item for item in value[:2]]
        try:
            support.append(value[2]) # t1
        except:
            support.append(value[0])
        
        try:
            support.append(value[3]) # t2
        except:
            support.append(value[1])
        
        try:
            support.append(value[4]) # k1
        except:
            support.append(None)
        try:
            support.append(value[5]) # k1
        except:
            support.append(None)
        self._support = support
    #
    # -----------------------------------------------------
    # Loading
    #
    @property
    def selfweight(self):
        """ """
        return self.load.basic._selfweight
    
    @selfweight.setter
    def selfweight(self, value:list|dict):
        """ """
        self.load.basic._selfweight = value
    #
    #
    @property
    def P(self):
        """
        """
        return self.load.basic.point # (beam_name=self._name)

    @P.setter
    def P(self, value:list|dict):
        """
        """
        self.load.basic.point = value
    #
    @property
    def q(self):
        """line load"""
        return self.load.basic.line

    @q.setter
    def q(self, value:list|dict):
        """line load"""
        self.load.basic.line = value
    #
    @property
    def load_combination(self):
        """load combination"""
        return self.load.combination

    @load_combination.setter
    def load_combination(self, value:list|dict):
        """load combination"""
        
        if isinstance(value, dict):
            for key, item in value.items():
                self.load.combination[key] = item
        else:
            if isinstance(value[0], list):
                for item in value:
                    self.load.combination[item[0]] = item[1]
            else:    
                self.load.combination[value[0]] = value[1]
    #
    #@property
    #def load_combination(self):
    #    """
    #    """
    #    return self._load_combination
    #
    #@load_combination.setter
    #def load_combination(self, value:list):
    #    """
    #    """
    #    self._load_combination.append(value)
    #
    # -----------------------------------------------------
    # TODO: beam with multiple supports
    #
    def join(self, other, join_type):
        """
        b1 = Beam()
        b2 = Beam()
        b = b1.join(b2, 'fixed')
        """
        pass
    #
    #
    #
    # -----------------------------------------------------
    # Calculations
    #
    #    
    #
    def _beam(self):
        """get beam""" 
        material = self.material
        section = self.section.properties()
        #
        return BeamBasic(L=self.length, area=section.area, 
                          Iy=section.Iy, Iz=section.Iz,
                          J=section.J, Cw=section.Cw, 
                          E=material.E, G=material.G)
    #
    def _getloads(self):
        """get beam loading""" 
        bloads = []
        # line load
        bloads.extend(self.load.basic.line)
        # point load
        bloads.extend(self.load.basic.point)
        #
        return bloads
    #
    #    
    #
    # -----------------------------------------------------
    # Results    
    #
    #
    #@property
    #def shear(self):
    #    """ """
    #    return self._response.shear(self.steps)
    #
    #@property
    #def bending_moment(self):
    #    """ """
    #    return self._response.bending(self.steps)
    #
    #@property
    #def slope(self):
    #    """ """
    #    return self._response.slope(self.steps)
    #
    #@property
    #def deflection(self):
    #    """ """
    #    return self._response.deflection(self.steps)
    #
    def reactions(self):
        """
        Return: 
        dataframe : [load_name, load_title, load_type, system,
                     element_name, support_type, support_end,
                     Fx, Fy, Fz, Mx, My, Mz,
                     delta_x, delta_y, delta_z,
                     phi_x, theta_y, theta_z]
        """
        bloads = self._getloads()
        #
        beam = self._beam()
        beam.supports(*self.support)
        #
        # load_name : [load_title, load_type,
        #              load_system, beam_name,
        #              R0[Fa, Tx, Ry, Rz],
        #              R1[Fa, Tx, Ry, Rz]]
        #
        # Tx = [FT, FB, Fpsi, Fphi, Tw]
        # Fa, Ry & Rz = [V, M, theta, w]        
        reactions = beam.reactions(bloads)
        #
        # Organize data for reaction output
        msupport = []
        for key, item in reactions.items():
            for reac in item:
                msupport.extend([[key, *reac[0:4],       # load details & beam name
                                  self._support[x], x+1, # support type & beam end
                                  lbf[0][0],  # Fx [axial]
                                  lbf[1][0],  # Mx [torsion]
                                  lbf[1][3],  # phi_x [torsion]
                                  lbf[0][3],  # delta_x [axial]
                                  *lbf[2],    # Bending in_plane
                                  *lbf[3]]    # Bending out_plane
                                 for x, lbf in enumerate(reac[4:])]) # R0 & R1
        #
        # Dataframe setup 
        #
        header = ['load_name', 'load_title','load_type',
                  'system', 'element_name', 
                  'support_type', 'support_end',
                  'Fx', 'Mx', 'phi_x', 'delta_x',
                  'Fy', 'Mz', 'theta_z', 'delta_y',
                  'Fz', 'My', 'theta_y', 'delta_z']
        #
        df_mload = self._db.DataFrame(data=msupport, columns=header, index=None)
        df_mload = df_mload[['load_name', 'load_title', 'load_type', 'system',
                             'element_name', 'support_type', 'support_end',
                             'Fx', 'Fy', 'Fz', 'Mx', 'My', 'Mz',
                             'delta_x', 'delta_y', 'delta_z',
                             'phi_x', 'theta_y', 'theta_z']]
        #
        return df_mload    
    #
    #
    #@property
    def response(self, steps: int = 10):
        """
        Results:
        Beam force response 
        Dataframe [load_name, load_title, load_type, load_system,
                   element_name, node_end,
                   Fx, Fy, Fz, Mx, My, Mz,
                   delta_x, delta_y, delta_z,
                   phi_x, theta_y, theta_z,
                   B(bimoment), Tw(Warping torque)]
        """
        bloads = self._getloads()
        #        
        # -----------------------------------------------------
        #        
        beam = self._beam()
        beam.supports(*self.support)
        #lbforce = beam.solve(bloads, steps)
        #
        #
        #lbforce =[[*lbf[:6], *lbf[6], *lbf[7], *lbf[8]]
        #          for lbf in lbforce]
        #
        # Member
        # [Fx, Fy, Fz, Mx, My, Mz]
        # [V, M, theta, w]
        #header = ['load_name', 'load_title', 'load_type', 'system',
        #          'element_name', 'node_end',
        #          'Fx', 'Mx', 'theta_x', 'delta_x',
        #          'Fy', 'Mz', 'theta_z', 'delta_y',
        #          'Fz', 'My', 'theta_y', 'delta_z']
        #
        #df_mload = self._db.DataFrame(data=lbforce, columns=header, index=None)
        #df_mload = df_mload[['load_name', 'load_title', 'load_type', 'system',
        #                     'element_name', 'node_end',
        #                     'Fx', 'Fy', 'Fz', 'Mx', 'My', 'Mz',
        #                     'delta_x', 'delta_y', 'delta_z', 'theta_x', 'theta_y', 'theta_z']]
        #
        #
        #
        df_mload = beam.forces(bloads, steps)
        return df_mload        
    #
    #
    def stress(self, steps: int = 10):
        """calculate beam stress"""
        actions_df = self.response(steps=steps)
        stress = self.section.stress(df=actions_df)
        return stress
    #
    @property
    def design(self, material=None):
        """
        """
        self._design.material = material
        if not material:
            self._design.material = self._material
        #
        self._design.section = self._section
        self._design.L = self.length
        self._design._stress = self._response.stress()
        return self._design
#
#
#
#
