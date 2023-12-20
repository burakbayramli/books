# 
# Copyright (c) 2009-2023 fem2ufo
# 
#
# Python stdlib imports
from __future__ import annotations
from collections.abc import Mapping
from typing import NamedTuple
from dataclasses import dataclass
import re
#

# package imports
# steelpy.f2uModel.mesh
from .sqlite.elements import ElementsSQL
#from .inmemory.elements import ElementsIM
#
# steelpy.f2uModel.mesh
#from .process.elements import BeamBasic
#
#
#
#
class Elements(Mapping):
    """
    """
    __slots__ = ['_elements', '_plane']
    
    def __init__(self, nodes, materials, sections,
                 plane: NamedTuple, 
                 mesh_type:str, db_file:str|None=None):
        """
        """
        self._plane = plane
        #
        #if mesh_type != "inmemory":
        self._elements = ElementsSQL(db_file=db_file,
                                     plane=self._plane, 
                                     db_system=mesh_type)
        #else:
        #    self._elements = ElementsIM(nodes=nodes,
        #                                materials=materials,
        #                                sections=sections)
    #
    @property
    def plane(self) -> NamedTuple:
        """ """
        return self._plane
    
    @plane.setter
    def plane(self, plane: NamedTuple):
        """ """
        self._plane = plane
        self._elements.plane(self._plane)
    #    
    #
    def __setitem__(self, element_number: int,
                    parameters: list[float]|dict[str, float]) -> None:
        """
        parameters = ['beam', node1, node2, material, section, roll_angle, title]
        """
        self._elements[element_number] = parameters
        
    #
    def __getitem__(self, element_number:str):
        """
        """
        return self._elements[element_number]
    #
    def __len__(self) -> float:
        return len(self._elements)

    def __iter__(self):
        """
        """
        return iter(self._elements)

    def __contains__(self, value) -> bool:
        return value in self._elements
    #
    def __str__(self, units:str="si") -> str:
        """ """
        lenght = ' m'
        space = " "
        output = "\n"
        output += "{:}\n".format(80*"_")
        output += "\n"
        output += f"{33*space}ELEMENTS\n"
        output += "\n"
        output += (f"Element     Node1    Node2 {4*space} Material  Section {4*space}")
        output += (f"Beta {3*space} Lenght {2*space} Title")
        output += "\n"
        output += "{:}\n".format(80*".")
        output += "\n"
        for key, element in self._elements.items():
            output += element.__str__()
            output += "\n"
        return output
    #
    @property
    def get_free_nodes(self):
        """
        find nodes not sharing elements
        """
        return self._elements.get_free_nodes
    #
    #def iter_elements(self, arraysize=1000):
    #    """
    #    """
    #    return self._elements.iter_elements(arraysize)
    #
    #
    def get_number(self, start:int=0):
        """
        """
        return self._elements.get_number(start)
    #
    #@property
    #def get_connectivities(self):
    #    """ """
    #    return self._elements.get_connectivities
    #
    @property
    def sections(self):
        """ """
        return self._elements._sections
    #
    @property
    def materials(self):
        """ """
        return self._elements._materials
    #
    #
    #@property
    def beams(self, values:None|list|dict=None, df=None):
        """
        """
        return self._elements.beams(values=values, df=df)
    #
    #
    def max_bandwidth(self,  jbc):
        """
        calculate max bandwidth
        ------------------------  
        npi : connectivity end 1
        npj : connectivity end 2
        jbc : nodes freedom
        nel: number of elements
        if we
        npi ,npj, jbc, nel
        """
        #TODO : plates 
        ibndm4 = [0]
        for key, element in self._elements.items():
            #idof, jdof = element.DoF
            #bc1 = jbc[idof]
            #bc2 = jbc[jdof]        
            nodes = element.connectivity
            #for node in nodes:
            bc1 = jbc.loc[nodes[0]].tolist()
            bc2 = jbc.loc[nodes[1]].tolist()
            ieqn = bc1 + bc2
            try:
                ibndm4.append(max([abs(ieqn1 - ieqn2)
                                   for x, ieqn1 in enumerate(ieqn) if ieqn1 > 0
                                   for ieqn2 in ieqn[x+1:] if ieqn2 > 0]))
            except ValueError:
                continue
        #
        return max(ibndm4) + 1
    #
    #
    @property
    def df(self):
        """ """
        #print('element out')
        return self._elements.df
    #
    @df.setter
    def df(self, df):
        """ """
        #
        group = df.groupby("type", sort=False)
        for memb_type, elements in group:
            
            if re.match(r"\b(beam(s)?)\b", memb_type, re.IGNORECASE):
                elements.drop(['node_3', 'node_4'], axis=1, inplace=True)
                self._elements._beams.df=elements
            
            else:
                raise Exception(f"element type {memb_type} not valid")
        #
        #print('element in')       
#
#
#
#