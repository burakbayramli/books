# 
# Copyright (c) 2009-2023 fem2ufo
# 
# Python stdlib imports
from __future__ import annotations
from collections.abc import Mapping
from typing import NamedTuple
import re
#

# package imports
from steelpy.f2uModel.mesh.sqlite.boundary import BoundaryNodeSQL
#from steelpy.f2uModel.mesh.inmemory.boundary import BoundaryNodes
#
#
#
#
class Boundaries:
    """
    """
    __slots__ = ['_nodes', '_labels', '_type', '_number']
    
    def __init__(self, mesh_type:str, db_file:str|None):
        """
        """
        self._labels:list[str|int] = []
        self._type:list[str|int] = []
        self._number:list[int] = []
        #
        #if mesh_type != "inmemory":
        self._nodes = BoundaryNodeSQL(db_file=db_file)
        #else:
        #    self._nodes = BoundaryNodes()
    #
    def __setitem__(self, node_name: int|str,
                    values: list|dict) -> None:
        """
        """
        boundary_type = values[0]
        b_number = self._set_item(b_name=node_name, 
                                  b_type=boundary_type)
        #
        if re.match(r"\b(node(s)?|support(s)?|constrain(s)?)\b", boundary_type, re.IGNORECASE):
            if isinstance(values[1], str):
                self._nodes[node_name] = values[1]
            else:
                self._nodes[node_name] = values[1:]
        #elif 'curve' == boundary_type :
        #    raise Exception('--> Mat type No ready')
        else:
            raise IOError(' Boundary type {:} not recognised'.format(boundary_type))
        
    
    def __getitem__(self, node_name: int|str):
        """
        """
        try:
            index = self._labels.index(node_name)
            boundary_type = self._type[index]
            b_number = self._number[index]
        except ValueError:
            raise KeyError('Invalid boundary name : {:}'.format(node_name))
        #
        if re.match(r"\b(node(s)?|coord(inate)?)\b", boundary_type, re.IGNORECASE):
            return self._nodes[b_number]
        
        else:
            raise IOError(f' boundary type {boundary_type} not recognised')
    #
    #
    def supports(self, values:list|None=None):
        """"""
        return self.nodes(values)
    #
    def nodes(self, values:list|None=None):
        """"""
        if isinstance(values, list):
            for value in values:
                node_name = value[0]
                boundary_type = "node"
                b_number = self._set_item(b_name=node_name, 
                                          b_type=boundary_type)
                #
                self._nodes[node_name] = value[1:]
        #
        return self._nodes 
    #
    def _set_item(self, b_name, b_type):
        """ """
        try:
            self._labels.index(b_name)
            raise IOError('   error boundary {:} already exist'.format(b_name))
        except ValueError:
            boundary_type = b_type
            self._labels.append(b_name)
            self._type.append(boundary_type)
            b_number = next(self.get_number())
            self._number.append(b_number)
        #
        return b_number
    #
    def get_number(self, start:int=1):
        """
        """
        try:
            n = max(self._number) + 1
        except ValueError:
            n = start
        #
        while True:
            yield n
            n += 1
    #
    #
    def __str__(self, units:str="si") -> str:
        """ """
        lenght = ' m'
        space = " "
        output = "\n"
        output += "{:}\n".format(80*"_")
        output += "\n"
        output += f"{33*space}BOUNDARIES\n"
        output += "\n"
        output += (f"Node {14*space} x {6*space} y {6*space} z {5*space} mx {5*space} my {5*space} mz {5*space} title")
        output += "\n"
        output += "{:}\n".format(80*".")
        output += "\n"
        for key, node in self._nodes.items():
            #if sum(node[:6]) == 0:
            #    continue
            output += node.__str__()
        return output
    #
    #
    #
    @property
    def df(self):
        """nodes in dataframe format"""
        #print('nodes df out')
        return self._nodes.df

    @df.setter
    def df(self, df):
        """nodes in dataframe format"""
        try:
            df.columns
            #
            #df[df['typr'].apply(lambda x: 'support'
            #                    #if re.search('^f', x)
            #                    if re.match(r"\b(node(s)?|support(s)?|constrain(s)?)\b", x, re.IGNORECASE)
            #                    else x)]
            #
            df['type'] = df['type'].apply(lambda x: 'support'
                                          if re.match(r"\b(node(s)?|support(s)?|constrain(s)?)\b", x, re.IGNORECASE)
                                          else x)
            #
            grptype = df.groupby('type')
            #
            self._nodes.df = grptype.get_group('support')
        except AttributeError:
            raise IOError('Node df not valid')    
    #
    #
#
#