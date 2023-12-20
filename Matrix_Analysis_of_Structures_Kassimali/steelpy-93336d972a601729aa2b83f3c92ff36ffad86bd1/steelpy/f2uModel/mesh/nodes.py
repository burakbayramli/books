# 
# Copyright (c) 2009 steelpy
# 


# Python stdlib imports
from __future__ import annotations
from array import array
from collections.abc import Mapping
from dataclasses import dataclass
import itertools as it
from typing import NamedTuple
#

# package imports
# steelpy.f2uModel.mesh
from .sqlite.nodes import NodeSQL
#from .inmemory.nodes import NodesIM
from .process.nodes import node_renumbering
#
from steelpy.utils.math.operations import zeros, to_matrix
from steelpy.utils.dataframe.main import DBframework
#
#
#
#
#
#
class Nodes(Mapping):
    """
    """
    __slots__ = ['_nodes', '_jbc', '_db',
                 '_plane']
    
    def __init__(self, mesh_type:str, plane: NamedTuple, 
                 db_file:str|None):
        """
        
        plane2D : Analysis in 2D plane (False/True)
        db_file : Database file name 
        """
        #
        self._plane = plane
        #
        self._nodes = NodeSQL(db_file=db_file,
                              db_system=mesh_type)
        #
        self._jbc: array = array('I', [])
        self._db = DBframework()
    #
    #
    @property
    def plane(self) -> NamedTuple:
        """ """
        return self._plane
    #
    @plane.setter
    def plane(self, plane: NamedTuple) -> None:
        """ """
        self._plane = plane
    #
    #
    def __setitem__(self, node_name: int|str,
                    coordinates: list[float]|dict[str, float]) -> None:
        """
        """
        self._nodes[node_name] = coordinates
    #
    def __getitem__(self, node_name:int|str):
        """
        """
        #print('<-- mat' )
        return self._nodes[node_name]
    #
    def __len__(self) -> int:
        return len(self._nodes)

    def __iter__(self):
        """
        """
        _nodes = sorted(self._nodes)
        return iter(_nodes)

    def __contains__(self, value) -> bool:
        return value in self._nodes
    #
    def __str__(self, units:str="si") -> str:
        """ """
        lenght = ' m'
        space = " "
        output = "\n"
        output += "{:}\n".format(80*"_")
        output += "\n"
        output += f"{33*space}NODES\n"
        output += "\n"
        output += (f"Node {12*space} x  [{lenght}] {4*space} y  [{lenght}] {4*space} z  [{lenght}] ")        
        output += "\n"
        output += "{:}\n".format(80*".")
        output += "\n"        
        for key, node in self._nodes.items():
            output += node.__str__()
        return output
    #
    def get_node_name(self, coordinates, tol: float = 0.01):
        """ """
        return self._nodes.get_point_name(coordinates, tol)
    #
    #
    def get_new_node(self, coordinates):
        """ """
        return self._nodes.get_new_point(coordinates)
    #
    #
    def update_item(self, node_name:int, item:str, value:float|int):
        """ """
        self._nodes.update_item(node_name, item, value)
        #print('---')
    #
    #
    #def update_number(self, node_name:int, number:int):
    #    """ """
    #    self._nodes.update_number(node_name, number)
    #
    #
    def renumbering(self, elements):
        """ """
        new_node_list = node_renumbering(self._nodes, elements)
        #new_node_list = list(reversed(new_node_list))
        self._nodes.renumbering(new_node_list)
        #print('end')
        #return single_nodes
    #
    def get_maxmin(self):
        """get """
        return self._nodes._get_maxmin()
    #
    #@property
    def jbc(self, supports):
        """ joints with boundary"""
        nnp = len(self._nodes)
        jbc = zeros(nnp, 6, code='I')
        #
        for node_name, bd in supports.items():
            ind = self._nodes[bd.node].index
            jbc[ind] = bd[:6]
        #
        #jbc = self.jbc.transposed()
        #jbc = boundaries.transposed()
        #jbc = list(it.chain.from_iterable(jbc))
        #
        #jbc = to_matrix(jbc, 6)
        df_jbc = self._db.DataFrame(data=jbc,
                                    columns=['x', 'y', 'z', 'rx', 'ry', 'rz'])
        jbc = df_jbc[self._plane.dof].values.tolist()
        jbc = list(it.chain.from_iterable(jbc))
        #
        counter = it.count(start=1)
        jbc = [next(counter) if item == 0 else 0
               for item in jbc]
        # update jbc
        self._jbc = array('I', jbc)
        #
        jbc = to_matrix(self._jbc, self._plane.ndof)
        df_jbc = self._db.DataFrame(data=jbc, columns=self._plane.dof)
        node_name = list(self._nodes.keys())
        df_jbc['node_name'] = node_name
        df_jbc = df_jbc.set_index('node_name', drop=True)    
        # remove rows with zeros
        #df_jbc = df_jbc[df_jbc.any(axis=1)]
        #df_jbc.replace(0, self._db.nan, inplace=True)
        #df_jbc = df_jbc.notnull()        
        return df_jbc
    #
    def neq(self, supports):
        """ Number the equations  in jbc from 1 up to the order.
           Start assigning equation numbers for zero dof's
           from 1 up;  only zero given a number. """
        #
        if not self._jbc:
            self.jbc(supports)
        neq = max(self._jbc)
        #jbc = to_matrix(self._jbc, 6)
        return neq
    #
    #
    @property
    def df(self):
        """nodes in dataframe format"""
        #print('nodes df out')
        return self._nodes.df       
        #1/0

    @df.setter
    def df(self, df):
        """nodes in dataframe format"""
        try:
            df.columns
            self._nodes.df =df
        except AttributeError:
            raise IOError('Node df not valid')
    #
    #
#
#