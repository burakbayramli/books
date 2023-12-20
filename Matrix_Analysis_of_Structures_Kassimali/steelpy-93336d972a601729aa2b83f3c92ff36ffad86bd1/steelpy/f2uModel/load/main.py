# 
# Copyright (c) 2009-2023 fem2ufo
#

# Python stdlib imports
from __future__ import annotations
#from dataclasses import dataclass
import re
from typing import NamedTuple

# package imports
# steelpy.f2uModel.load
#from .inmemory.main import LoadingInmemory
from .concept.main import BasicLoadConcept, LoadCombConcept
#from .concept.combination import LoadCombConcept
#from .inmemory.timehistory import TimeHistory
#
#from .sqlite.main import LoadingSQL
from .sqlite.main import BasicLoadSQL, LoadCombSQL
#from .sqlite.combination import LoadCombSQL
from steelpy.f2uModel.plot.main import PlotLoad

#
#
class Load:
    """
    """
    __slots__ = ['_labels', '_number', '_df_nodal',
                 '_basic', '_combination', '_plane',
                 '_elements', '_nodes', '_boundaries']

    def __init__(self, nodes, elements, boundaries, 
                 plane: NamedTuple, 
                 mesh_type: str,
                 db_file: str | None = None):
        """
        """
        # mesh components
        self._nodes = nodes
        self._elements = elements
        self._boundaries = boundaries
        self._plane = plane
        #
        self._number = []
        self._labels = []
        #if mesh_type != "inmemory":
            #self._load = LoadingSQL(db_file=db_file,
            #                        db_system=mesh_type)
        self._basic = BasicLoadSQL(db_file, plane=self._plane)
        self._combination = LoadCombSQL(db_file, plane=self._plane)
        #else:
        #    #self._load = LoadingInmemory(nodes=nodes,
        #    #                             elements=elements)
        #    self._basic = BasicLoad(nodes=nodes, elements=elements)
        #    self._combination = LoadCombination(basic_load=self._basic)
    #
    #
    def __str__(self) -> str:
        """ """
        output = "\n"
        output += self._basic.__str__()
        output += self._combination.__str__()
        return output
    #
    # ----------------------------
    #
    @property
    def plane(self) -> NamedTuple:
        """
        """
        return self._plane
        
    @plane.setter
    def plane(self, plane: NamedTuple) -> None:
        """
        """
        self._plane = plane
        self._basic._plane = self._plane
        self._combination._plane = self._plane

    #
    # ----------------------------
    #
    def basic(self, values: None|list|tuple = None,
              df = None):
        """
        """
        if isinstance(values, (list, tuple)):
            number = len(self._basic) #+ 1
            if isinstance(values[0], (list,tuple)):
                for item in values:
                    name = item[0]
                    try:
                        index = self._labels.index(name)
                        number = self._number[index]
                    except ValueError:
                        number += 1
                        self._basic[number] = name
                        self._number.append(number)
                        self._labels.append(name)
                    #
                    if re.match(r"\b(node(s)?|point(s)?)\b", item[1], re.IGNORECASE):
                        self._basic[number].node(item[2:])
                    
                    elif re.match(r"\b(beam(s)?)\b", item[1], re.IGNORECASE):
                        self._basic[number].beam(item[2:])
                    
                    else:
                        raise IOError(f"Basic load type {item[1]} not available")
            else:
                name = values[0]
                try:
                    index = self._labels.index(name)
                    number = self._number[index]
                except ValueError:
                    number += 1
                    self._basic[number] = name
                    self._number.append(number)
                    self._labels.append(name)
                #
                if re.match(r"\b(node(s)?|point(s)?)\b", values[1], re.IGNORECASE):
                    self._basic[number].node(values[2:])
                
                elif re.match(r"\b(beam(s)?)\b", values[1], re.IGNORECASE):
                    self._basic[number].beam(values[2:])                
        #
        # dataframe input
        try:
            df.columns
            #self._sections.df(df)
        except AttributeError:
            pass
        #
        return self._basic

    #
    # ----------------------------
    #
    def combination(self, values: None | list = None,
                    df = None):
        """
        """
        if isinstance(values, list):
            number = len(self._combination)
            for item in values:
                name = item[0]
                try:
                    index = self._labels.index(name)
                    number = self._number[index]
                except ValueError:
                    number += 1
                    self._combination[number] = name
                    self._number.append(number)
                    self._labels.append(name)
                #
                if re.match(r"\b(basic(_)?(load)?)\b", item[1], re.IGNORECASE):
                    self._combination[number]._basic[item[2]] = item[3]
                
                elif re.match(r"\b(comb(ination)?(_)?(load)?)\b", item[1], re.IGNORECASE):
                    self._combination[number]._combination[item[2]] = item[3]
                
                else:
                    raise IOError(f"Combination load type {item[1]} not available")
        #
        #
        # dataframe input
        try:
            df.columns
            #self._sections.df(df)
        except AttributeError:
            pass
        #
        return self._combination

    #
    # ----------------------------
    #
    def time_history(self):
        """
        """
        return self.th

    #
    # ----------------------------
    #
    def mass(self):
        """
        :return:
        """
        return self._mass

    #
    #
    # --------------------
    # Plotting
    # --------------------
    #
    #@property
    def plot(self, figsize:tuple = (10, 10)):
        """ """
        return PlotLoad(cls=self, figsize=figsize)
#
#
#
class ConceptLoad:
    
    __slots__ = ["_basic", "_combination", 'th', '_mass',
                 '_nodes', '_elements', '_boundaries']
    
    def __init__(self, points, elements, boundaries) -> None:
        """
        """
        self._nodes = points
        self._elements = elements
        self._boundaries = boundaries
        #
        self._basic = BasicLoadConcept(points=self._nodes,
                                       elements=self._elements)
        #self.th = TimeHistory()
        self._combination = LoadCombConcept(basic_load=self._basic)
        self._mass = LoadCombConcept(basic_load=self._basic)
    #
    #@property
    def basic(self):
        """
        """
        return self._basic
    #
    #@property
    def combination(self):
        """
        """
        return self._combination
    #
    #@property
    #def time_history(self):
    #    """
    #    """
    #    return self.th
    #
    def mass(self):
        """
        """
        return self._mass
    #
        #
    def __str__(self) -> str:
        """ """
        output = "\n"
        output += self._basic.__str__()
        output += self._combination.__str__()
        return output
    #
    # --------------------
    # Plotting
    # --------------------
    #
    #@property
    def plot(self, figsize:tuple = (10, 10)):
        """ """
        return PlotLoad(cls=self, figsize=figsize)    
#
