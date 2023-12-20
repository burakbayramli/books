#
# Copyright (c) 2009-2023 fem2ufo
# 

# Python stdlib imports
from __future__ import annotations
from array import array
from collections.abc import Mapping
#from collections import defaultdict
#from typing import NamedTuple

# package imports
#import pandas as pd
from steelpy.utils.dataframe.main import DBframework

#
class LoadCombinationBasic(Mapping):
    __slots__ = ['_labels', '_title', '_number',
                 '_index', '_basic', '_combination', '_metocean']
    
    def __init__(self):
        """
        """
        self._labels: list[str|int] = []
        self._title: list[str] = []
        self._number: array = array("I", [])
    #
    def __len__(self) -> int:
        return len(self._labels)
    #
    def __iter__(self):
        """
        """
        #comb =  list(dict.fromkeys(self._labels))
        #return iter(comb)
        return iter(self._labels)
    #
    def get_number(self, start:int=1):
        """
        """
        try:
            n = len(self._labels) + 1
        except ValueError:
            n = start
        #
        while True:
            yield n
            n += 1    
    #
    def __str__(self) -> str:
        """ """
        output = "\n"
        output += "{:}\n".format(80*"_")
        output += "\n"
        output += "{:}LOAD COMBINATIONS\n".format(30*" ")
        #output += "--- Basic Load \n"
        #output += f"Load Type [Basic/Combination]\n"
        output += f"Load Type   Name{' '*10} Factor\n"
        #output += "\n"
        #output += "\n"
        output += "\n"
        output += "{:}\n".format(80*".")
        output += "\n"
        for key in self._labels:
            lcase = self.__getitem__(key)
            output += f"Load Name : {str(key):12s}  Number : {lcase.number:8.0f}  Title : {lcase.title}\n"
            try:
                #output += f"--- Basic\n"
                for basic_name, factor in lcase.basic.items():
                    output += f"Basic       {str(basic_name):12s} {factor: 1.4e}\n"
                    #name, basic
            except TypeError:
                pass
            # comb
            try:
                #output += f"--- Combination\n"
                for comb_name, factor in lcase.combination.items():
                    output += f"Combination {str(comb_name):12s} {factor: 1.4e}\n"
            except TypeError:
                continue
            #name, comb
            output += "\n"
        #print('---')
        return output
    #
    def to_basic(self):
        """ """
        db = DBframework()
        # get combination of combination and convert them to basic loads
        # TODO : check this loop works
        for key, item in self._combination.items():
            for comb_name, factor in item._combination.items():
                for precomb, prefactor in self._combination[comb_name]._basic.items():
                    try:
                        item._basic[precomb] += prefactor * factor
                    except KeyError:
                        item._basic[precomb] = prefactor * factor
        # organize basic load
        #basic_loads = defaultdict(list)
        # form combination formed by basic loads only
        dftemp = []
        for key, item in self._combination.items():
            for bl_name, factor in item._basic.items():
                #basic_loads[key].append([bl_name, factor])
                dftemp.append([key, item.number, 'combination', item.title, bl_name, factor])
                #try:
                #    #basic = self._basic[bname]
                #    #basic_loads[item.name].append([basic.title, factor])
                #    basic_loads[key].append([bl_name, factor])
                #except KeyError:
                #    raise Warning("  warning basic load {:} not found".format(bname))
        #
        header = ['load_name', 'load_number','load_type', 'load_title', 'basic_load', 'factor']
        df_comb = db.DataFrame(data=dftemp, columns=header, index=None)
        return df_comb #, basic_loads    
    #