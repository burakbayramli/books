# 
# Copyright (c) 2009-2023 fem2ufo
# 
#
#
# Python stdlib imports
from __future__ import annotations
from collections.abc import Mapping
from dataclasses import dataclass
from typing import Iterable
import re
#
# package imports
from ..process.mechanical import MaterialElastic, Curve #, MaterialIsotropic
from ..process.operations import get_isomat_prop, get_isomat_prop_df # find_mat_type,
#
#
class MaterialIM(Mapping):
    """
    """
    __slots__ =  ['_type', '_labels', '_number', '_material',
                  '_elastic', '_curve', '_default']
    
    def __init__(self):
        """
        """
        self._default:str|None = None
        self._labels:list[str|int] = []
        self._type:list[str|int] = []
        self._number:list[int] = []
        #
        self._elastic = MaterialElastic()
        self._curve = Curve(self)
        #self._material:dict = {}
    def __setitem__(self, material_name:str|int,
                    properties:list[str|float]) -> None:
        """
        [name, elastic, Fy, Fu, E, G, Poisson, density, alpha, title(optional)]
        """
        try:
            self._labels.index(material_name)
            raise IOError('   error material {:} already exist'.format(material_name))
        except ValueError:
            material_type = properties[0]
            self._labels.append(material_name)
            self._type.append(material_type)
            mat_number = next(self.get_number())
            self._number.append(mat_number)
            #
            if re.match(r"\b(curve)\b", material_type, re.IGNORECASE):
            #if 'curve' == material_type :
                raise Exception('--> Mat type No ready')
                #self._material[mat_number] = CurveIsotropic(material_name, *properties[1:])
            elif re.match(r"\b(elastic|linear|isotropic)\b", material_type, re.IGNORECASE):
            #elif 'elastic' == material_type :
                self._elastic[material_name] = [material_name, *properties[1:]]
                #self._material[mat_number] = MaterialIsotropic(material_name, *properties[1:])
            else:
                raise IOError(' material type {:} not recognised'
                              .format(material_type))
        #
        self._default = material_name
    
    def __getitem__(self, material_name:str|int):
        """
        """
        try:
            index = self._labels.index(material_name)
            material_type = self._type[index]
            #mat_number = self._number[index]
            self._default = material_name
            #return self._material[mat_number]
        except ValueError:
            raise KeyError(f'Material name {material_name} not found')
        #
        if re.match(r"\b(curve)\b", material_type, re.IGNORECASE):
        #if 'curve' == material_type :
            return self._curve[material_name]
        elif re.match(r"\b(elastic|linear|isotropic)\b", material_type, re.IGNORECASE):
        #elif 'elastic' == material_type :
            return self._elastic[material_name]
        else:
            raise IOError(f' material type {material_type} not recognised')
    
    def __delitem__(self, material_name:str|int) -> None:
        """
        """
        _index = self._labels.index(material_name)
        1/0
        del self._labels[material_name]
        #del self._labels[material_number]
    
    def __len__(self):
        return len(self._labels)
    
    def __iter__(self):
        """
        """
        return iter(self._labels)
    
    def __contains__(self, value):
        return value in self._labels
    #
    # Modify material
    #
    @property
    def default(self):
        """ """
        return self._default

    @default.setter
    def default(self, material_name:str|int):
        """ """
        if not material_name in self._labels:
            raise IOError(f'material {material_name} missing')
        self._default = material_name
    #
    #@property
    def get_item_by_number(self, material_name):
        """
        """
        1/0
        #_items = {_item.number:key for key, _item in self._materials.items()}
        try:
            #material_name = _items[material_number]
            return self.__getitem__(material_name)
        except KeyError:
            raise KeyError('Invalid material name')
    #
    def get_material(self):
        """
        """
        summary = {}
        #xxx = self._elastic._getdf()
        for index, name in enumerate(self._labels):
            #mat_number = self._number[index]
            #try:
            dat = self._elastic[name]
            #summary[index] = mat._get_data()
        return summary
    #
    def get_number(self, start:int=1)-> Iterable[int]:
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
    #@property
    #def elastic(self):
    #    """ """
    #    return self._elastic
    
    #@elastic.setter
    def elastic(self, values:None|list=None,
                df=None):
        """ """
        if values:
            if isinstance(values, list):
                for item in values:
                    material_name = item[0]
                    material_type = "elastic"
                    self._labels.append(material_name)
                    self._type.append(material_type)
                    mat_number = next(self.get_number())
                    self._number.append(mat_number)
                    #self.__setitem__(item[0], item[1:])
                    properties = get_isomat_prop(item[1:])
                    self._elastic[material_name] = [material_name, *properties]
            
            else:
                raise IOError('material input not valid')
        # dataframe input
        try:
            df.columns
            df = df.drop_duplicates(['name'], keep='first')
            #
            group = df.groupby("type")
            elastic = group.get_group("elastic")
            elastic = get_isomat_prop_df(elastic)
            elastic = elastic.drop_duplicates(['name'])
            #
            self._labels.extend(elastic.name)
            self._type.extend(elastic.type)
            material_number = [next(self.get_number()) for _ in elastic.name]
            self._number.extend(material_number)
            #
            self._elastic.df = elastic
        except AttributeError:
            pass
        #print('--')
        #return self._elastic
        return MaterialType(mat_type="elastic",
                            cls_type=self._elastic, cls=self)
#
#
@dataclass
class MaterialType:
    __slots__ = ['_type', '_labels', '_number',
                 '_cls_type', '_item_type']
    
    def __init__(self, mat_type: str, cls_type, cls):
        """
        """
        self._cls_type = cls_type
        self._item_type = mat_type
        self._labels = cls._labels
        self._type = cls._type
        self._number = cls._number
        
    def __setitem__(self, material_name:str|int,
                    properties:list[str|float]) -> None:
        """
        """
        self._labels.append(material_name)
        self._type.append(self._item_type)
        mat_number = next(self.get_number())
        self._number.append(mat_number)
        prop = get_isomat_prop(properties)
        self._cls_type[material_name] = [material_name, *prop]
    
    def __getitem__(self, material_name:str|int):
        """
        """
        #index = self._labels.index(material_name)
        #mat_number = self._number[index]        
        return self._cls_type[material_name]
    #
    @property
    def df(self):
        """ """
        return self._cls_type.df
    #
    def get_number(self, start:int=1)-> Iterable[int]:
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
#