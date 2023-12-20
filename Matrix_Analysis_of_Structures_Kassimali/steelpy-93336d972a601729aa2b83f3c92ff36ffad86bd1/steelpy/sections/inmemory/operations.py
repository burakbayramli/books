#
# Copyright (c) 2019-2023 steelpy
#

# Python stdlib imports
from __future__ import annotations
from dataclasses import dataclass
from collections.abc import Mapping
import math
import re
#

# package imports
#
from ..process.operations import get_sect_properties,  get_Isection
from steelpy.utils.dataframe.main import DBframework
import numpy as np
#
#
#
# ----------------------------------------
#      Standard Sections Profiles
# ----------------------------------------
#
#
#
#
#
@dataclass(kw_only=True)
class ShapeBasic:
    name: str|int
    build: str = 'welded'
    #
    # -------------------------------------
    # Operations
    # -------------------------------------
    #
    def stress(self, actions=None, stress=None, df=None):
        """return cross section stress"""
        #print('-->')
        try:
            df.columns
            dfres = DBframework()
            # -------------------------------------
            try:
                stress_df = df[['node_end', 'tau_x', 'tau_y', 'tau_z',
                                'sigma_x', 'sigma_y', 'sigma_z']]
                stress = self._stress(stress=stress_df)
            except KeyError:
                actions_df = df[['load_title', 'node_end',
                                 'Fx', 'Fy', 'Fz', 'Mx', 'My', 'Mz',
                                 'B', 'Tw']]
                stress = self._stress(actions=actions_df)
                # -------------------------------------
                header = ['load_name', 'load_title', 
                          'element_name','node_end','stress_points', 'y', 'z']
                # -------------------------------------
                coord = stress.stress_point
                items = [[row.load_name, row.load_title,
                          row.element_name, row.node_end,
                          x+1, coord.y[x], coord.z[x]]
                         for x in range(len(coord.y))
                         for row in df.itertuples()]
                df_stress = dfres.DataFrame(data=items, columns=header, index=None)
                # -------------------------------------
                # axial stress
                df_stress['tau_x'] = np.array(stress.tau_x).flatten()
                # In Plane shear stress
                df_stress['tau_y'] = np.array(stress.tau_y).flatten()
                # Out Plane shear stress
                df_stress['tau_z'] = np.array(stress.tau_z).flatten()
                # torsion stress
                df_stress['sigma_x'] = np.array(stress.sigma_x).flatten()
                # In plane bending stress
                df_stress['sigma_y'] = np.array(stress.sigma_y).flatten()
                # Out plane bending stress
                df_stress['sigma_z'] = np.array(stress.sigma_z).flatten()
                # return dataframe
                return df_stress

        except AttributeError:
            if stress:
                print("stress")
            elif actions:
                print("actions")
            else:
                print('--> ??')
                1 / 0
    #
    def add_stress(self, stress, other):
        """ """
        if isinstance(stress.tau_x, list):
            stress.tau_x = self._combine_stress(other.tau_x, stress.tau_x)
            stress.tau_y = self._combine_stress(other.tau_y, stress.tau_y)
            stress.tau_z = self._combine_stress(other.tau_z, stress.tau_z)
            #
            stress.sigma_x = self._combine_stress(other.sigma_x, stress.sigma_x)
            stress.sigma_y = self._combine_stress(other.sigma_y, stress.sigma_y)
            stress.sigma_z = self._combine_stress(other.sigma_z, stress.sigma_z)
        else:
            # Assuming global stress
            stress.tau_x = self._add_global_stress(other.tau_x, stress.tau_x)
            stress.tau_y = self._add_global_stress(other.tau_y, stress.tau_y)
            stress.tau_z = self._add_global_stress(other.tau_z, stress.tau_z)
            #
            stress.sigma_x = self._add_global_stress(other.sigma_x, stress.sigma_x)
            stress.sigma_y = self._add_global_stress(other.sigma_y, stress.sigma_y)
            stress.sigma_z = self._add_global_stress(other.sigma_z, stress.sigma_z)
        #
        return stress
    #
    def _add_global_stress(self, stress_local, stress_global):
        """
        """  
        # _new_stress = [ _item + math.copysign(1, _item.value) * stress_global  
        #                 if _item.value != 0  else _item for _item in stress_local] #aldh6850
        
        #aldh6850 - update to ensure the "global" stress has the same sign as the "local" stress to be conservative
        #aldh6850 - update to ensure when the "local" stress is zero the "global" stress is used
        
        _new_stress = [ _item + math.copysign(1, _item.value) * abs(stress_global)  
                        if _item.value != 0  else stress_global for _item in stress_local] #aldh6850
        
        
        return _new_stress
    #
    def _combine_stress(self, stress_1, stress_2):
        """
        """
        # change * by +
        _new_stress = [stress_1[x] + math.copysign(1, stress_1[x].value) * abs(stress_2[x]) 
                       for x in range(9)]
        return _new_stress   
    #
    #
    # -------------------------------------
    #
    def _print_section_properties(self):
        """
        """
        file = shape_io.print_header()
        file.extend(self._shape())
        file.extend(shape_io.print_properties(self))
        return file    
    #    
    # -------------------------------------
    #
    #@property
    def properties(self):
        """
        --------------------------
        General Beam Element Data
        --------------------------
        
        Parameters  
        ----------
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
        return self._properties()
    #
    #
    def push_property(self):
        """ """
        self.properties
    #    
    
#
#
#
class SectionBasic(Mapping):
    #__slots__ = ['_labels', '_number', '_title', '_type',
    #             '_tubular', '_solid', '_ibeam', '_box',
    #             '_channel', '_tee', '_angle', '_default']

    def __init__(self):
        """
        """
        self._default: str | None = None
        self._labels: list[str|int] = []
        self._number: list[int] = []
        self._title: list[str] = []
        self._type: list = []
    #
    def __setitem__(self, shape_name: str | int,
                    properties: list[float] | dict[str, float] | str) -> None:
        """
        """
        try:
            self._labels.index(shape_name)
            raise Exception(f'Section {shape_name} already exist')
        except ValueError:
            #
            shape_type = properties[0]
            properties = get_sect_properties(properties[1:])
            #
            self._labels.append(shape_name)
            self._type.append(shape_type)
            #mnumber = next(self.get_number())
            #self._number.append(mnumber)            
            #
            if re.match(r"\b(i((\_)?beam|section)?|w|m|s|hp|ub|uc|he|ipe|pg)\b",
                        shape_type, re.IGNORECASE):
                # [d, tw, bf, tf, bfb, tfb, r, title]
                properties =  get_Isection(properties)
                self._ibeam[shape_name] = properties

            elif re.match(r"\b(t(ee)?)\b", shape_type, re.IGNORECASE):
                self._tee[shape_name] = properties                  

            elif re.match(r"\b(tub(ular)?|pipe|chs)\b", shape_type, re.IGNORECASE):
                self._tubular[shape_name] = properties

            elif re.match(r"\b((solid|bar(\_)?)?rectangle|trapeziod|circular|round)\b",
                          shape_type, re.IGNORECASE):
                self._solid[shape_name] = [shape_type, *properties] 

            elif re.match(r"\b(b(ox)?|rhs|shs)\b", shape_type, re.IGNORECASE):
                self._box[shape_name] = properties 

            elif re.match(r"\b(c(hannel)?)\b", shape_type, re.IGNORECASE):
                self._channel[shape_name] = properties                 

            elif re.match(r"\b(l|angle)\b", shape_type, re.IGNORECASE):
                self._angle[shape_name] = properties               

            else:
                raise Exception(" section item {:} not recognized".format(shape_type))    
    #
    def __getitem__(self, shape_name: int):
        """
        node_name : node number
        """
        try:
            index = self._labels.index(shape_name)
            shape_type = self._type[index]
        except ValueError:
            raise KeyError(f'   *** Section {shape_name} does not exist')
        #
        if re.match(r"\b(tub(ular)?|pipe)\b", shape_type, re.IGNORECASE):
            return self._tubular[shape_name]

        elif re.match(r"\b((solid|bar(\_)?)?rectangle|trapeziod|circular|round)\b", shape_type, re.IGNORECASE):
            return self._solid[shape_name]
        
        elif re.match(r"\b(i((\_)?beam|section)?|w|m|s|hp|ub|uc|he|ipe|pg)\b", shape_type, re.IGNORECASE):
            return self._ibeam[shape_name]
        
        elif re.match(r"\b(b(ox)?|rhs|shs)\b", shape_type, re.IGNORECASE):
            return self._box[shape_name]
        
        elif re.match(r"\b(c(hannel)?)\b", shape_type, re.IGNORECASE):
            return self._channel[shape_name]
        
        elif re.match(r"\b(t(ee)?)\b", shape_type, re.IGNORECASE):
            return self._tee[shape_name]
        
        elif re.match(r"\b(l|angle)\b", shape_type, re.IGNORECASE):
            return self._angle[shape_name]
        
        else:
            raise IOError(f' Section type {shape_type} not recognised')
    #    
    #
    def __len__(self):
        return len(self._labels)

    def __iter__(self):
        return iter(self._labels)

    def __contains__(self, value):
        return value in self._labels
    #
    def __delitem__(self, shape_name: str | int) -> None:
        try:
            _nodes_empty = []
            _index = self._labels.index(shape_name)
            1/0
        except ValueError:
            raise KeyError(f'    *** warning section {shape_name} does not exist')
    #
    def properties(self):
        """
        """
        return self._properties()    
    #
    def get_number(self, start: int = 1):
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
    @property
    def df(self):
        """ """
        db = DBframework()
        #header = ['number', 'name', 'title', 'type', 
        #          'diameter', 'wall_thickness',
        #          'height', 'web_thickness',
        #          'top_flange_width', 'top_flange_thickness',
        #          'bottom_flange_width', 'bottom_flange_thickness',
        #          'fillet_radius',
        #          'SA_inplane', 'SA_outplane',
        #          'shear_stress', 'build', 'compactness']
        #
        number = [x + 1 for x, item in enumerate(self._labels)]
        #
        datadf = {'number': number,
                  'name' : self._labels,
                  'title' : self._title,
                  'type': self._type, 
                  'diameter': [None for item in self._labels],
                  'wall_thickness': [None for item in self._labels],
                  'height': [None for item in self._labels],
                  'web_thickness': [None for item in self._labels],
                  'top_flange_width': [None for item in self._labels],
                  'top_flange_thickness': [None for item in self._labels],
                  'bottom_flange_width': [None for item in self._labels],
                  'bottom_flange_thickness': [None for item in self._labels],
                  'fillet_radius': [None for item in self._labels],
                  'SA_inplane': [1.0 for item in self._labels],
                  'SA_outplane': [1.0 for item in self._labels],
                  'shear_stress': ['maximum' for item in self._labels],
                  'build': ['welded' for item in self._labels],
                  'compactness': [None for item in self._labels]}        
        #
        #sec_df = db.DataFrame(data)
        #
        #for row in sec_df.itertuples():
        #    section = self.__getitem__(shape_name=row.name)
        #    data = section._data_df()
        #    sec_df[data.keys()].iloc[row.Index] =  data.values()
        #    #for key, item in data.items():
        #    #    sec_df[key].iloc[row.Index] = item
        #
        for idx, name in enumerate(self._labels):
            section = self.__getitem__(shape_name=name)
            data = section._data_df()
            for key, item in data.items():
                datadf[key][idx] = item
        #
        return db.DataFrame(datadf)
#
#
#-------------------------------------------------
#
