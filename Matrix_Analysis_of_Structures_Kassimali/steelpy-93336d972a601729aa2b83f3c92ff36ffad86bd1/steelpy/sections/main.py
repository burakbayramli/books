# 
# Copyright (c) 2019-2023 fem2ufo
#
# Python stdlib imports
from __future__ import annotations
from collections.abc import Mapping
import re
#
# package imports
#
from .inmemory.main import SectionIM
from .sqlite.main import SectionSQL
#from .process.operations import SectionBasic


# ---------------------------------
#
# ---------------------------------
#
class Sections(Mapping):
    __slots__ = ['_sections', '_default']

    def __init__(self, mesh_type: str = 'inmemory',
                 db_file: str | None = None):
        """
        """
        if mesh_type != "inmemory":
            self._sections = SectionSQL(db_file=db_file,
                                        db_system=mesh_type)
        else:
            self._sections = SectionIM()

    #
    def __setitem__(self, shape_name: str | int,
                    properties: str|list) -> None:
        """
        """
        self._sections[shape_name] = properties
    
    #
    def __getitem__(self, shape_name: str | int):
        """
        """
        try:
            return self._sections[shape_name]
        except KeyError:
            raise Exception(f" section name {shape_name} not found")    

    #
    def __str__(self, units: str = "si") -> str:
        """ """
        unit_sec = " m"
        unit_mas = "kg/m"
        space = " "
        #
        output = "\n"
        output += "{:}\n".format(80 * "_")
        output += "\n"
        output += f"{30 * space}SECTION PROPERTIES REPORT{15 * space}UNITS [{unit_sec}]\n"
        output += "\n"
        output += f"{48 * space}Web{14 * space}Flanges\n"
        output += (f"Member Name{5 * space}Type{6 * space}Diametre{3 * space}Thickness"
                   f"{2 * space}Height{5 * space}Top Width{2 * space}Bot Width\n")
        output += f"{48 * space}Thickness{2 * space}Top Thick{2 * space}Bot Thick\n"
        output += f"{70 * space}Fillet\n"
        output += "{:}\n".format(80 * ".")
        output += "\n"
        for name, section in self._sections.items():
            output += "{:<15s} ".format(str(name))
            output += section._dimension()
        #
        output += "\n"
        output += "{:}\n".format(80 * "_")
        output += "\n"
        output += "                               SECTION DERIVED PROPERTIES\n"
        output += "{:}\n".format(80 * "_")
        output += "\n"
        output += (f"{15 * space}Area[{unit_sec}^2] Ixx [{unit_sec}^4] Iyy [{unit_sec}^4]"
                   f" Yp    [{unit_sec}] rx    [{unit_sec}] J   [{unit_sec}^4]\n")
        output += (f"{26 * space}Sxx [{unit_sec}^3] Syy [{unit_sec}^3] SCeny [{unit_sec}]"
                   f" ry    [{unit_sec}] Cw  [{unit_sec}^6]\n")
        output += f"{26 * space}Zxx [{unit_sec}^3] Zyy [{unit_sec}^3] SCenx [{unit_sec}] Mass[{unit_mas}]\n"
        output += "{:}\n".format(80 * ".")
        output += "\n"
        for name, section in self._sections.items():
            output += "{:<14s} ".format(str(name))
            prop = section.properties()
            output += prop.__str__()
        # print("-->")
        return output

    #
    #
    @property
    def default(self):
        """ """
        return self._sections._default

    @default.setter
    def default(self, shape_name: int|str):
        """ """
        try:
            self._sections[shape_name]
        except KeyError:
            raise IOError(f'section {shape_name} missing')
        self._sections._default = shape_name

    #
    def get_properties(self):
        """
        """
        summary = {}
        for key, item in self._sections.items():
            item.push_property()
            # item.properties
            # summary[key] = item._get_properties()
        # return summary
    #
    #
    def tubular(self, values:None|list=None,
                df=None):
        """Tubular section"""
        if values:
            if isinstance(values, list):
                #self._sections.tubular
                print('-->')
                1/0
            else:
                raise IOError('material input not valid')
        #
        # dataframe input
        try:
            df.columns
            df = df.drop_duplicates(['name'], keep='first')
            #
            # shape_type = properties[0]
            # properties = get_sect_properties(properties[1:])
            #
            self._sections._labels.extend(df.name.tolist())
            self._sections._title.extend(['NULL' for _ in df.name])
            self._sections._number.extend([next(self._sections.get_number())
                                           for _ in df.name])
            self._sections._type.extend(df.type.tolist())
            #
            self._sections._tubular.df = df
        except AttributeError:
            pass

        #print("here")
        return self._sections._tubular
    #
    #
    #
    def __getitem__(self, shape_name: str | int):
        """
        """
        try:
            return self._sections[shape_name]
        except KeyError:
            raise Exception(f" Section name {shape_name} not found")

    #
    def __delitem__(self, shape_name: str | int) -> None:
        del self._sections[shape_name]

    def __len__(self):
        return len(self._sections)

    def __iter__(self):
        return iter(self._sections)

    def __contains__(self, value):
        return value in self._sections

    #    
    #
    ##
    #def push_df(self, values):
    #    """ """
    #    for item in values:
    #        try:
    #            self._sections[item[0]] = item[1:]
    #        except IOError:
    #            continue
    #
    #
    @property
    def df(self):
        """ raw data for dataframe"""
        #from steelpy.process.dataframe.dframe import DataFrame
        #df = DataFrame()
        #
        #1/0
        #title = []
        #for key, item in self._sections.items():
        #    title.append(item.type)
        #    key
        #
        return self._sections.df

    @df.setter
    def df(self, df):
        """ """
        # TODO: define if drop
        #df = df.drop_duplicates(['name'], keep='first')
        group = df.groupby("type", sort=False)
        for shape_type, section in group:
            if re.match(r"\b(i((\_)?beam|section)?|w|m|s|hp|ub|uc|he|ipe|pg)\b",
                        shape_type, re.IGNORECASE):
                # Bottom Flange
                try:
                    section['bottom_flange_width'] =  section.apply(lambda x: x['top_flange_width']
                                                                    if x['bottom_flange_width']== ""
                                                                    else x['bottom_flange_width'], axis=1)
                except KeyError:
                    section['bottom_flange_width'] = section['top_flange_width']
                #
                try:
                    section['bottom_flange_thickness'] =  section.apply(lambda x: x['top_flange_thickness']
                                                                    if x['bottom_flange_thickness']== ""
                                                                    else x['bottom_flange_thickness'], axis=1)
                except KeyError:
                    section['bottom_flange_thickness'] = section['top_flange_thickness']                
                #
                try:
                    section['fillet_radius'] =  section.apply(lambda x: float(0.0)
                                                                    if x['fillet_radius']== ""
                                                                    else x['fillet_radius'], axis=1)
                except KeyError:
                    section['fillet_radius'] = float(0.0)                
                #
                self._sections._ibeam.df= section
            
            elif re.match(r"\b(t(ee)?)\b", shape_type, re.IGNORECASE):
                self._sections._tee.df= section
            
            elif re.match(r"\b(tub(ular)?|pipe|chs)\b", shape_type, re.IGNORECASE):
                self._sections._tubular.df= section
            
            elif re.match(r"\b((solid|bar(\_)?)?rectangle|trapeziod|circular|round)\b",
                           shape_type, re.IGNORECASE):
                self._sections._solid.df= section
            
            elif re.match(r"\b(b(ox)?|rhs|shs)\b", shape_type, re.IGNORECASE):
                self._sections._box.df= section
            
            elif re.match(r"\b(c(hannel)?)\b", shape_type, re.IGNORECASE):
                self._sections._channel.df= section
            
            elif re.match(r"\b(l|angle)\b", shape_type, re.IGNORECASE):
                self._sections._angle.df= section
            
            else:
                raise Exception(" section item {:} not recognized".format(shape_type))
        #
        self._sections._type.extend(df['type'].tolist())
        self._sections._labels.extend(df['name'].tolist())
        for item in df['name']:
            mnumber = next(self._sections.get_number())
            self._sections._number.append(mnumber)
            self._sections._title.append(None)
        #
        #print('-->')
    #    if section.items():
    #        tub = section[["name", "type", "d", "tw"]]
    #        self.push_df(tub.values)
    #        #
    #    # section = group.get_group("ub")
    #    # section = group.get_group("box")
    #
    #@df.setter
    #def df(self, df):
        #""" """
        #
        #section_number = [next(self.get_number()) for _ in df.name]
        #
        #self._labels.extend(material_number)
        #self._number.extend(material_number)
        #self._grade.extend([-1 for _ in df.name])
        # Fill values
        #self._title.extend(df.name.tolist())
        #1 / 0
    #
    #
    def get_item_by_number(self, shape_name: str | int):
        """
        """
        _items = {_item.number: key
                  for key, _item in self._sections.items()}
        try:
            _name = _items[shape_name]
            return self.__getitem__(_name)
        except KeyError:
            raise KeyError('Invalid section number')
    #    
#    
# ---------------------------------
#

#
#
#
