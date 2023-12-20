# 
# Copyright (c) 2009 steelpy
#

# Python stdlib imports
from __future__ import annotations
from collections.abc import Mapping
import re
#

# package imports
from .process.operations import find_mat_type, get_isomat_prop #, get_isomat_prop_df
from .sqlite.isotropic import MaterialSQL
from .inmemory.isotropic import MaterialIM
from steelpy.utils.dataframe.main import DBframework

#
#
#
class Materials(Mapping):
    """This module stores material classes.
       -------------------------------
       Nonlinear materials:
       - Nonlinear elastic material
       - Bilinear elastoplastic material
         Yield criterion:
          - von Mises
          - Treca
          - Morh-Coulomb
          - Drucker-Prager
         Hardering rules:
          - Isotropic
          - Kinematic
          - Isotropic + Kinematic
       - Multilinear plastic material
       - Rigid-plastic material
       
       -----------------------------
    """
    __slots__ = ['_material']

    def __init__(self, mesh_type: str = 'inmemory',
                 db_file: str | None = None) -> None:
        """
        """
        if mesh_type != "inmemory":
            self._material = MaterialSQL(db_file=db_file,
                                         db_system=mesh_type)
        else:
            self._material = MaterialIM()

    #
    def __setitem__(self, material_name: str | int,
                    properties: list[float] | dict[str, float] | str) -> None:
        """
        [name, elastic, Fy, Fu, E, G, Poisson, density, alpha, title(optional)]
        """
        # get material type
        if isinstance(properties, str):
            material_type = find_mat_type(properties)
            properties = []
        else:
            material_type = find_mat_type(properties[0])
            properties = properties[1:]
        #
        # set material default plastic
        if re.match(r"\b(curve)\b", material_type, re.IGNORECASE):
            raise NotImplementedError('--> Mat type not yet implemented')
        elif re.match(r"\b(elastic|linear(\_elastic)?)\b", material_type, re.IGNORECASE):
            properties = get_isomat_prop(properties)
        else:
            raise IOError(' material type {:} not recognised'
                          .format(material_type))
        #
        self._material[material_name] = [material_type, *properties]

    #
    def __getitem__(self, material_name: str):
        """
        """
        return self._material[material_name]

    #
    @property
    def default(self):
        """ """
        return self._material.default

    @default.setter
    def default(self, material_name):
        """ """
        try:
            self._material[material_name]
        except KeyError:
            raise IOError(f'material {material_name} missing')

        self._material.default = material_name

    #
    def __len__(self) -> float:
        return len(self._material)

    def __iter__(self):
        """
        """
        return iter(self._material)

    def __contains__(self, value) -> bool:
        return value in self._material

    #
    def __str__(self, units: str = "si") -> str:
        """ """
        stress = "N/mm2"
        density = "kg/m3"
        space = " "
        #
        output = "\n"
        output += "{:}\n".format(80 * "_")
        output += "\n"
        output += f"{33 * space}MATERIAL PROPERTIES\n"
        output += "\n"
        output += (f"Member ID      Fy [{stress}] Fu [{stress}] E  [{stress}] "
                   f"G  [{stress}] Poisson    Rho[{density}]\n")
        output += "\n"
        output += "{:}\n".format(80 * ".")
        output += "\n"
        for name, mat in self._material.items():
            output += "{:<14s} ".format(str(name))
            output += mat.__str__()
            # output += "{:<14s} {:1.4E} {:1.4E} {:1.4E} {:1.4E} {:1.4E} {:1.4E}\n"\
            #    .format(str(name), mat.Fy.value, mat.Fu.value, mat.E.value,
            #            mat.G.value, mat.poisson, mat.density.value)
        #
        return output

    #
    #
    @property
    def elastic(self):
        """
        Linear elastic material
        """
        return self._material.elastic
    
    @property
    def linear(self):
        """
        Linear elastic material
        """
        return self._material.elastic
    #
    @property
    def df(self):
        """Data frame format"""
        #df = DBframework()
        #matlin = self._material._elastic._getdata()
        #return df(matlin)
        return self._material._elastic.df
    #
    @df.setter
    def df(self, df):
        """ """
        try:
            df.columns
            group = df.groupby("type")
            #
            # Elastic type
            try:
                elastic = group.get_group("elastic")
                # df = df.drop_duplicates(['name'], keep='first')
                self._material.elastic(df=elastic)
            except KeyError:
                # nonlin = group.get_group("plastic")
                raise IOError('Material type not valid')
        except AttributeError:
            raise IOError('Material type not valid')
        #print('--')
#
#
