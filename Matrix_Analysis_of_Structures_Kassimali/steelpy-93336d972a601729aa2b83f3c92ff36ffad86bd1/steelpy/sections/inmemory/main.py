#
# Copyright (c) 2019-2023 steelpy
#

# Python stdlib imports
from __future__ import annotations
#from collections.abc import Mapping
#

# package imports
from .angle import Angle
from .tubular import TubularIM
from .tee import Tee
from .channel import Channel
from .box import Box
from .ibeam import Ibeam
from .solid import SolidSection
#from ..process.operations import get_sect_properties
from .operations import SectionBasic
#from steelpy.utils.dataframe.main import DBframework

# ---------------------------------
#
#
#
class SectionIM(SectionBasic):
    __slots__ = ['_labels', '_number', '_title', '_type',
                 '_tubular', '_solid', '_ibeam', '_box',
                 '_channel', '_tee', '_angle', '_default']

    def __init__(self):
        """
        """
        super().__init__()
        #
        self._tubular = TubularIM()
        self._solid = SolidSection()
        self._ibeam = Ibeam()
        self._box = Box()
        self._channel = Channel()
        self._tee = Tee()
        self._angle = Angle()
    #
    def __setitem__(self, shape_name: str | int,
                    properties: list[float] | dict[str, float] | str) -> None:
        """
        """
        super().__setitem__(shape_name, properties)
        mnumber = next(self.get_number())
        self._number.append(mnumber)
        self._title.append('NULL')
    #
    #
    # def get_properties(self):
    #    """
    #    """
    #    summary = {}
    #    for key, item in self._sections.items():
    #        summary[key] = item._get_properties()
    #    return summary
    #
    #
    #@property
    #def tubular(self, shape_name:int|str):
    #    """ """
    #    return self._sections[shape_name]
    #
    #@tubular.setter
    #def tubualar(self, shape_name:int|str, properties):
    #    """ """
    #    self._sections[shape_name] = TubularBasic(name=shape_name,
    #                                              d=properties[0], t=properties[1])
    #
    #
    #@property
    #def df(self):
    #    """ """
    #    db = DBframework()
    #    #        
    #    print('mat df fixme')
    #    1 / 0
#
