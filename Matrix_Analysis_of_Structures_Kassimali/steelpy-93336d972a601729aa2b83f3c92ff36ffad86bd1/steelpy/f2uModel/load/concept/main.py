# 
# Copyright (c) 2009 steelpy
#
# Python stdlib imports
from __future__ import annotations
from collections.abc import Mapping
#
#
# package imports
# steelpy.f2uModel.load
from steelpy.f2uModel.load.process.actions import SelfWeight
from steelpy.f2uModel.load.process.basic_load import BasicLoadBasic
#
from steelpy.f2uModel.load.concept.beam import BeamLoadItemIM
from steelpy.f2uModel.load.concept.node import NodeLoadItemIM
from steelpy.f2uModel.load.concept.timehistory import TimeHistory
from steelpy.f2uModel.load.concept.combination import LoadCombConcept
from steelpy.f2uModel.load.concept.wave_load import WaveLoadItemIM
#
#
#
class BasicLoadConcept(BasicLoadBasic):
    __slots__ = ['_load', '_labels', '_title', '_number',
                 'f2u_points', 'f2u_elements']

    def __init__(self, points, elements):
        """
        """
        super().__init__()
        #
        self._load: dict = {}
        # FIXME: reduce dependency
        self.f2u_points = points
        self.f2u_elements= elements
    #
    def __setitem__(self, load_name: int, load_title: str) -> None:
        """
        load_name :
        load_title :
        """
        try:
            self._labels.index(load_name)
            self._title.index(load_title)
            raise Warning("Basic Load title {:} already defined".format(load_title))
        except ValueError:
            self._labels.append(load_name)
            self._title.append(load_title)
            # TODO: fix numbering
            load_number = len(self._load) + 1
            self._load[load_name] = LoadTypesConcept(name=load_name,
                                                     number=load_number,
                                                     title=load_title,
                                                     points=self.f2u_points,
                                                     beams=self.f2u_elements._beams)
            self._number.append(load_number)

    def __getitem__(self, load_name: str|int):
        """
        """
        try:
            return self._load[load_name]
        except KeyError:
            raise IOError("load case not defined")
    #
    #def __contains__(self, value) -> bool:
    #    return value in self._labels

    #def __len__(self) -> float:
    #    return len(self._labels)

    #def __iter__(self):
    #    """
    #    """
    #    items = list(set(self._labels))
    #    return iter(items)
    #
    def __delitem__(self, load_name: str|int):
        """
        """
        del self._load[load_name]
#
#
class LoadTypesConcept:
    """
    """
    __slots__ = ['_node', '_node_id', '_beam', '_beam_id',
                 '_selfweight', '_line', '_line_id', '_wave',
                  'name', 'number', 'title', 'f2u_points', 'f2u_beams']

    def __init__(self, name: str|int, number: int, title: str,
                 points, beams):
        """
        """
        self.f2u_points = points
        self.f2u_beams = beams
        #
        self.name = name
        self.number = number
        self.title = title
        #
        self._selfweight = SelfWeight()
        
        self._node = NodeLoadItemIM(load_name=self.name,
                                    load_title=self.title,
                                    nodes=self.f2u_points)
        
        self._beam = BeamLoadItemIM(load_name=self.name,
                                    load_title=self.title,
                                    beams=self.f2u_beams)
        #
        self._wave = WaveLoadItemIM(load_name=self.name)

    #
    @property
    def gravity(self):
        """
        The self weight form allows you to specify multipliers to
        acceleration due to gravity (g) in the X, Y, and Z axes.
        If switched on, the default self weight acts in the Y axis
        with a magnitude and sign of -1."""
        return self._selfweight
    
    @gravity.setter
    def gravity(self, values):
        """ """
        self._selfweight[self.name] = [*values, self.title]
    #
    #
    @property
    def points(self):
        """ return all points"""
        return self._node
    #
    @property
    def point(self):
        """ return current point"""
        return self._node[self._node_id]

    @point.setter
    def point(self, values):
        """ set point"""
        # set connectivity
        try:
            node_id = self.f2u_points.get_point_name(values)
        except IOError:
            node_id = self.f2u_points.get_new_point(values)
        self._node_id = node_id
    #
    #
    def line(self):
        """ """
        return self._line[self._line_id]
    #
    def line(self, values):
        """ """
        pass
    #
    @property
    def beams(self):
        """ return all beam"""
        return self._beam
    #
    @property
    def beam(self):
        """ return current beam"""
        return self._beam[self._beam_id]

    @beam.setter
    def beam(self, values):
        """ """
        #if isinstance(values[0], list):
        #    for value in values:
        #        self._beam[value[ 0 ] ] = value[ 1: ]
        #else:
        #    self._beam[ values[ 0 ] ] = values[ 1: ]
        if isinstance(values, str):
            try:
                self.f2u_beams[values]
                self._beam_id = values
            except IndexError:
                1 / 0
                raise IOError(f'Beam {values} not found')
            
        else:
            self._beam_id = values.name
    #
    #
    @property
    def wave(self):
        """ """
        return self._wave
    
    @wave.setter
    def wave(self, wave_data):
        """
        design_load : max_BSOTM
        wave_load=None, 
        """
        design_load: str = 'max_BS'
        criterion: str = 'local'
        
        if isinstance(wave_data, (str, int)):
            wave_data = [wave_data, design_load, criterion, self.title]
        
        elif isinstance(wave_data, (list, tuple)):
            try:
                wave_data.extend([criterion, self.title])
            except AttributeError:
                wave_data = wave_data + (criterion, self.title, )
        
        elif isinstance(wave_data, dict):
            raise NotImplementedError
        
        else:
            raise IOError('wave data not valid')
        #
        self._wave[self.name] = wave_data
#
#
#class TimeHistoryConcept(Mapping):
#    __slots__ = ['_load', '_labels', '_title', '_number', 'f2u_points']
#
#