#
# Copyright (c) 2009-2023 fem2ufo
#
from __future__ import annotations
# Python stdlib imports
from dataclasses import dataclass
#from typing import NamedTuple
#import pickle
#from typing import NamedTuple

#
# package imports
from steelpy.trave.postprocessor.operations import ElementProcess
#from .output import  Node, Beam
#from steelpy.utils.dataframe.main import DBframework

#
#
#
# --------------------
# Results
# --------------------
#
#
class PostProcess:
    __slots__ = ['_mesh', '_process', '_Un']
    
    def __init__(self, mesh) -> None:
        """
        """
        self._mesh = mesh
        #self._plane = self._mesh._plane
        #elements = self._mesh.elements()
        #boundaries = self._mesh.boundaries()
        self._process = ElementProcess(mesh=mesh)
    #
    #def mesh(self, mesh):
    #    """ """
    #    self._mesh = mesh
    #    self._plane = self._mesh._plane
    #
    def Un(self, Udf):
        """ """
        self._Un = Udf
    #
    #
    def results(self, beam_steps):
        """ """
        res = self._process.results(Un=self._Un,
                                    beam_steps=beam_steps)
        #print('-->')
        return res