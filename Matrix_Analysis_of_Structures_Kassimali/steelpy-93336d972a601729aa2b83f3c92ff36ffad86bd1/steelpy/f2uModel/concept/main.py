# 
# Copyright (c) 2009 steelpy
# 
from __future__ import annotations
# Python stdlib imports


# package imports
from steelpy.f2uModel.process.main import BasicModel
from ..concept.joint import Connection
from ..concept.boundary import BoundaryConcept
from ..load.main import ConceptLoad
from ..process.meshing import Meshing
from ..concept.elements.sets import Groups
#
from steelpy.sections.main import Sections
from steelpy.material.main import Materials
#
#from .elements.beam import ConceptBeam
from .process.geometry import Releases
from .elements.points import NodesIM
from .elements.main import ConceptElements

from steelpy.f2uModel.plot.main import PlotConcept


class Concepts(BasicModel):
    """
    Structural elements
    beam
    truss
    shell
    solid
    cable
    pile
    
    Utilities
    connection
    mass
    intersection
    spring
    
    Contact
    """
    __slots__ = ['joints', '_beams', 'pile', '_nodes',
                 '_materials', '_sections', '_load',
                 'shells', 'membranes', 'solids', 'springs',
                 '_hinges', '_boundaries', '_properties',
                 '_name', '_groups', '_elements'] # 'data_type', 
    
    def __init__(self, name:str, properties): # load,
        """
        """
        #
        #super().__init__()
        #
        self._name = name
        #self.data_type = mesh_type
        #
        #self._materials = materials
        #self._sections = sections
        #
        self._materials = Materials(mesh_type="inmemory")

        self._sections = Sections(mesh_type="inmemory")        
        #
        # Points
        self._nodes = NodesIM()
        # 
        self.joints = Connection(points=self._nodes)
        #
        self._hinges = Releases()
        #
        #self._boundaries = ConceptBoundaries()
        self._boundaries = BoundaryConcept(points=self._nodes)
        #
        #self._beams = ConceptBeam(element_type="beam", points=self._points,
        #                          materials=materials, sections=sections)
        #
        #
        self._elements = ConceptElements(points=self._nodes,
                                         materials=self._materials,
                                         sections=self._sections)
        #
        # groups
        self._groups = Groups()
        #
        #self.truss = Elements(element_type='truss', points = self.points,
        #                      materials=mesh.materials, sections=mesh.sections)
        #
        #self.piles = Beams(beam_type='pile', 
        #                   points=self.points, elements=mesh.elements,
        #                   materials=mesh.materials, sections=mesh.sections, 
        #                   properties=properties,hinges=self._hinges)
        #
        #self.shells = Shells(shell_type='shell',
        #                     points=self.points, materials=mesh.materials)
        #
        #self.springs = Springs(spring_type='spring', 
        #                       points=self.points, materials=mesh.materials)
        #
        #self._load = load
        self._load = ConceptLoad(points=self._nodes,
                                 elements=self._elements,
                                 boundaries=self._boundaries) # self._load,
    #
    #
    # --------------------
    # Common
    # --------------------
    #    
    #def materials(self, values: None|list|dict=None,
    #              df=None):
    #    """
    #    """
    #    if isinstance(values, list):
    #        1/0
    #        if isinstance(values[0], list):
    #            for item in values:
    #                self._materials[item[0]] = item[1:]
    #        else:
    #            self._materials[values[0]] = values[1:]
    #    #
    #    try:
    #        df.columns
    #        self._materials.df = df
    #    except AttributeError:
    #        pass
    #    #
    #    return self._materials
    ##
    ##
    #def sections(self, values: None|list|dict=None,
    #             df=None):
    #    """
    #    """
    #    if isinstance(values, list):
    #        1/0
    #        if isinstance(values[0], list):
    #            for item in values:
    #                self._sections[item[0]] = item[1:]
    #        else:
    #            self._sections[values[0]] = values[1:]
    #    #
    #    # dataframe input
    #    try:
    #        df.columns
    #        self._sections.df = df
    #    except AttributeError:
    #        pass
    #    #
    #    return self._sections
    #
    # --------------------
    # Elements
    # --------------------
    #
    def points(self, values: None|list|dict=None,
                 df=None):
        return self._nodes
    #
    #def beams(self, values: None|list|dict = None,
    #          df=None):
    #    """ """
    #    if values:
    #        1/0
    #        if isinstance(values, list):
    #            1/0
    #        else:
    #            raise IOError('beam input not valid')
    #    #
    #    try:
    #        df.columns
    #        self._beams.df = df
    #    except AttributeError:
    #        pass
    #
    #    return self._beams
    #
    def boundaries(self, values: None|list|dict = None,
                   df = None):
        """
        """
        if values:
            if isinstance(values, list):
                1/0
                for item in values:
                    self._boundaries[item[0]] = item[1:]
            else:
                raise IOError('boundary input not valid')
        #
        try:
            df.columns
            self._boundaries.df(df)
        except AttributeError:
            pass
            
        return self._boundaries
    #
    #def groups(self):
    #    """
    #    """
    #    return self._groups
    #    
    #
    #def elements(self):
    #    """ """
    #    return self._elements
    #
    #
    #@property
    #def get_name(self):
    #    """
    #    """
    #    return self._name
    #
    #
    #def __delattr__(self, name:str) -> None:
    #    """
    #    """
    #    _joints = []
    #    if 'piles' in name:
    #        _tobe_deleted = [key for key in self.piles.keys()]
    #        for _item in _tobe_deleted:
    #            for _element in self.piles[_item].elements:
    #                _joints.extend(_element.connectivity)
    #            del self.piles[_item]
    #    #
    #    # deleting redundant link joints
    #    _joints = set(_joints)
    #    for _number in _joints:
    #        try:
    #            del self.joints[_number]
    #        except KeyError:
    #            print('-->', _number)
    #
    #
    # --------------------
    # Loading
    # --------------------
    #    
    #
    #def load(self, values: None|list|dict=None,
    #             df=None):
    #    """ """
    #    if values:
    #        1/0
    #
    #    try:
    #        df.columns
    #        self._load.df = df
    #    except AttributeError:
    #        pass
    #
    #    return self._load
    #
    #def __iter__(self):
        #"""
        #"""
        #for _name in self.beams.keys():
        #    yield self.beams[_name]
        #
        #for _name in self.trusses.keys():
        #    yield self.trusses[_name]
        #
        #for _name in self.piles.keys():
        #    yield self.piles[_name]
    #
    #
    # --------------------
    # Operations
    # --------------------
    #    
    #
    def mesh(self):
        """ Meshing"""
        self._sections.get_properties()
        #
        meshing = Meshing(concept=self,
                          component=self._name)
        mesh = meshing.get_mesh()
        mesh.renumbering()
        mesh.build()        
        return mesh
    #
    # --------------------
    # Plotting
    # --------------------
    #
    def plot(self, figsize:tuple = (10, 10)):
        """ """
        #print('--')
        return PlotConcept(cls=self, figsize=figsize)
    #
    #    
