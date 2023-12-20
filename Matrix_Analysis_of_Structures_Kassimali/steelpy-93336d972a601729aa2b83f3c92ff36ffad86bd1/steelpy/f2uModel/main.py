# 
# Copyright (c) 2009 steelpy
#
# Python stdlib imports
from __future__ import annotations
#from dataclasses import dataclass
#
# package imports
# steelpy.f2uModel
from .mesh.main import Mesh
from .concept.main import Concepts
from .properties.main import Properties
#from .process.meshing import Meshing
#from .plot.main import PlotModel
# 
#from steelpy.sections.main import Sections
#from steelpy.material.main import Materials
#
#from steelpy.material.process.operations import get_isomat_prop_df
#
def get_number(items: dict) -> int:
    """
    return maximum consecutive number of items
    """
    _number = [item.number for item in items.values()]
    try:
        return max(_number)
    except ValueError:
        return 0


#
class f2uModel:
    """
    FE Geometry model class
    
    Parameters:
      :number: integer
      :name: string
      :nodes: list of node's class
      :elements: list of element's class
      :materials: list of material's class
      :sets: list of groups (elements, nodes, etc)
      :sections: list of section's class
      :vectors: list of guide points
      :eccentricities: list of eccentricities
      :joints: list of joint's class
      :hinges: list of hinges definitios
      :loads: list of load's class
      :data: FE model data
      :units: FE model units
      :soil: list of soil's class
      :hydrodynamics: hydrodynamic's data
      :boundaries: list of FE model boundary
    
    Parameters:  
      :number:  integer internal number 
      :name:  string node external name
    """
    __slots__ = ['component', 'type', 'data',
                 'db_file', '_plot', 'mesh_type',
                 '_properties', # '_materials', '_sections', 
                 '_mesh', '_concept']
                 # '_nodes', '_meshing', '_boundaries',  'sets', '_load', '_results',

    def __init__(self, component:str|int|None = None) -> None:
        """
        mesh_type : sqlite/inmemory
        """
        print("-- module : fem2ufo Version 5.00dev")
        print('{:}'.format(52*'-'))
        #
        if not component:
            component = "f2u_model"
        self.component = component
        self.type: str = 'substructure'
        #self.mesh_type:str = 'sqlite'
        #
        # set main components
        #mesh_type2 = 'sqlite'
        #self._materials = Materials(mesh_type=self.mesh_type,
        #                            db_file=self.db_file)

        #self._sections = Sections(mesh_type=self.mesh_type,
        #                          db_file=self.db_file)
        #
        self._mesh:dict = {}
        #self._mesh = Mesh(materials=self._materials,
        #                  sections=self._sections,
        #                  mesh_type=self.mesh_type,
        #                  db_file=self.db_file)
        #
        self._properties = Properties()
        #
        self._concept:Concepts|None = None
        #self._concept_flag = False
    #
    # -------------------
    #
    #def materials(self, values:None|list=None,
    #              df=None):
    #    """
    #    """
    #    if isinstance(values, list):
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
    #
    #def sections(self, values:None|list=None,
    #             df=None):
    #    """
    #    """
    #    #if values:
    #    if isinstance(values, list):
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
    def properties(self):
        """
        """
        return self._properties
    #
    # -------------------
    #
    def concept(self, name:str|None = None):
        """
        """
        if not name:
            name = self.component
        #
        self._concept = Concepts(name=name, 
                                 #materials=self._materials,
                                 #sections=self._sections,
                                 properties= self._properties)
        return self._concept

    #
    #
    def mesh(self, name:str|None = None,
             sql_file:str|None = None):
        """ """
        if not name:
            name = self.component
        
        self._mesh[name] = Mesh(db_name=name,
                                sql_file=sql_file)

        return self._mesh[name]
    #
    # -------------------
    #
    def build(self, name:str|None = None) -> None:
        """
        """
        #
        if not name:
            name = self.component        
        #
        #self._sections.get_properties()
        #
        if self._concept:
            self._mesh[name] = self._concept.mesh()
        #    meshing = Meshing(concept=self._concept,
        #                      component=name, 
        #                      mesh_type=self.mesh_type)
        #    self._mesh[name] = meshing.get_mesh()
        #    self._mesh[name].renumbering()
        #    #mesh._load._basic.FER()
        #    #return mesh
        #    #_sql.write_concept(self._concept)
        #
        # check wave load case
        #
        for key, item in self._mesh.items():
            item.build()
            #item._load._basic.wave_process()
            # TODO : remove second _load for simplification
            #item._load._basic.FER(elements= item._elements)        
        #
        #
        print('end meshing')
        return self._mesh[name]
    #
    #@property
    #def plot(self):
    #    """ """
    #    return self._plot
#
#
#
#