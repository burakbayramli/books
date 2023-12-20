# 
# Copyright (c) 2009-2023 steelpy
#
from __future__ import annotations
# Python stdlib imports
#from collections import defaultdict
#import multiprocessing
#import pickle
#from dataclasses import dataclass

# package imports
#from steelpy.f2uModel.mesh.main import MeshPlane
# steelpy.trave3D
#from .preprocessor.mass import form_mass
from .processor.dynamic import eigen, trnsient
from .processor.static import StaticSolver
#from .postprocessor.main import Results
#
from .postprocessor.main import PostProcess
#
from .beam.main import Beam
#
#
#
#
#
#
class TraveItem:
    """
    A program for static & dynamic analysis
    of 3-d framed structures
    """
    __slots__ = ['_mesh', '_load', '_f2u', '_results', 
                 '_postprocess', '_plane2D']
    
    def __init__(self, log: bool = False) -> None:
        """
        """
        plane = "3D"
        if self._plane2D:
            plane = "2D"
        #
        print (f"-- module : Trave{plane} version 2.50")
        print ('{:}'.format(52 * '-'))
        #
        #self._postprocess = None
    #
    @property
    def mesh(self):
        """
        """
        return self._mesh
    
    @mesh.setter
    def mesh(self, mesh):
        """
        """
        self._mesh = mesh
        self._mesh.plane(self._plane2D)
        #
        self._postprocess = PostProcess(mesh=self._mesh)
    #
    #
    #
    def static(self, method:str|None=None,
               second_order: bool = False):
        """
        Solves the static system by the Direct Stiffness Method (DSM)
        
        method : banded, frontal
        second_order : Second order (True/False)
        """
        #
        # ------------------------------
        # Get K matrix
        # ------------------------------
        mesh = self._mesh
        K = mesh.K(solver=method) 
        jbc = mesh.jbc()
        #
        # ------------------------------
        # Get load vector
        # ------------------------------        
        # 
        load =  mesh.load()
        basic_load = load.basic()
        df_nload = basic_load.node_df()
        #
        # ------------------------------
        # Static solution
        # ------------------------------
        #        
        static = StaticSolver(plane=mesh._plane,
                              method=method)
        #      
        Udf = static.solve(Kg=K, Fn=df_nload, jbc=jbc)
        #
        # -----------------------------------
        # Postprocess
        # -----------------------------------
        #
        self._postprocess.Un(Udf)
        #
        #print('-->')
    #
    def dynamic(self):
        """ """
        pass
    #
    #
    def solve(self, beam_steps: int= 10):
        """ """
        #
        # -------------------------------------
        # Results
        # -------------------------------------        
        #        
        results = self._postprocess.results(beam_steps=beam_steps)
        return results
#
#
class Trave3D(TraveItem):
    """
    A program for static & dynamic analysis of 3D framed structures
    """
    __slots__ = ['_mesh', '_load', '_f2u', '_results', '_plane']
    
    def __init__(self) -> None:
        """
        """
        self._plane2D=False
        super().__init__()
        #
        #print ("-- module : Trave3D version 2.50")
        #print ('{:}'.format(52 * '-'))
        #self._results = Results()        
    #
    #
    def dynamic(self, end_time: float, delta_t: float,
                    type:str|None=None):
        """
        Solves the dynamic system
        
        end_time: simulation time [seconds]
        deltat : time increment [seconds]
        type : modal, time history
        mass : lumped, consistent
        damping : 
        """
        file = open( "stfmx.f2u", "rb" )
        jbc = pickle.load( file )
        stf = pickle.load( file )
        mass =  pickle.load( file )
        file.close()
        #
        ibandm = 1
        #load = []
        npt =  int(end_time // delta_t)
        #
        trnsient(stf, mass, jbc, npt, 
                 #load,
                 #disp, vel, acc, fmag, olddis,
                 #wk, damp, loadin, maxnode,
                 ibandm)        
        #
        print('--')
    #
    def nfreq(self):
        """ """
        # geometry
        elements = self._mesh.elements()
        nodes = self._mesh.nodes()
        boundaries = self._mesh.boundaries()
        # pure python solution
        assemble_banded_Kmatrix(elements, nodes, boundaries)
        #
        mss, ibandm = form_mass(elements, nodes, boundaries)
        eigen(ibandm=ibandm, ivib=2)
        print('--')
    #
    #
#
#
class Trave2D(TraveItem):
    """
    A program for static & dynamic analysis of 2D framed structures
    """
    __slots__ = ['_mesh', '_load', '_f2u', '_results', '_plane']
    
    def __init__(self) -> None:
        """
        """
        self._plane2D=True
        super().__init__()
        #print ("-- module : Trave2D version 2.50")
        #print ('{:}'.format(52 * '-'))
        #self._results = Results()
    #
#    
#
#
#
    
    
    