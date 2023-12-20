#
# Copyright (c) 2009-2023 fem2ufo
#

# Python stdlib imports
from __future__ import annotations

# package imports
#steelpy.f2uModel.mesh
#from steelpy.f2uModel.mesh.process.matrix.Kassemble import assemble_banded_Kmatrix
#
from steelpy.f2uModel.mesh.process.operations import assemble_matrix
#

def Kmatrix(elements, jbc, neq, ndof: int, solver: str|None = None):
    """ """
    
    # Banded matrix
    #if solver == 'banded':
    #    iband = elements.max_bandwidth(jbc=jbc)
    #    aa =  assemble_banded_Kmatrix(elements=elements , jbc=jbc,
    #                                  neq=neq, iband=iband)
    #else:
    # numpy matrix
    #aa = assemble_Kmatrix_np(elements=elements, jbc=jbc, neq=neq, plane=plane)
    aa = assemble_matrix(elements=elements, jbc=jbc, neq=neq, ndof=ndof, mitem="K")
    #
    return aa
#
def Mmatrix(elements, jbc, neq, ndof: int, solver: str|None = None):
    """ """
    aa = assemble_matrix(elements=elements, jbc=jbc, neq=neq, ndof=ndof, mitem="M")
    #
    return aa    
#
#
#
def Gmatrix(elements, jbc, neq, plane, solver: str|None = None):
    """ """
    aa = assemble_matrix(elements=elements, jbc=jbc, neq=neq, plane=plane, mitem="Kg")
    #
    return aa    
#
#