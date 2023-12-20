# 
# Copyright (c) 2009-2023 steelpy
#
from __future__ import annotations
#
# Python stdlib imports
import time
#from multiprocessing import Process, Manager

# package imports
from steelpy.utils.math.operations import remove_column_row
import numpy as np

#
#
def form_matrix(elements, nn:int, ndof: int, mitem: str):
    """
    Global system stiffness matrix 
    
    elements : 
    nn  : node number
    ndof : node degree of freedom
    :return
    Ka : global stiffness matrix
    """
    #nodal degrees of freedom per node 
    ndof = ndof
    Ka = np.zeros((nn*ndof, nn*ndof), dtype=np.float64)
    for key, element in elements.items():
        # TODO : check applicable to all element type
        #keg = element.K
        keg = getattr(element, mitem)
        idof, jdof = element.DoF
        # node and corresponding dof (start, finish), used to define the
        # elements of the system stiffness and force matrices
        niqi, niqj = idof*ndof, idof*ndof + ndof
        njqi, njqj = jdof*ndof, jdof*ndof + ndof
        # assemble global stiffness matrix, quadrant 1 to 4
        Ka[niqi:niqj, niqi:niqj] += keg[:ndof, :ndof]             # 2nd
        Ka[niqi:niqj, njqi:njqj] += keg[:ndof, ndof:2*ndof]       # 1st
        Ka[njqi:njqj, niqi:niqj] += keg[ndof:2*ndof, :ndof]       # 3rd
        Ka[njqi:njqj, njqi:njqj] += keg[ndof:2*ndof, ndof:2*ndof] # 4th
    return Ka
#
#
def assemble_matrix(elements, jbc, neq, ndof: int, mitem: str):
    """
    Asseable the element matrices
    -------------------------------------------------
    Ka : stiffness matrix
    jbc : nodes freedom
    ndof : node degree of freedom
    mitem : matrix item
    """
    print(f"** Processing Global [{mitem}] Matrix")
    start_time = time.time()
    #
    nn = len(jbc)
    Ka = form_matrix(elements, nn, ndof=ndof, mitem=mitem)
    #
    jbcc = jbc.stack().values
    index = list(reversed([i for i, jbitem in enumerate(jbcc)
                           if jbitem == 0]))
    #
    for i in index:
        Ka = remove_column_row(Ka, i, i)
    #
    uptime = time.time() - start_time
    print(f"** [{mitem}] assembly Finish Time: {uptime:1.4e} sec")
    return Ka
#
#