# 
# Copyright (c) 2009-2023 fem2ufo
#
from __future__ import annotations
# Python stdlib imports
#from collections.abc import Mapping
from dataclasses import dataclass
from typing import NamedTuple
import itertools as it
#
# package imports
from steelpy.trave.beam.main import BeamBasic
from steelpy.trave.postprocessor.main import Results
from steelpy.utils.math.operations import zeros, to_matrix, mtxmul, linspace
from steelpy.utils.dataframe.main import DBframework
#
import numpy as np
#
#
#
# -----------------------------------------------------------
#
# -----------------------------------------------------------
#
def max_bandwidth(elements,  jbc):
    """
    calculate max bandwidth
    ------------------------  
    npi : connectivity end 1
    npj : connectivity end 2
    jbc : nodes freedom
    nel: number of elements
    if we
    npi ,npj, jbc, nel
    """
    ibndm4 = [0]
    for key, element in elements.items():
        idof, jdof = element.DoF
        bc1 = jbc[idof]
        bc2 = jbc[jdof]
        ieqn = bc1 + bc2
        try:
            ibndm4.append(max([abs(ieqn1 - ieqn2)
                               for x, ieqn1 in enumerate(ieqn) if ieqn1 > 0
                               for ieqn2 in ieqn[x+1:] if ieqn2 > 0]))
        except ValueError:
            continue
    #
    return max(ibndm4) + 1
#
def bd_condition(nodes, boundaries):
    """
    set boundary conditions
    bcs: set default = 0 free (1 fix)
    """
    nnp = len(nodes)
    jbc = zeros(nnp, 6, code='I')
    supports = boundaries.supports()
    #for node_name, bd in boundaries.node.items():
    for node_name, bd in supports.items():
        ind = nodes[bd.node].index
        jbc[ind] = list(bd[:6])
    return jbc
#
def spclbc(elements, nodes, free_nodes, jbc):
    """
    Impose condition for  special GLOBAL shapes
    bcs: set default = 0 free (1 fix)
    """
    # free_nodes = elements.get_free_nodes()
    #
    #for _element in elements.values():
    #for element in elements.iter_elements:
    for key, element in elements.items():
        conn = element.connectivity
        pos_node = set(conn) - set(free_nodes)
        for node_name in pos_node:
            ind = nodes[node_name].index
            #ind
            # jbc[ind][3:] = [1, 1, 1]
            # jbc[ind][3] = 1
            # jbc[ind][4] = 1
        # if _element.type == "truss":
        #    
        #    # end 1
        #    ind = self._nodes._labels.index(conn[0])
        #    jbc[ind][3:] = [0, 0, 0]
        #    # end 2
        #    ind = self._nodes._labels.index(conn[1])
        #    jbc[ind][3:] = [0, 0, 0]
    return jbc
#
def shape_cond(elements, nodes, boundaries, free_nodes):
    """
    jcs: modify default = 0 free (1 fix)
    """
    jbc = bd_condition(nodes, boundaries)
    #jbcX = nodes.neq()
    # TODO : check this module
    #jbc = spclbc(elements, nodes, free_nodes, jbc)
    #
    # Number the equations  in jbc from 1 up to the order.
    # Start assigning equation numbers for zero dof's
    # from 1 up;  only zero given a number.        
    #
    jbc = list(it.chain.from_iterable(jbc))
    counter = it.count(start=1)
    jbc = [next(counter) if _item == 0 else 0
           for _item in jbc]
    neq = max(jbc)
    jbc = to_matrix(jbc, 6)
    return jbc, neq
#
def get_bandwidth(elements, nodes, boundaries):
    """ """
    #jbcc, neq = shape_cond(elements=elements,
    #                       nodes=nodes,
    #                       boundaries=boundaries,
    #                       free_nodes=free_nodes)
    #
    jbc, neq = nodes.neq(supports=boundaries._nodes)
    #
    iband = max_bandwidth(elements=elements, jbc=jbc)
    return jbc, neq, iband
#
#
