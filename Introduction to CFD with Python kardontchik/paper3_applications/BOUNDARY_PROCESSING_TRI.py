"""
   Creates the triangular MESH for non-rectangular domains and provides
   info needed by the solver when using  general boundary conditions
   (transmissive, reflective and forced)
"""

import numpy as np
import matplotlib.pylab as plt

from DISTMESH import *
from Meshing_Tools import *

from Impose_BC import *


class BOUNDARY_PROCESSING_TRI:
    def __init__(self,p,tri,bbars,boundary_nodes,boundary_bars,BC_points,BC_type):
        self.p,self.tri,self.bbars = p, tri, bbars
        self.boundary_nodes,self.boundary_bars = boundary_nodes,boundary_bars
        self.BC_points, self.BC_type = BC_points, BC_type
    def __call__(self):
        p, tri, bbars = self.p,self.tri,self.bbars
        boundary_nodes,boundary_bars = self.boundary_nodes,self.boundary_bars
        BC_points, BC_type = self.BC_points, self.BC_type

        # boundary_nodes: ordered boundary nodes (beginning from Left/Bottom corner
        #                 and going ccw)
        # boundary_bars: ordered boundary bars (to which the ghost cells will be
        #                attached), beginning from the same Left/Bottom corner

        # calculate the area of each triangle in the physical domain, the
        # lenght of its three edges and the angles of the three normals to
        # the edges (pointing towards outside the triangle)
        geo = GEOMETRY(p,tri)
        area, e01,e12,e20,ang01,ang12,ang20 = geo()
        # return the cosine and sine of the normal to the boundary_bars (pointing
        # towards the ghost triangles) (needed when reflective BC exist)
        bcos,bsin = boundary_angles(p,boundary_nodes)

        # -----------------------------------------------------------
        #   Add Ghost Triangles to define the Boundary Conditions
        # -----------------------------------------------------------

        # finds the triangles that are on the boundary (btri), the ghost
        # triangles (tghost). The ghost triangles add more nodes to the
        # computational space, so len(comp_p) > len(p), and the computational
        # space includes also the ghost triangles, so len(comp_tri) > len(tri)

        gboundary = GHOSTS(p,tri,bbars,e01,e12,e20,ang01,ang12,ang20)

        btri,tghost,comp_p,comp_tri = gboundary()

        # Notice that the node array 'comp_p' now includes also the additional
        # nodes of the ghost triangles added to the boundary

        # reorder tghost so that the ghost triangle attached to boundary_bars[i]
        # is tghost[i]. Accordingly, reorder comp_tri (that includes the ghosts)

        tghost,comp_tri = reorder_tghost(boundary_bars,tghost,comp_tri)

        matched_pairs = matching_pairs(bbars,comp_tri)

        # reorder the matched_pairs so that matched_pairs[i,0] = i-th ghost triangle

        matched_pairs = reorder_matched_pairs(boundary_bars,tghost,tri,matched_pairs)

        # now boundary_bars, tghost and matched_pairs are aligned: the boundary_bars[i]
        # has the ghost triangle tghost[i] attached to it.
        # matched_pairs[i,0] gives the number of the thhost[i] in the array comp_tri
        # matched_pairs[i,1] gives the number of the physical triangle (in the array
        # comp_tri) that is abutted to tghost[i]

        # Note: len(matched_pairs) = len(boundary_bars) = len(tghost)
        #    if matched_pairs[i] = [43,12], this means that the comp_tri[43]
        #    (which is also the tghost[i]) is the ghost triangle abutted to the
        #    comp_tri[12] triangle inside the physical space (that is also tri[12])
        #    and their shared boundary bar is boundary_bars[i]

        neighbors = vecinos(bbars,comp_tri)

        # Ex, suppose neighbors[i] = [j, k, m]. This means that the neighbors
        # of the triangle tri[i] in the physical domain are comp_tri[j], comp_tri[k] 
        # and comp_tri[m]. Some of these neighbors may be ghost triangles

        plot_ghosts(comp_p,tghost)

        # --------------------------------------------------------
        #           IMPOSE the BC (tag the ghost cells)
        # --------------------------------------------------------
        # tag the ghost cells with the type of BC
        tghost_BC = impose_BC(BC_points,BC_type,p,tri,boundary_nodes,boundary_bars,tghost)

        # Example:
        # if tghost_BC[i] = 1, then the ghost triangle tghost[i] will
        # have reflective boundary conditions. This could be the case
        # if the fluid is inviscid and tghost[i] is part of a wall

        # ------------------- END OF IMPOSE the BC -----------------

        # ----------------------------------------------------------
        #       Find the matched pairs (tghost/tri) for the BC
        # ----------------------------------------------------------
        trans_ind = np.array(np.where(tghost_BC == 0))[0]
        refle_ind = np.array(np.where(tghost_BC == 1))[0]
        force_ind = np.array(np.where(tghost_BC == 2))[0]

        trans_matched_pairs = np.zeros((len(trans_ind),2),dtype=int)
        refle_matched_pairs = np.zeros((len(refle_ind),2),dtype=int)
        force_matched_pairs = np.zeros((len(force_ind),2),dtype=int)

        trans_matched_pairs = matched_pairs[trans_ind]
        refle_matched_pairs = matched_pairs[refle_ind]
        force_matched_pairs = matched_pairs[force_ind]

        refle_bcos = bcos[np.where(tghost_BC==1)]
        refle_bsin = bsin[np.where(tghost_BC==1)]



        return tri,p,neighbors,matched_pairs,area,e01,e12,e20,ang01,ang12,ang20, \
               trans_matched_pairs,refle_matched_pairs,force_matched_pairs, \
                  refle_bcos,refle_bsin


