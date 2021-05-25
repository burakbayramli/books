"""
    QUAD_PROCESSING
    It converts the quad into a triangular mesh, ie., the idea is to
    flatten all the physical domain into a triangular mesh, so we can
    then apply all the algorithms developed for a triangular mesh

    The processing consists of several steps:
    1) flatten the quad into an array of triangles, keeping the
       original indexing of the quad nodes qp. For example, if
         quad[i] = np.array([47, 93, 7, 523], dtype=int)
       this quad is flattened into two triangles, say, qtri[n] and
       qtri[n+1], with nodes:
            qtri[n] = np.array([47,93,7])
            qtri[n+1] = np.array[7,523,47])
        (i.e., both ccw)
    2) Next in the boundary between the triangular mesh and the quad
       mesh we have N physical nodes that are identified by two different
       sets of indexes. In the triangular mesh, these nodes are, by
       construction:
          int_bnodes[0:N] = [0,1,2,...,(N-)]
       with physical coordinates given by
          p[int_bnodes] = p[0], p[1], ..., p[N-1]

       The corresponding nodes in the quad domain can be taken directly
       from the qp array, which is a N*M array, whose last N nodes are
       identical (same position in the physical space) to the N nodes of
       int_bnodes
       The indices of these nodes in the quad space range from
       (N*M - N) to N*M -1
       For example,
           p[int_bnodes[0] = qp[N*M -N]
           p[int_bnodes[-1]] = qp[N*M-1]
       So, when we see a triangle, say, tri = np.array([34,35,477]],
       abutted to the boundary between the triangles and quadrilateral
       domains,34 and 35 are the indices of the boundary nodes in the
       triangular physical space
       The corresponding nodes in the quadrilateral physical space
       will be: (N*M-N+34) and (N*M-N+35], ie., for N=58, M=10,
       556 and 557. This means that
       p[34] = qp[556] and p[35] = qp[557]
       Summarizing the quad nodes with indexes (N*M-N) to (N*M-1) will
       have to be relabed in the flattened mesh as indexes 0 to (N-1)
    3) Now we have to deal with the rest of the qp indexes that go from
       0 to (N*M-N-1). If the length of the triangular array of nodes,
       is K = len(p), i.e., p[0], p[1], ... p[K-1], then
          a) quad node 0 will have to be relabed as node in the triangular
             (flattened) mesh of index K
          b) quad node 1 will be relabed as node index K+1
          c) ... quad node (N*M-N-1) will be relabed as (K + N*M-N-1)
    4) Once we now the new indexing of the quad nodes in the flattened
       triangular domain, we have to do a simultaneous global relabeling
       of all the indexes in the qtri array.
"""

import numpy as np

class FLATTEN:
    """    
    Takes the quad and qp arrays, flattens the quad array into triangles
    and returns two arrays: new_p (with the coordinates of all the nodes
    in the physical domain) and new_tri (with all the triangles in the
    physical domain)
    """
    def __init__(self,quad,qp,int_bnodes,tri,p):
        self.quad, self.qp = quad, qp
        self.int_bnodes = int_bnodes
        self.tri,self.p = tri, p
    def __call__(self):
        quad, qp = self.quad, self.qp
        int_bnodes = self.int_bnodes
        tri,p = self.tri, self.p

        def calc_area(pa,pb,pc):
            x0 = pa[0]; y0 = pa[1]
            x1 = pb[0]; y1 = pb[1]
            x2 = pc[0]; y2 = pc[1]
            area = 0.5*((x1 - x0)*(y2 - y0) - (y1 - y0)*(x2 - x0))
            return area
            
        # --------------------------------------------------
        #    convert the quad mesh into a triangular mesh
        # --------------------------------------------------
        n = 0
        qtri = np.zeros((2*len(quad),3),dtype=int)
        for q in quad:
            a1 = np.array([q[0],q[1],q[2]],dtype=int)
            a2 = np.array([q[2],q[3],q[0]],dtype=int)
            a3 = np.array([q[1],q[2],q[3]],dtype=int)
            a4 = np.array([q[3],q[0],q[1]],dtype=int)
            # choose the couple with best shape
            # calculate the area of each triangle
            area1 = calc_area(qp[q[0]],qp[q[1]],qp[q[2]])
            area2 = calc_area(qp[q[2]],qp[q[3]],qp[q[0]])
            area3 = calc_area(qp[q[1]],qp[q[2]],qp[q[3]])
            area4 = calc_area(qp[q[3]],qp[q[0]],qp[q[1]])
            # estimate partition quality (the smaller the number the better)
            quality1 = np.abs(area1 - area2)/np.abs(area1 + area2)
            quality2 = np.abs(area3 - area4)/np.abs(area3 + area4)
            if quality1 < quality2:
                qtri[n] = a1
                qtri[n+1] = a2
            else:
                qtri[n] = a3
                qtri[n+1] = a4
            n = n+2
        # -------------------------------------------------        
        #      compute relabel (for quad node relabeling)
        # -------------------------------------------------
        """
        relabel: an array relabel[i], where is is the node number of the
        quadrangular nodes qp[i], i = 0,1,2,...(N*M-1) and relabel[i] is
        the new index that replaces i in the flattened mesh. For example,
        relabel[0] = K, relabel[1] = K+1, where K = len(p), the length
        of the node array of the triangular mesh. However, the last N
        numbers of the relabel array are:
            relabel[-N] = int_bnodes[0] = 0
            relabel[-N+1] = int_bnodes[1] = 1
            relabel[-1] = int_bnodes[-1] = (N-1)
        """
        N = len(int_bnodes) # num of nodes per layer
        M = len(qp)/N       # num of layers
        K = len(p)
        relabel = np.arange(N*M)
        relabel = relabel + K
        # last N nodes of qp correspond to int_bnodes
        relabel[-N:] = int_bnodes        
        # ------------------------------------------------
        #       relabel the nodes of qtri
        # ------------------------------------------------
        tri_to_add = np.zeros((len(qtri),3),dtype=int)
        tri_to_add[:,0] = relabel[qtri[:,0]]
        tri_to_add[:,1] = relabel[qtri[:,1]]
        tri_to_add[:,2] = relabel[qtri[:,2]]

        new_tri = np.concatenate((tri,tri_to_add))
        # ------------------------------------------------------------
        #   relabel the indexes of the qp nodes by concatenating them
        #   to the q nodes
        # ------------------------------------------------------------
        p_to_add = qp[0:-N]
        new_p = np.concatenate((p,p_to_add))        

        return new_p, new_tri
