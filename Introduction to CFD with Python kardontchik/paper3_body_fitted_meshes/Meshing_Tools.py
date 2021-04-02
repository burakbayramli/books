"""
   Meshing_Tools
"""
import numpy as np
from numpy import pi as pi
from scipy.spatial import Delaunay
import matplotlib.pylab as plt

#from DISTMESH import *

def unique_rows(data):
    unique = dict()
    for row in data:
        row = tuple(row)
        if row in unique:
            unique[row] += 1
        else:
            unique[row] = 1
    data = np.asarray(unique.keys())
    return data

def ktrimesh(p,bars,pflag=0):
    # create the (x,y) data for the plot
    xx1 = p[bars[:,0],0]; yy1 = p[bars[:,0],1]
    xx2 = p[bars[:,1],0]; yy2 = p[bars[:,1],1]
    xmin = np.min(p[:,0])
    xmax = np.max(p[:,0])
    ymin = np.min(p[:,1])
    ymax = np.max(p[:,1])
    xmin = xmin - 0.05*(xmax - xmin)
    xmax = xmax + 0.05*(xmax - xmin)
    ymin = ymin - 0.05*(ymax - ymin)
    ymax = ymax + 0.05*(ymax - ymin)

    plt.figure()
    for i in range(len(xx1)):
        xp = np.array([xx1[i],xx2[i]])
        yp = np.array([yy1[i],yy2[i]])
        plt.plot(xmin,ymin,'.',xmax,ymax,'.',markersize=0.1)
        plt.plot(xp,yp,'k')
        plt.hold('on')
    plt.axis('equal')
    if pflag == 0:
        stitle = 'Triangular Mesh'
    if pflag == 1:
        stitle = 'Visual Boundary Integrity Check'
    #plt.title('Triangular Mesh')
    plt.title(stitle)
    plt.xlabel('x')
    plt.ylabel('y')
    plt.hold('off')
    plt.show()
    return 1

def ccw_tri(p,t):
    """
    orients all the triangles counterclockwise
    """
    # vector A from vertex 0 to vertex 1
    # vector B from vertex 0 to vertex 2
    A01x = p[t[:,1],0] - p[t[:,0],0]
    A01y = p[t[:,1],1] - p[t[:,0],1]
    B02x = p[t[:,2],0] - p[t[:,0],0]
    B02y = p[t[:,2],1] - p[t[:,0],1]
    # if vertex 2 lies to the left of vector A the component z of
    # their vectorial product A^B is positive
    Cz = A01x*B02y - A01y*B02x
    a = t[np.where(Cz<0)]
    b = t[np.where(Cz>=0)]
    a[:,[1,2]] = a[:,[2,1]]
    t = np.concatenate((a, b))
    return t

def triqual_flag(p,t):
    # a(1,0), b(2,0), c(2,1)
    a = np.sqrt((p[t[:,1],0] - p[t[:,0],0])**2 + (p[t[:,1],1] - p[t[:,0],1])**2)
    b = np.sqrt((p[t[:,2],0] - p[t[:,0],0])**2 + (p[t[:,2],1] - p[t[:,0],1])**2)
    c = np.sqrt((p[t[:,2],0] - p[t[:,1],0])**2 + (p[t[:,2],1] - p[t[:,1],1])**2)
    A = 0.25*np.sqrt((a+b+c)*(b+c-a)*(a+c-b)*(a+b-c))
    R = 0.25*(a*b*c)/A
    r = 0.5*np.sqrt( (a+b-c)*(b+c-a)*(a+c-b)/(a+b+c) )
    q = 2.0*(r/R)
    min_edge = np.minimum(np.minimum(a,b),c)
    min_angle_deg = (180.0/np.pi)*np.arcsin(0.5*min_edge/R)
    
    min_q = np.min(q)
    min_ang = np.min(min_angle_deg)
    return min_q, min_ang

def triqual(p,t,fh,qlim=0.2):
    # a(1,0), b(2,0), c(2,1)
    a = np.sqrt((p[t[:,1],0] - p[t[:,0],0])**2 + (p[t[:,1],1] - p[t[:,0],1])**2)
    b = np.sqrt((p[t[:,2],0] - p[t[:,0],0])**2 + (p[t[:,2],1] - p[t[:,0],1])**2)
    c = np.sqrt((p[t[:,2],0] - p[t[:,1],0])**2 + (p[t[:,2],1] - p[t[:,1],1])**2)
    A = 0.25*np.sqrt((a+b+c)*(b+c-a)*(a+c-b)*(a+b-c))
    R = 0.25*(a*b*c)/A
    r = 0.5*np.sqrt( (a+b-c)*(b+c-a)*(a+c-b)/(a+b+c) )
    q = 2.0*(r/R)
    pmid = (p[t[:,0]] + p[t[:,1]] + p[t[:,2]])/3.0
    hmid = fh(pmid)
    Ah = A/hmid
    Anorm = Ah/np.mean(Ah)
    min_edge = np.minimum(np.minimum(a,b),c)
    min_angle_deg = (180.0/np.pi)*np.arcsin(0.5*min_edge/R)
    
    plt.figure()
    plt.subplot(3,1,1)
    plt.hist(q)
    plt.title('Histogram;Triangle Statistics:q-factor,Minimum Angle and Area')
    plt.subplot(3,1,2)
    plt.hist(min_angle_deg)
    plt.ylabel('Number of Triangles')
    plt.subplot(3,1,3)
    plt.hist(Anorm)
    plt.xlabel('Note: for equilateral triangles q = 1 and angle = 60 deg')
    plt.show()

    # detailed data on worst offenders
    indq = np.where(q < qlim)  # indq is a tuple: len(indq) = 1
    # to get the number of elements in the tuple do: len(indq[0])
    # to get the element i in this tuple do: indq[0][i]
    if list(indq[0]) != []:
        print 'List of triangles with q < %5.3f and the (x,y) location of their nodes' % qlim
        print ''
        print 'q     t[i]      t[nodes]         [x,y][0]       [x,y][1]       [x,y][2]'
        for i in indq[0]:
            print '%.2f  %4d  [%4d,%4d,%4d]     [%+.2f,%+.2f]  [%+.2f,%+.2f]  [%+.2f,%+.2f]' % \
              (q[i],i,t[i,0],t[i,1],t[i,2],p[t[i,0],0],p[t[i,0],1],p[t[i,1],0],p[t[i,1],1],p[t[i,2],0],p[t[i,2],1])
        print ''
        # end of detailed data on worst offenders    
    return q,min_angle_deg,Anorm


class GEOMETRY:
    """
        Returns the area of each triangle. For each triangle it
        returns also the length of its three edges (in ccw order:
        first edge (e01) from node 0 to node 1, 2nd edge (e12) from
        node 1 to node 2 and 3rd edge (e20) from node 2 to node 0.
        It also returns the vectors normal to the three egdes
        (vectors pointing out of the triangle). The normals are given
        in terms of the angle they make with the positive x-axis:
        ang01, ang12 and ang20 (in radians). See Toro, p579, Fig 16.14
        Instantiate and use as follows:
            geo = GEOMETRY(p,tri)
            area,e01,e12,e20,ang01,ang12,ang20 = geo() 
        Note:
            n0[:,0] = vertex 0 of the triangles in the mesh
            n1[:,1] = vertex 1 of the triangles in the mesh
            n2[:,2] = vertex 2 of the triangles in the mesh
    """
    def __init__(self,p,tri):
        self.p, self.tri = p, tri
    def __call__(self):
        p, tri = self.p, self.tri
        n0 = p[tri[:,0]]
        n1 = p[tri[:,1]]
        n2 = p[tri[:,2]]
        # coordinates of the vertices
        x0 = n0[:,0]; y0 = n0[:,1]
        x1 = n1[:,0]; y1 = n1[:,1]
        x2 = n2[:,0]; y2 = n2[:,1]
        # area
        area = 0.5*((x1 - x0)*(y2 - y0) - (y1 - y0)*(x2 - x0))
        # edges' length
        e01 = np.sqrt((x1-x0)**2 + (y1-y0)**2)
        e12 = np.sqrt((x2-x1)**2 + (y2-y1)**2)
        e20 = np.sqrt((x0-x2)**2 + (y0-y2)**2)
        # angles of the corresponding normals
        # edges' slopes
        # np.arctan2(y-coor,x-coor);  -pi < ang < +pi
        ang01 = np.arctan2(x0-x1,y1-y0)
        ang12 = np.arctan2(x1-x2,y2-y1)
        ang20 = np.arctan2(x2-x0,y0-y2)
        return area,e01,e12,e20,ang01,ang12,ang20

def boundary_info(p,bars):
    """
    If Bflag = 1, it returns a 1D array of nodes ('boundary_nodes') and
    a 2D array of bars ('boundary').
    The first element in 'boundary_nodes' identifies the node (by its
    number) that has the minimum x-coordinate. If there are several nodes
    in the boundary with minimum x, it will choose the node from this set
    the node with minimum y-coordinate. This will be the 'reference node'
    of the set of nodes on the boundary, easy to find out visually in the
    boundary plot. Following this reference node the elements in the
    'boundary_nodes' array appear in the same order as they appear on
    the boundary plot as one follows the perimeter of the boundary in a
    ccw direction, defined as the direction in which we see all the nodes
    inside the mesh at our left.
    Likewise, the first element in the 2D array 'boundary' defines the
    first bar in the boundary (using a pair of nodes). The order of the
    bars follows the same order used by the 'boundary_nodes' array.

    If Bflag=2, it returns two 1D arrays of nodes ('ext_bound_nodes' and
    'int_bound_nodes') and two 2D arrays of bars ('ext_bound' and
    'int_bound'). The first set, 'ext_bound_nodes' and 'ext_bound' refers
    to the external boundary and the second set, 'int_bound_nodes' and
    'int_bound' refers to the internal boundary. In both cases the order
    of the nodes and bars is such that as we go along the boundary we
    see the internal nodes of the mesh towards our left ('ccw' convention)
    """
    bars = bars.tolist()
    
    Bflag = 1  # boundary flag default, indicating only 1 boundary
    bound = [bars[0]]
    del bars[0]

    while bound[0][0] != bound[-1][1]:
        for bar in bars:
            if bar[0] == bound[-1][1]:
                to_remove = bar
                b = [bar[0],bar[1]]       
                bound.append(b)
                break
            if bar[1] == bound[-1][1]:
                to_remove = bar
                b = [bar[1],bar[0]]
                bound.append(b)
                break
        bars.remove(to_remove)

    if len(bars) > 0:
        Bflag = 2  # two boundaries, nodes between boundaries(external flow)
        next_bound = [bars[0]]
        del bars[0]

        while next_bound[0][0] != next_bound[-1][1]:
            for bar in bars:
                if bar[0] == next_bound[-1][1]:
                    to_remove = bar
                    b = [bar[0],bar[1]]       
                    next_bound.append(b)
                    break
                if bar[1] == next_bound[-1][1]:
                    to_remove = bar
                    b = [bar[1],bar[0]]
                    next_bound.append(b)
                    break
            bars.remove(to_remove)
    if len(bars) > 0:
        print 'Error: there are more than 2 boundaries'
        print '       number of bars left out = %4d' % len(bars)

    # ---------------------------------------------------------------------
    # If there is more than one boundary find out which one is external and
    # which one is internal
    # ---------------------------------------------------------------------
    p = np.asarray(p)
    if Bflag == 1:
        bound = np.asarray(bound)
    if Bflag == 2:
        bound = np.asarray(bound)
        next_bound = np.asarray(next_bound)

    # find the node(s) with minimum x-coordinate in bound
    # find the node(s) with minimum x-coordinate in next_bound
    # the one with the minimum x-coord is the external boundary

    # since the nodes in bound appear twice each (once to the left of a bar,
    # the second time to the right of a bar) all the nodes are represented
    # either in p[bound[:,0]] or p[bound[:,1]]


    px_min_bound,py_min_bound = np.amin(p[bound[:,0]],axis=0)
    # px_min_bound = -3.0; py_min_bound = -1.0

    if Bflag == 1:
        boundary = np.copy(bound)
        px_min_boundary = np.copy(px_min_bound)

    if Bflag == 2:
        px_min_next_bound, py_min_next_bound =  \
                           np.amin(p[next_bound[:,0]],axis=0)
        if px_min_bound < px_min_next_bound:
            ext_bound = np.copy(bound)
            int_bound = np.copy(next_bound)
            px_min_ext = px_min_bound
            px_min_int = px_min_next_bound
        else:
            ext_bound = np.copy(next_bound)
            int_bound = np.copy(bound)
            px_min_ext = np.copy(px_min_next_bound)
            px_min_int = np.copy(px_min_bound)

    # -------------------------------------------------------------
    # put the nodes making the boundary in a separate array(s)
    # -------------------------------------------------------------
    if Bflag == 1:
        bpoints = np.reshape(boundary[0::2],2*len(boundary[0::2]))
        if len(boundary) % 2 == 0:   # even
            boundary_nodes = bpoints
        else:
            boundary_nodes = bpoints[0:-1]
    if Bflag == 2:
        bpoints = np.reshape(ext_bound[0::2],2*len(ext_bound[0::2]))
        if len(ext_bound) % 2 == 0:  # even
            ext_bound_nodes = bpoints
        else:
            ext_bound_nodes = bpoints[0:-1]
        bpoints = np.reshape(int_bound[0::2],2*len(int_bound[0::2]))
        if len(int_bound) % 2 == 0:   # even
            int_bound_nodes = bpoints
        else:
            int_bound_nodes = bpoints[0:-1]

    # Note ext_bound_nodes and int_bound_nodes contain the indexes
    # of the nodes on the boundary. To find their coordinates, do
    #   p[ext_bound_nodes] and p[int_bound_nodes]

    # -------------------------------------------------------------
    # Find the reference node (index) for each array of boundary nodes
    # -------------------------------------------------------------
    eps = 0.0001
    if Bflag == 1:
        ind_min_x = np.where(p[boundary_nodes,0] < px_min_boundary + eps)
        nn = np.argmin(p[boundary_nodes[ind_min_x],1])
        ref_ind = ind_min_x[0][nn] 

    if Bflag == 2:
        ind_min_x = np.where(p[ext_bound_nodes,0] < px_min_ext + eps)
        nn = np.argmin(p[ext_bound_nodes[ind_min_x],1])
        ext_ref_ind = ind_min_x[0][nn]
        
        ind_min_x = np.where(p[int_bound_nodes,0] < px_min_int + eps)
        nn = np.argmin(p[int_bound_nodes[ind_min_x],1])
        int_ref_ind = ind_min_x[0][nn]
        
    # --------------------------------------------------------------
    #  roll the boundaries so the ref point is the first point
    #---------------------------------------------------------------
    if Bflag == 1:
        boundary = np.roll(boundary, len(boundary) - ref_ind, axis=0)
        boundary_nodes = np.roll(boundary_nodes, len(boundary_nodes) - ref_ind)
    if Bflag == 2:
        ext_bound = np.roll(ext_bound, len(ext_bound) - ext_ref_ind, axis=0)
        int_bound = np.roll(int_bound, len(int_bound) - int_ref_ind, axis=0)
        ext_bound_nodes = np.roll(ext_bound_nodes,len(ext_bound_nodes) - ext_ref_ind)
        int_bound_nodes = np.roll(int_bound_nodes,len(int_bound_nodes) - int_ref_ind)
        
    # --------------------------------------------------------------
    #  Order the boundary(s) such that as we travel along the boundary(s)
    #  we always see the nodes of the grid to our left ('ccw')
    #  Test of ccw:
    #  For the boundary (Bflag=1) or ext_bound Bflag=2), as we traverse the
    #  periphery of the boundary increasing the index of the node, the total
    #  area enclosed in a complete turn must be negative
    #  For the int_bound the total area enclosed in a complete turn along the
    #  boundary must be positive

    # to change from ccw to cw (or viceversa):
    # 1) swap the columns of the array
    # 2) reverse the order of the elements in the array: 1st becomes last,
    #    2nd becomes next to last, and so on
    # --------------------------------------------------------------
    def kintegrate(y,x):
        area = 0.0
        for i in range(len(x)-1):
            dx = x[i+1] - x[i]
            h = 0.5*(y[i+1] + y[i])
            area += h*dx
        return area

    if Bflag == 1:
        y = np.concatenate((p[boundary_nodes,1],[p[boundary_nodes[0],1]]))
        x = np.concatenate((p[boundary_nodes,0],[p[boundary_nodes[0],0]]))
        area = kintegrate(y,x)
        if area > 0:
            # invert the order
            boundary_nodes = boundary_nodes[::-1]
            boundary_nodes = np.roll(boundary_nodes,1)
            boundary[:,[0,1]] = boundary[:,[1,0]]
            boundary = boundary[::-1]
        return boundary_nodes, boundary
    if Bflag == 2:
        y_ext = np.concatenate((p[ext_bound_nodes,1],[p[ext_bound_nodes[0],1]]))
        x_ext = np.concatenate((p[ext_bound_nodes,0],[p[ext_bound_nodes[0],0]]))
        y_int = np.concatenate((p[int_bound_nodes,1],[p[int_bound_nodes[0],1]]))
        x_int = np.concatenate((p[int_bound_nodes,0],[p[int_bound_nodes[0],0]]))
        area_ext = kintegrate(y_ext,x_ext)
        area_int = kintegrate(y_int,x_int)
        if area_ext > 0:
            # invert the order
            ext_bound_nodes = ext_bound_nodes[::-1]
            ext_bound_nodes = np.roll(ext_bound_nodes,1)
            ext_bound[:,[0,1]] = ext_bound[:,[1,0]]
            ext_bound = ext_bound[::-1]
            
        if area_int < 0:
            # invert the order
            int_bound_nodes = int_bound_nodes[::-1]
            int_bound_nodes = np.roll(int_bound_nodes,1)
            int_bound[:,[0,1]] = int_bound[:,[1,0]]
            int_bound = int_bound[::-1]
        return ext_bound_nodes,ext_bound,int_bound_nodes,int_bound


class GHOSTS:
    """
    Generates and returns the following info about the boundary:
    1) btri: the triangles in the mesh abutted to the boundary
    2) tghost: generates and returns the ghost triangles around
       the boundary. These ghost triangles will be used to define
       the Boundary Conditions: transmissive, reflective or forced.
    4) comp_tri: the union (or concatenation) of the triangles in
       the physical space('tri') and the ghost triangles ('tghost')
       added at the end of the array.
       The computational mesh ('comp_tri') creates a uniform
       environment for the triangles in the physical space ('tri'):
       now each triangle in 'tri' is surrounded by 3 neighboring
       triangles. If a triangle is abutted to the boundary, 1 or 2
       of the neighboring triangles may be a ghost triangle.
       This environment standardizes the computation and updating
       of the physical variables (U) and the fluxes (F)
    5) comp_p: the enlarged array of nodes, including the new nodes
       belonging to the ghost triangles (added at the end of the array)
    Notice about indexing:
       bbars[i]: i-th boundary bar
       btri[i]: triangle in 'tri' abutted to bbars[i]
       tghost[i]: ghost triangle abutted to bbars[i]
    """
    def __init__(self,p,tri,bbars,e01,e12,e20,ang01,ang12,ang20):
        self.p, self.tri, self.bbars = p, tri, bbars
        self.e01, self.e12, self.e20 = e01, e12, e20
        self.ang01, self.ang12, self.ang20 = ang01, ang12, ang20

    def __call__(self):
        p, tri, bbars = self.p, self.tri, self.bbars
        e01, e12, e20 = self.e01, self.e12, self.e20
        ang01, ang12, ang20 = self.ang01, self.ang12, self.ang20
        # ------------------------------------------------------------------
        #       Find the triangles on the boundary of the mesh
        # ------------------------------------------------------------------

        # for each boundary bar in bbars, find the index of the triangle it
        # belongs to. This index enables to extract information about the length
        # of the boundary bar and, most importantly, the direction of the normal
        # to the bar pointing in the direction out of the physical mesh. This
        # direction will enable to define a third node for the ghost triangle
        # abutted to the physical space.

        # find the three edges of every triangle in the mesh
        edge01 = np.sort(tri[:,[0,1]])
        edge12 = np.sort(tri[:,[1,2]])
        edge20 = np.sort(tri[:,[2,0]])

        # info about the normal and bar lenght will be stored here:
        bnormal = np.zeros(len(bbars))
        blen = np.zeros(len(bbars))
        # info about the triangles abutted to the boundary
        btri = np.zeros((len(bbars),3),dtype=int)

        for i in range(len(bbars)):
            # for each boundary bar find the triangle it belongs to by checking if
            # this boundary bar corresponds to one of the edges of the triangle
            ind01 = np.logical_and(edge01[:,0] == bbars[i,0], edge01[:,1] == bbars[i,1])
            ind12 = np.logical_and(edge12[:,0] == bbars[i,0], edge12[:,1] == bbars[i,1])
            ind20 = np.logical_and(edge20[:,0] == bbars[i,0], edge20[:,1] == bbars[i,1])
            if np.any(ind01):
                bnormal[i] = ang01[ind01]
                blen[i] = e01[ind01]
                btri[i] = tri[ind01]
            elif np.any(ind12):              
                bnormal[i] = ang12[ind12]
                blen[i] = e12[ind12]
                btri[i] = tri[ind12]     
            elif np.any(ind20):               
                bnormal[i] = ang20[ind20]
                blen[i] = e20[ind20]
                btri[i] = tri[ind20]         

        # ------------------------------------------------------------------------
        #    For each boundary bar add a ghost triangle abutted to the
        #    corresponding triangle in the mesh.
        # ------------------------------------------------------------------------

        tghost = np.zeros((len(bbars),3),dtype=int)
        comp_p = np.copy(p)
        for i in range(len(bbars)):
            # find midpoint of the boundary bar
            pmid = 0.5*(p[bbars[i,0]] + p[bbars[i,1]])
            # calculate the 3rd node of the ghost triangle, assummed equilateral
            pnode = pmid + blen[i]*np.array([np.cos(bnormal[i]), np.sin(bnormal[i])])
            comp_p = np.concatenate((comp_p,[pnode]))
            tghost[i] = np.array([bbars[i,0],bbars[i,1],len(comp_p)-1])

        tghost = ccw_tri(comp_p,tghost)

        # computational mesh: comp_tri
        comp_tri = np.concatenate((tri,tghost))

        return btri,tghost,comp_p,comp_tri

def matching_pairs(bbars,comp_tri):
    """
    returns: matched_pairs
    if matched_pairs[i] = [43,12], this means that the
    comp_tri[43] is the ghost triangle abutted to the comp_tri[12] triangle
    inside the physical space
    """
    matched_pairs = np.zeros((len(bbars),2),dtype = int)
    num_tghosts = len(bbars)
    num_tri = len(comp_tri) - num_tghosts
    # find the three edges of every triangle inside the physical space
    in_edge01 = np.sort(comp_tri[0:num_tri,[0,1]])
    in_edge12 = np.sort(comp_tri[0:num_tri,[1,2]])
    in_edge20 = np.sort(comp_tri[0:num_tri,[2,0]])
    # find the three edges of every tghost triangle
    out_edge01 = np.sort(comp_tri[num_tri:,[0,1]])
    out_edge12 = np.sort(comp_tri[num_tri:,[1,2]])
    out_edge20 = np.sort(comp_tri[num_tri:,[2,0]])
    for i in range(len(bbars)):
        # for each boundary bar find the inside triangle that shares this bar
        ind01 = np.logical_and(in_edge01[:,0] == bbars[i,0], in_edge01[:,1] == bbars[i,1])
        ind12 = np.logical_and(in_edge12[:,0] == bbars[i,0], in_edge12[:,1] == bbars[i,1])
        ind20 = np.logical_and(in_edge20[:,0] == bbars[i,0], in_edge20[:,1] == bbars[i,1])
        if np.any(ind01):
            ind = np.where(ind01)
            inside = ind[0][0]
        if np.any(ind12):
            ind = np.where(ind12)
            inside = ind[0][0]
        if np.any(ind20):
            ind = np.where(ind20)
            inside = ind[0][0]
        # for each boundary bar find the ghost triangle that shares this bar
        ind01 = np.logical_and(out_edge01[:,0] == bbars[i,0], out_edge01[:,1] == bbars[i,1])
        ind12 = np.logical_and(out_edge12[:,0] == bbars[i,0], out_edge12[:,1] == bbars[i,1])
        ind20 = np.logical_and(out_edge20[:,0] == bbars[i,0], out_edge20[:,1] == bbars[i,1])
        if np.any(ind01):
            ind = np.where(ind01)
            outside = ind[0][0]
        if np.any(ind12):
            ind = np.where(ind12)
            outside = ind[0][0]
        if np.any(ind20):
            ind = np.where(ind20)
            outside = ind[0][0]
        matched_pairs[i,0] = num_tri + outside
        matched_pairs[i,1] = inside
        
    return matched_pairs

def vecinos(bbars,comp_tri):
    """
    Returns: neighbors for all the triangles in the physical space
    Example:
       neighbors[2] = [17, 8, 3]
    Means that the triangle comp_tri[2] (= tri[2]) in the physical
    space has the following neighbors that are abutted to it:
       comp_tri[17], comp_tri[8] and comp_tri[3]
    If comp_tri[2] = [9, 7, 5] then:
       comp_tri[17] shares the edge [9,7] with comp_tri[2]
       comp_tri[8]  shares the edge [7,5] with comp_tri[2]
       comp_tri[3]  shares the edge [5,9] with comp_tri[2]
    Notice that some of the neighbors of the physical triangle
    could be ghost triangles
    """
    num_of_ghosts = len(bbars)
    num_of_tri = len(comp_tri) - num_of_ghosts
    neighbors = np.zeros((num_of_tri,3),dtype=int)
    
    for i in range(num_of_tri):
        # since the abutted triangle is also ccw, the order of the edges must be
        # opposite
        vect = np.array([comp_tri[i,0], comp_tri[i,1]])
        ind01 = np.logical_and(comp_tri[:,0] == vect[1], comp_tri[:,1] == vect[0])
        ind12 = np.logical_and(comp_tri[:,1] == vect[1], comp_tri[:,2] == vect[0])
        ind20 = np.logical_and(comp_tri[:,2] == vect[1], comp_tri[:,0] == vect[0])
        if np.any(ind01):
            ind = np.where(ind01 == True)
            neighbors[i,0] = ind[0][0]
        elif np.any(ind12):
            ind = np.where(ind12 == True)
            neighbors[i,0] = ind[0][0]
        elif np.any(ind20):
            ind = np.where(ind20 == True)
            neighbors[i,0] = ind[0][0]
        vect = np.array([comp_tri[i,1], comp_tri[i,2]])
        ind01 = np.logical_and(comp_tri[:,0] == vect[1], comp_tri[:,1] == vect[0])
        ind12 = np.logical_and(comp_tri[:,1] == vect[1], comp_tri[:,2] == vect[0])
        ind20 = np.logical_and(comp_tri[:,2] == vect[1], comp_tri[:,0] == vect[0])
        if np.any(ind01):
            ind = np.where(ind01 == True)
            neighbors[i,1] = ind[0][0]
        elif np.any(ind12):
            ind = np.where(ind12 == True)
            neighbors[i,1] = ind[0][0]
        elif np.any(ind20):
            ind = np.where(ind20 == True)
            neighbors[i,1] = ind[0][0]            
        vect = np.array([comp_tri[i,2], comp_tri[i,0]])
        ind01 = np.logical_and(comp_tri[:,0] == vect[1], comp_tri[:,1] == vect[0])
        ind12 = np.logical_and(comp_tri[:,1] == vect[1], comp_tri[:,2] == vect[0])
        ind20 = np.logical_and(comp_tri[:,2] == vect[1], comp_tri[:,0] == vect[0])
        if np.any(ind01):
            ind = np.where(ind01 == True)
            neighbors[i,2] = ind[0][0]
        elif np.any(ind12):
            ind = np.where(ind12 == True)
            neighbors[i,2] = ind[0][0]
        elif np.any(ind20):
            ind = np.where(ind20 == True)
            neighbors[i,2] = ind[0][0]

    return neighbors

def reorder_tghost(boundary_bars,tghost,comp_tri):
    # reorder tghost (and comp_tri) so that it matches the order of the
    # boundary_bars
    len_tri = len(comp_tri) - len(tghost)
    ttghost = np.zeros((len(tghost),3),dtype=int)

    for i in range(len(boundary_bars)):
        for j in range(len(tghost)):
            if np.any(tghost[j] == boundary_bars[i,0]) and \
               np.any(tghost[j] == boundary_bars[i,1]):
                ttghost[i] = tghost[j]
    comp_tri[len_tri:] = ttghost
    return ttghost,comp_tri

def reorder_matched_pairs(boundary_bars,tghost,tri,matched_pairs):
    ordered_matched_pairs = np.zeros((len(tghost),2),dtype=int)
    for i in range(len(tghost)):
        for j in range(len(tghost)):
            if np.any(tri[matched_pairs[j,1]] == boundary_bars[i,0]) and \
               np.any(tri[matched_pairs[j,1]] == boundary_bars[i,1]):
                # found the match to tghost[i]
                #ordered_matched_pairs[i] = matched_pairs[j]
                ordered_matched_pairs[i,0] = len(tri) + i
                ordered_matched_pairs[i,1] = matched_pairs[j,1]
    return ordered_matched_pairs   

def boundary_angles(p,boundary_nodes):  
    # -------------------------------------------------------
    #           boundary angles (cos and sin are needed)
    # -------------------------------------------------------
    # boundary_angles = angle of the normal (pointing towards the ghost
    # triangle), in radians
    # (for details of the calculation see class GEOMETRY. Notice that the
    # boundary_nodes are created ccw, the same as for a triangle. That is,
    # walking along the boundary_nodes we see the physical domain always
    # to the left

    # close the boundary polygon
    cboundary_nodes = np.zeros(len(boundary_nodes)+1,dtype=int)
    cboundary_nodes[0:-1] = boundary_nodes
    cboundary_nodes[-1] = boundary_nodes[0]
    boundary_angles = np.zeros(len(boundary_nodes))

    xx0 =p[boundary_nodes,0]; yy0 = p[boundary_nodes,1]
    xx1 = p[cboundary_nodes[1:],0]; yy1 = p[cboundary_nodes[1:],1]
    # angle of the normal (pointing towards the ghost triangle), in radians
    boundary_angles = np.arctan2(xx0 - xx1,yy1 - yy0)
    bcos = np.cos(boundary_angles)
    bsin = np.sin(boundary_angles)
    return bcos,bsin

def plot_ghosts(comp_p,tghost):
    # plots the added ghost triangles only
    plt.figure()
    for i in range(len(tghost)):
        x = [comp_p[tghost[i,0],0],comp_p[tghost[i,1],0],comp_p[tghost[i,2],0],comp_p[tghost[i,0],0]]
        y = [comp_p[tghost[i,0],1],comp_p[tghost[i,1],1],comp_p[tghost[i,2],1],comp_p[tghost[i,0],1]]
        plt.plot(x,y,'b')
        plt.hold('on')
    plt.axis('equal')
    plt.title('Ghost Triangles (used for the BC)')
    plt.xlabel('x')
    plt.ylabel('y')
    plt.hold('off')
    plt.show()   
    return 1


