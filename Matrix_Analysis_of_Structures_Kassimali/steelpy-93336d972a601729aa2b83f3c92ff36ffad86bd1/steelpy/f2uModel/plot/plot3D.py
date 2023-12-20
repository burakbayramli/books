#
# Copyright (c) 2009-2023 fem2ufo
#
from __future__ import annotations
# Python stdlib imports
#import math
from enum import Enum
#from typing import NamedTuple, Tuple, List


# package imports
#import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D, proj3d
from matplotlib.patches import FancyArrowPatch
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
#import matplotlib.cm as cm
#from matplotlib.widgets import RadioButtons
#
import numpy as np

#from steelpy.utils.units.main import Units
from steelpy.utils.geometry.euclid import Point3

#
class PlotItems(Enum):
    bc = 'bc'
    bc_id = 'bc_id'
    beam_index = 'beam_index'
    deformed = 'deformed'
    forces = 'forces'
    global_axes = 'global_axes'
    moments = 'moments'
    node_uids = 'node_uids'
    nodes = 'nodes'
    undeformed = 'undeformed'

    @classmethod
    def to_list(cls):
        return [e.value for e in cls]
#
#
class C:
    BOX_BC = 'black'
    TXT_BC = 'white'

    BOX_NODE_UID = 'orange'
    TXT_NODE_UID = 'black'

    BOX_BEAM_IDX = 'navy'
    TXT_BEAM_IDX = 'white'

    UNDEFORMED = 'grey'
    DEFORMED = 'red'

    FORCE = 'steelblue'
    MOMENT = 'purple'

    GLOBAL_SYS = 'blue'
    LOCAL_SYS = 'green'

    MASS = 'maroon'
#
#  
#
#
def add_items_concept(m, ax, plot_num, 
                      verbosity:bool=False):
    """ """
    points = m.points
    #named_nodes = []
    #
    #ax.scatter(points._x, points._y, points._z,
    #            marker='o', color='red', s=50, alpha=0.5) # , zdir="y"
    #
    for key, point in points.items():
        ax.scatter(point.x, point.y, point.z, edgecolor='indigo',
                    marker='o', color='purple', s=50, alpha=0.5,
                    linewidth=1)
        #
        ax.text3D(point.x, point.y, point.z, str(key))
    #
    for key, beam in m.beam.items():
        n1,n2 = beam.connectivity
        #n1 = nodes[conn[0]]
        #n2 = nodes[conn[1]]
        #named_nodes = [n1, n2]
        #named_nodes.extend(conn[:])
        x = [n1.x, n2.x]
        y = [n1.y, n2.y]
        z = [n1.z, n2.z]
        #xyz = abm.get_all_points(beam_idx)
        #x, y, z = xyz[:, 0], xyz[:, 1], xyz[:, 2]

        # ----- Undeformed mesh -----
        ax.plot(x, y, z, **args_plot(color='teal', l_width=2))
#
#
class Arrow3D(FancyArrowPatch):
    def __init__(self, xs, ys, zs, *args, **kwargs):
        super().__init__((0,0), (0,0), *args, **kwargs)
        self._verts3d = xs, ys, zs

    def do_3d_projection(self, renderer=None):
        xs3d, ys3d, zs3d = self._verts3d
        xs, ys, zs = proj3d.proj_transform(xs3d, ys3d, zs3d, self.axes.M)
        self.set_positions((xs[0],ys[0]),(xs[1],ys[1]))

        return np.min(zs)
#
#
#
def get_vnorm(n1, n2):
    """get vector normalized"""
    A = Point3(*n1[:3])
    B = Point3(*n2[:3])
    vector = B - A
    return vector.normalized()    
#
#
def plot_circle(ax, moments, coord):
    """ """
    x, y, z = coord
    Mx, My, Mz = moments
    # Theta varies only between pi/2 and 3pi/2. to have a half-circle
    theta = np.linspace(np.pi/2., 4*np.pi/2.)
    # Mx
    if Mx:
        r = Mx*0.10
        xc = np.zeros_like(theta) + x # x=0
        yc = r*np.cos(theta) + y # y - y0 = r*cos(theta)
        zc = r*np.sin(theta) + z # z - z0 = r*sin(theta)
        ax.plot(xc, yc, zc)
        # arrow heads
        get_arrow(ax, xc, yc, zc, color="b")
    # My
    if My:
        r = My*0.10    
        yc = np.zeros_like(theta) + y
        zc = r*np.cos(theta) + z
        xc = r*np.sin(theta) + x
        ax.plot(xc, yc, zc, color="b")
        # arrow heads
        get_arrow(ax, xc, yc, zc, color="b")    
    # Mz
    if Mz:
        r = Mz*0.10    
        zc = np.zeros_like(theta) + z
        yc = r*np.cos(theta) + y 
        xc = r*np.sin(theta) + x
        ax.plot(xc, yc, zc, color="b")
        # arrow heads
        get_arrow(ax, xc, yc, zc, color="b")
    #
#
def get_arrow(ax, xc, yc, zc, color):
    """ """
    a = Arrow3D([xc[-2], xc[-1]], [yc[-2], yc[-1]], 
                [zc[-2], zc[-1]], mutation_scale=10, 
                lw=2, arrowstyle="->", color=color)
    ax.add_artist(a)     
#
#
#
def get_line_coord(axis:str, n1, n2, q0, q1, L0, L1, normalized):
    """ """
    x = [ 0, 0, 0, 0, 0]
    y = [ 0, 0, 0, 0, 0]
    z = [ 0, 0, 0, 0, 0]
    # corner 1
    x[0] = n1.x + normalized[ 0 ] * L0
    y[0] = n1.y + normalized[ 1 ] * L0
    z[0] = n1.z + normalized[ 2 ] * L0
    # corner 2
    x[1] = n2.x - normalized[ 0 ] * L1
    y[1] = n2.y - normalized[ 1 ] * L1
    z[1] = n2.z - normalized[ 2 ] * L1
    if axis.lower() == 'z':
        # corner 3
        x[2] = x[1]
        y[2] = y[1]
        z[2] = z[1] + q1
        # corner 4
        x[3] = x[0]
        y[3] = y[0]
        z[3] = z[0] + q0
    else:
        # corner 3
        x[2] = x[1] + q1
        y[2] = y[1] 
        z[2] = z[1] 
        # corner 4
        x[3] = x[0] + q0
        y[3] = y[0]   
        z[3] = z[0]       
    # corner 1
    x[4] = x[0]
    y[4] = y[0]
    z[4] = z[0]
    return x, y, z
#
def plot_lload(ax, x, y, z,
               color:str = 'palegreen',
               edgecolor:str = 'lightgrey'):
    """ """
    verts = [list(zip( x, y, z))]
    tri = Poly3DCollection(verts)
    tri.set_color(color)
    tri.set_alpha(0.50)
    tri.set_edgecolor(edgecolor)
    ax.add_collection3d(tri)
    #return ax.add_collection3d(tri)
#
# --------------------
#
def args_plot(color, l_width:int=2, marker=None, m_size:int=5):
    args = {'linewidth': l_width,
            #'markersize': m_size,
            'color': color,
            #'edgecolor': color,
    }
    if marker is not None:
        args['marker'] = marker
        args['markersize'] = m_size
    return args
#
def args_scatter(color, marker, l_width:int=1.5,
                 edgecolors:str = 'k'):
    args = {'linewidth': l_width, 
            'color': color,
            'edgecolors': edgecolors,
            'marker':marker,}
    return args

#
def args_text(f_size, c_txt, c_box):
    args = {'fontsize': f_size,
            'fontweight': 'bold',
            'color': c_txt,
            #'bbox': dict(facecolor=c_box, alpha=0.5),
            'horizontalalignment': 'center',
            'verticalalignment': 'bottom',}
    return args
#
# ---------------
#
def set_equal_aspect_3D(ax):
    """
    Set aspect ratio of plot correctly

    Args:
        :ax: (obj) axis object
    """

    # See https://stackoverflow.com/a/19248731
    # ax.set_aspect('equal') --> raises a NotImplementedError
    # See https://github.com/matplotlib/matplotlib/issues/1077/

    extents = np.array([getattr(ax, 'get_{}lim'.format(dim))() for dim in 'xyz'])
    sz = extents[:, 1] - extents[:, 0]
    centers = np.mean(extents, axis=1)
    maxsize = max(abs(sz))
    r = maxsize/2
    for ctr, dim in zip(centers, 'xyz'):
        getattr(ax, 'set_{}lim'.format(dim))(ctr - r, ctr + r)
#
# --------------
#
class GlobalSystem:
    Origin = np.array([0, 0, 0])
    X = np.array([1, 0, 0])
    Y = np.array([0, 1, 0])
    Z = np.array([0, 0, 1])
#
def _coordinate_system(plot, origin, axes, axes_names, color, scale=1):
    axes = [scale*np.array(axis) for axis in axes]
    x_axis, y_axis, z_axis = axes

    for axis, axis_name in zip(axes, axes_names):
        x, y, z = origin
        u, v, w = axis
        plot.scatter(x, y, z)
        plot.quiver(x, y, z, u, v, w, length=1)
        plot.text(x+u, y+v, z+w, axis_name)

    # Plot xy-plane
    p1 = np.array(origin)
    p2 = np.array(origin + y_axis)
    p3 = np.array(origin + z_axis)
    p4 = np.array(origin + y_axis + z_axis)
    points = np.array([p1, p2, p3, p4])
    xx = points[:, 0].reshape(2, 2)
    yy = points[:, 1].reshape(2, 2)
    z = points[:, 2].reshape(2, 2)
    plot.plot_surface(xx, yy, z, alpha=0.4, color=color)
#
def add_global_axes(ax):
    #to_show = m.get('post_proc').get('plot')[plot_num]
    #to_show = 'global_axes'
    #if 'global_axes' in to_show:
    #if to_show:
    orig = GlobalSystem.Origin
    X = GlobalSystem.X
    Y = GlobalSystem.Y
    Z = GlobalSystem.Z
    ax = _coordinate_system(ax, orig, (X, Y, Z), ('X', 'Y', 'Z'), color=C.GLOBAL_SYS)
#
# --------------
#



