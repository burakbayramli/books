#
# Copyright (c) 2009 fem2ufo
#
from __future__ import annotations
# Python stdlib imports
#
#
#
# package imports
#
from steelpy.f2uModel.plot.frame import PlotBasic
from steelpy.f2uModel.plot.plot3D import (get_vnorm,
                                          plot_circle,
                                          get_line_coord,
                                          plot_lload)
#
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D, proj3d
from matplotlib.patches import FancyArrowPatch
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
#import matplotlib.cm as cm
#from matplotlib.widgets import RadioButtons
#
#
#
#
# --------------------
# Concept
# --------------------
#
#
def add_concept_load(ax, elements, nodes, basic):
    """ """
    # nodal load
    #nodeload(ax, nodes, 
    #         nload=basic.point_node)
    # beam point load
    try:
        beampointload(ax, elements, nodes,
                      beamload=basic.beams)
    except AttributeError:
        pass
    #
    #
    # beam line load
    beamlineload(ax, elements, nodes, 
                 beamload=basic.beams)
#
#
#
# --------------------
# Mesh
# --------------------
#
#
class PlotLoad(PlotBasic):
    __slots__ = ['_cls', 'figsize']
    
    def __init__(self, cls, figsize:tuple = (10, 10)):
        """
        """
        self._cls = cls
        super().__init__(figsize)
    #
    def basic(self):
        """ """
        elements = self._cls._elements
        basic = self._cls._basic
        #bname = [key for key in basic.keys()]
        #
        lims = self._cls._nodes.get_maxmin()
        fig, ax = self.init_frame(elements, lims)
        #
        #
        self.get_load(ax, load=basic)
        #
        #axcolor = 'lightgoldenrodyellow'
        #rax = plt.axes([0.05, 0.7, 0.15, 0.15], facecolor=axcolor)
        #lines_by_label = {'Nodes': s2, 'Elements': s3, 'Supports': s4}
        #
        plt.show()
        #else:
        return ax
    #
    def get_load(self, ax, load):
        """ """
        nodes = self._cls._nodes
        beams = self._cls._elements.beams()
        #
        for key, item in load.items():
            #key, item
            # nodal load
            nodeload(ax, nodes, nload=item._node)
            # beam point load
            beampointload(ax, beams, nodes, 
                          pointload=item._beam._load._point)
            # beam line load
            beamlineload(ax, beams, nodes, 
                         lineload=item._beam._load._line)
#
#
def add_mesh_load(ax, beams, nodes, basic):
    """ """
    # nodal load
    nodeload(ax, nodes, 
             nload=basic._node)
    # beam point load
    beampointload(ax, beams, nodes, 
                  pointload=basic._beam._load._point)
    # beam line load
    beamlineload(ax, beams, nodes, 
                 lineload=basic._beam._load._line)
#
def nodeload(ax, nodes, nload, scale:float = 1.0):
    """ """
    #scale = 0.000001
    df_nload = nload._node._load.df # [['node_name', 'Fx', 'Fy', 'Fz', 'Mx', 'My', 'Mz']]
    if df_nload.empty:
        return
    #
    # process
    #
    colmax = df_nload[['Fx', 'Fy', 'Fz']].abs().max()
    df_nload[['Fxnorm', 'Fynorm', 'Fznorm']] = df_nload[['Fx', 'Fy', 'Fz']].apply(lambda x: x/colmax.max(), axis=0)
    colmax = df_nload[['Mx', 'My', 'Mz']].abs().max()
    df_nload[['Mxnorm', 'Mynorm', 'Mznorm']] = df_nload[['Mx', 'My', 'Mz']].apply(lambda x: x/colmax.max(), axis=0)
    df_nload.fillna(0, inplace=True)
    #
    #df_nload.set_index('node_name')
    grpnload = df_nload.groupby(['load_name', 'load_type', 'load_number',
                                 'load_system', 'load_comment',
                                 'node_name'])
    
    #fnorm = df_nload[['Fx', 'Fy', 'Fz']].apply(lambda x: x/np.max(x.abs()), axis=0)
    #fnorm.fillna(0, inplace=True)
    #Mmax = df_nload[['Mx', 'My', 'Mz']].apply(lambda x: x/np.max(x.abs()), axis=0)
    #Mmax.fillna(0, inplace=True)
    #
    for key, nloads in grpnload:
        # FIXME: cheap hack to avoid sqlite error
        nname = key[-1]
        if isinstance(nname, (int, np.int64)):
            nname = int(key[-1])
        #
        node = nodes[nname]
        x, y, z = node[:3]
        for nload in  nloads.itertuples():
            # nodal force
            force = [ item * scale for item in nload[12:]]
            if any(force[:3]):
                Fx, Fy, Fz = force[:3]
                ax.quiver(x, y, z, Fx, Fy, Fz, color='r')
            # nodal moment
            if any(force[3:]):
                Mx, My, Mz = [item*4 for item in force[3:6]]
                #Mx, My, Mz = force[3:]
                plot_circle(ax, [Mx, My, Mz], [x, y, z])
    #
    #for key, nloads in nload._node._load.items():
    #    node = nodes[key]
    #    x, y, z = node[:3]
    #    for nload in  nloads:
    #        # nodal force
    #        force = [ item * scale for item in nload[:6]]
    #        if any(force[:3]):
    #            Fx, Fy, Fz = force[:3]
    #            ax.quiver(x, y, z, Fx, Fy, Fz, color='r')
    #        # nodal moment
    #        if any(force[3:6]):
    #            Mx, My, Mz = [item*20 for item in force[3:6]]
    #            plot_circle(ax, [Mx, My, Mz], [x, y, z])            
    #    
    #for lname, nloading in nload.items():
    #    node = nodes[lname]
    #    x, y, z = node[:3]
    #    1 / 0
    #    for key, nloading in nloads._load.items():
    #        for nload in nloads:
    #            # nodal force
    #            force = [ item * scale for item in nload[:6]]
    #            if any(force[:3]):
    #                Fx, Fy, Fz = force[:3]
    #                ax.quiver(x, y, z, Fx, Fy, Fz, color='r')
    #            # nodal moment
    #            if any(force[3:6]):
    #                Mx, My, Mz = [item*20 for item in force[3:6]]
    #                plot_circle(ax, [Mx, My, Mz], [x, y, z])
    #
    #
#
def beamlineload(ax, beams, nodes, lineload,
                 scale:float=1.0):
    """ """
    # scale = 0.00001
    df_bload = lineload.df
    if df_bload.empty:
        return
    #
    # process
    #    
    colmax = df_bload[['qx0', 'qy0', 'qz0','qx1', 'qy1', 'qz1']].abs().max()
    df_bload[['qx0norm', 'qy0norm', 'qz0norm',
              'qx1norm', 'qy1norm', 'qz1norm']] = df_bload[['qx0', 'qy0', 'qz0',
                                                            'qx1', 'qy1', 'qz1']].apply(lambda x: x/colmax.max(), axis=0)
    #
    df_bload.fillna(0, inplace=True)
    #
    grpbload = df_bload.groupby(['load_name', 'load_type', 'load_number',
                                 'load_system', 'load_comment',
                                 'element_name'])
    #
    for key, line in grpbload:
        # FIXME: cheap hack to avoid sqlite error
        bname = key[-1]
        if isinstance(bname, (int, np.int64)):
            bname = int(key[-1])
        #
        try:
            beam = beams[bname]
        except IndexError:
            continue
        n1, n2 = beam.nodes
        normalized = get_vnorm(n1, n2)
        #
        for lload in line.itertuples():
            force = [item * scale for item in lload[15:]]
            L0 = lload.L0
            L1 = lload.L1
            # qy
            if any([force[1], force[4]]):
                q0, q1 = force[1], force[4]
                x, y, z = get_line_coord('y', n1, n2, q0, q1, L0, L1, normalized)
                plot_lload(ax, x,y,z,
                           color= 'palegreen',
                           edgecolor= 'lightgrey')
            # qz
            if any([force[2], force[5]]):
                q0, q1 = force[2], force[5]
                x, y, z = get_line_coord('z', n1, n2, q0, q1, L0, L1, normalized)
                plot_lload(ax, x,y,z,
                           color= 'lightsalmon',
                           edgecolor= 'lightgrey')     
    #
    #for bname, line in lineload.items():
    #    try:
    #        beam = beams[bname]
    #    except IndexError:
    #        continue
    #    n1, n2 = beam.nodes
    #    normalized = get_vnorm(n1, n2)
    #    #
    #    for lload in line:
    #        force = [item * scale for item in lload[:6]]
    #        L0 = lload.L0
    #        L1 = lload.L1
    #        # qy
    #        if any([force[1], force[4]]):
    #            q0, q1 = force[1], force[4]
    #            x, y, z = get_line_coord('y', n1, n2, q0, q1, L0, L1, normalized)
    #            ax = plot_lload(ax, x,y,z)
    #        # qz
    #        if any([force[2], force[5]]):
    #            q0, q1 = force[2], force[5]
    #            x, y, z = get_line_coord('z', n1, n2, q0, q1, L0, L1, normalized)
    #            ax = plot_lload(ax, x,y,z)    
#
def beampointload(ax, beams, nodes, pointload,
                  scale:float=1.0):
    """ """
    df_bload = pointload.df
    if df_bload.empty:
        return
    #
    # process
    #
    #
    colmax = df_bload[['Fx', 'Fy', 'Fz']].abs().max()
    df_bload[['Fxnorm', 'Fynorm', 'Fznorm']] = df_bload[['Fx', 'Fy', 'Fz']].apply(lambda x: x/colmax.max(), axis=0)
    colmax = df_bload[['Mx', 'My', 'Mz']].abs().max()
    df_bload[['Mxnorm', 'Mynorm', 'Mznorm']] = df_bload[['Mx', 'My', 'Mz']].apply(lambda x: x/colmax.max(), axis=0)
    df_bload.fillna(0, inplace=True)
    #
    grpbload = df_bload.groupby(['load_name', 'load_type', 'load_number',
                                 'load_system', 'load_comment',
                                 'element_name'])    
    #
    for key, point in grpbload:
        # FIXME: cheap hack to avoid sqlite error
        bname = key[-1]
        if isinstance(bname, (int, np.int64)):
            bname = int(key[-1])
        #
        try:
            beam = beams[bname]
        except IndexError:
            continue
        n1, n2 = beam.nodes
        normalized = get_vnorm(n1, n2)
        #
        for pload in point.itertuples():
            force = [ item * scale for item in pload[14:]]
            x = n1.x + normalized[0] * pload.L0
            y = n1.y + normalized[1] * pload.L0
            z = n1.z + normalized[2] * pload.L0
            # point force
            if any(force[:3]):
                Fx, Fy, Fz = force[:3]
                ax.quiver(x, y, z, Fx, Fy, Fz, color='r')
            # point moment
            if any(force[3:]):
                Mx, My, Mz = [item*4 for item in force[3:]]
                ax.quiver(x, y, z, Mx, My, Mz, color='b')        
    #
    #
    #for bname, point in pointload.items():
    #    try:
    #        beam = beams[bname]
    #    except IndexError:
    #        continue        
    #    n1, n2 = beam.nodes
    #    normalized = get_vnorm(n1, n2)
    #    #
    #    for pload in point:
    #        force = [ item * scale for item in pload[:6]]
    #        x = n1.x + normalized[ 0 ] * pload.L0
    #        y = n1.y + normalized[ 1 ] * pload.L0
    #        z = n1.z + normalized[ 2 ] * pload.L0
    #        # point force
    #        if any(force[:3]):
    #            Fx, Fy, Fz = force[:3]
    #            ax.quiver(x, y, z, Fx, Fy, Fz, color='r')
    #        # point moment
    #        if any(force[3:6]):
    #            Mx, My, Mz = [item*100 for item in force[3:6]]
    #            ax.quiver(x, y, z, Mx, My, Mz, color='b')
#