#
# Copyright (c) 2009-2023 fem2ufo
#
from __future__ import annotations
# Python stdlib imports
#import math
#from enum import Enum
#from typing import NamedTuple, Tuple, List
from collections import defaultdict


# package imports
import numpy as np
import matplotlib.pyplot as plt
#from mpl_toolkits.mplot3d import Axes3D, proj3d
#from matplotlib.patches import FancyArrowPatch
#from mpl_toolkits.mplot3d.art3d import Poly3DCollection
#import matplotlib.cm as cm
from matplotlib.widgets import RadioButtons, CheckButtons
import matplotlib.cbook as cbook
#

#from steelpy.utils.units.main import Units
#from steelpy.utils.geometry.euclid import Point3
from steelpy.f2uModel.plot.plot3D import (set_equal_aspect_3D,
                                          add_global_axes, get_vnorm, 
                                          args_plot, args_text, args_scatter)
#
#
#
# --------------------
# Basic
# --------------------
#
#
class PlotBasic:
    __slots__ = ['figsize']
    
    def __init__(self, figsize:tuple = (10, 10)):
        """
        """
        #self._cls = cls
        self.figsize = figsize
    #
    #
    def init_plot(self, lims: list):
        """ """
        #beams = self._mesh._elements.beams()
        #nodes = self._cls._nodes
        #lims = nodes.get_maxmin()
        #
        #mbc = self._cls._boundaries
        #supports = mbc.supports()
        #supports = mbc._nodes
        #
        fig, ax = init_frame_plot([lims[0][0], lims[1][0]],
                                  [lims[0][1], lims[1][1]],
                                  [lims[0][2], lims[1][2]],
                                  figsize = self.figsize)
        #
        #
        #with cbook.get_sample_data('C:\\Users\\svort\\OneDrive\\Public\\Python\\steelpy_06\\steelpy\\docs\\img\\steelpy_logo.png') as logo_file:
        #    logo = plt.imread(logo_file, format='png')
        #
        #newax = fig.add_axes([0.85, 0.90, 0.1, 0.1], anchor='NE', zorder=-1)
        #newax.imshow(logo)
        #newax.axis('off')
        # Draw image
        #axin = ax.inset_axes([0.8, 0.8, 0.2, 0.2],transform=ax.transData)    # create new inset axes in data coordinates
        #axin.imshow(logo)
        #axin.axis('off')        
        #
        #ax = init_frame(beams, ax)
        #ax = init_nodes(nodes, supports, ax)
        #ax.grid()
        return fig, ax    
    #
    def init_frame(self, elements, lims:list):
        """ """
        beams = elements.beams()
        fig, ax = self.init_plot(lims)
        ax = init_frame(beams, ax)
        return fig, ax
    #
    #
    def materials(self, materials, elements, lims:list):
        """ """
        beams = elements.beams()
        #materials = self._mesh._materials
        #
        fig, ax = self.init_plot(lims)
        #ax = init_frame(beams, ax)
        ax = add_materials(beams, materials, fig, ax)
        plt.legend(fontsize=12,
                   numpoints=1, 
                   bbox_to_anchor=(0, 0))
        plt.show()
    #
    def sections(self, sections, elements, lims:list):
        """ """
        beams = elements.beams()
        #sections = self._mesh._sections
        fig, ax = self.init_plot(lims)
        ax = add_sections(beams, sections, fig, ax)
        plt.legend(fontsize=12,
                   numpoints=1, 
                   bbox_to_anchor=(0, 0))
        plt.show()
    #    
#
#
# --------------------
# Concept
# --------------------
#
#
# --------------------
# Mesh
# --------------------
#
#
class PlotFrame(PlotBasic):
    __slots__ = ['figsize']
    
    def __init__(self, figsize:tuple = (10, 10)):
        """
        """
        #self._mesh = mesh
        #self.figsize = figsize
        super().__init__(figsize)
    #
    #
    def frame(self, nodes, elements, supports, f_size:float = 10):
        """ """
        def callback(label):
            """ """
            lns = lines_by_label[label]
            for ln in lns:
                ln.set_visible(not ln.get_visible())
                #ln.figure.canvas.draw_idle()
            fig.canvas.draw()
        #
        beams = elements.beams()
        #nodes = self._cls._nodes
        #lims = nodes.get_maxmin()
        #
        #mbc = self._cls._boundaries
        #supports = mbc.supports()
        #supports = mbc._nodes
        #
        lims = nodes.get_maxmin()
        fig, ax = self.init_frame(elements, lims)
        ax = init_nodes(nodes, supports, ax)
        #
        s3 = add_beams(nodes, beams, ax, f_size)
        s2 = add_nodes(nodes, ax, f_size)
        s4 = add_supports(nodes, supports, ax, f_size)
        #
        #
        axcolor = 'lightgoldenrodyellow'
        rax = plt.axes([0.05, 0.7, 0.15, 0.15], facecolor=axcolor)
        lines_by_label = {'Nodes': s2, 'Elements': s3, 'Supports': s4}
        #
        check = CheckButtons(ax=rax,
                             labels=lines_by_label.keys(),
                             actives=[False for l in lines_by_label.values()])
        #
        for r in check.rectangles:
            r.set_facecolor("chartreuse") 
            r.set_edgecolor("k")
            r.set_alpha(0.2)        
        #
        check.on_clicked(callback)
        #
        plt.show()
        #else:
        return ax        
        
    #
    #
    #
    def frame1(self):
        """ """
        #
        #
        def callback(label):
            """ """
            lns = lines_by_label[label]
            for ln in lns:
                ln.set_visible(not ln.get_visible())
                #ln.figure.canvas.draw_idle()
            fig.canvas.draw()
            #plt.show()
            #ax.clear()        
        #
        #
        #plot_num = 2
        beams = self._mesh._elements.beams()
        nodes = self._mesh.nodes()
        lims = nodes.get_maxmin()
        #
        mbc = self._mesh.boundaries()
        supports = mbc.supports()
        #
        fig, ax = init_frame_plot([lims[0][0], lims[1][0]],
                                  [lims[0][1], lims[1][1]],
                                  [lims[0][2], lims[1][2]])
        #
        s3 = add_beams(nodes, beams, ax)
        #
        s2 = add_nodes(nodes, ax)
        #
        s4 = add_supports(nodes, supports, ax)
        #
        #
        axcolor = 'lightgoldenrodyellow'
        rax = plt.axes([0.05, 0.7, 0.15, 0.15], facecolor=axcolor)
        #
        lines = [s2, s3, s4]
        labels = ['Nodes', 'Elements', 'Supports']
        #label = [True, True, True]
        #
        lines_by_label = {'Nodes': s2, 'Elements': s3, 'Supports': s4}
        #
        # Make checkbuttons with all plotted lines with correct visibility
        check = CheckButtons( ax=rax,
                              labels=lines_by_label.keys(),
                              actives=[False for l in lines_by_label.values()])
                              #actives=[l.get_visible() for l in lines_by_label.values()])
                              #label_props={'color': line_colors},
                              #frame_props={'edgecolor': line_colors},
                              #check_props={'facecolor': line_colors},)
        #
        check.on_clicked(callback)
        #
        #rfiles.set('plots', file_list)
        #if show:
        plt.show()
        #else:
        return ax        
    #    
    def frame2(self):
        """ """
        #
        def callback(label):
            lns = lines_by_label[label]
            for ln in lns:
                ln.set_visible(not ln.get_visible())
                #ln.figure.canvas.draw_idle()
            fig.canvas.draw()
            #plt.show()
            #ax.clear()
        #
        beams = self._mesh._elements.beams()
        nodes = self._mesh.nodes()
        lims = nodes.get_maxmin()
        #mbc = self._mesh.boundaries()
        #supports = mbc.supports()
        #
        fig, ax = init_frame_plot([lims[0][0], lims[1][0]],
                                  [lims[0][1], lims[1][1]],
                                  [lims[0][2], lims[1][2]])
        #
        S0 = add_beams2(nodes, beams, ax)
        #
        materials = self._mesh._materials
        S1 = add_materials(beams, materials, ax)
        #
        S2 = add_sections(beams, ax)
        #
        # get radio bottoms
        #
        axcolor = 'lightgoldenrodyellow'
        rax = plt.axes([0.05, 0.7, 0.15, 0.15], facecolor=axcolor)
        #
        radio = RadioButtons(rax, ('Material','Section'))
        #
        #lines = [S0, S1]
        #labels = ['Reset', 'Material']
        #label = [True, False, False]
        #
        lines_by_label = {'Material': S1, 'Section': S2}
        #
        radio.on_clicked(callback)
        #
        #print('-->')
        plt.show()
        return ax
    #
    def frameXX(self, verbosity:bool=False , show=True):
        """"""
        #print('--')
        #
        def framefunc(ax, label, s2, s3, s4):
            """ """
            fdict = { #'Material': s0, 'Section': s1,
                     'Nodes': s2, 'Elements': s3,
                     'Supports': s4}
            ydata = fdict[label]
            for data in ydata:
                ax.set_ydata(ydata)
            ax.canvas.draw()
        #
        def func(label):
            index = labels.index(label)
            lines[index].set_visible(not lines[index].get_visible())
            fig.canvas.draw()        
        #
        def callback(label):
            lns = lines_by_label[label]
            for ln in lns:
                ln.set_visible(not ln.get_visible())
                ln.figure.canvas.draw_idle()
        #
        #
        def callback2(label):
            ln = lines_by_label[label]
            ln.set_visible(not ln.get_visible())
            ln.figure.canvas.draw_idle()         
        #
        #plot_num = 2
        beams = self._mesh._elements.beams()
        nodes = self._mesh.nodes()
        lims = nodes.get_maxmin()
        #
        mbc = self._mesh.boundaries()
        supports = mbc.supports()       
        #
        fig, ax = init_frame_plot([lims[0][0], lims[1][0]],
                                  [lims[0][1], lims[1][1]],
                                  [lims[0][2], lims[1][2]])
        #
        s3 = add_beams(nodes, beams, ax)
        #
        #
        s2 = add_nodes(nodes, ax)
        #
        #s3 = add_beams2(nodes, beams, ax)
        #
        s4 = add_supports(nodes, supports, ax)
        #
        #materials = self._mesh._materials
        #add_materials(beams, materials, fig)
        #
        #if verbosity:
        #    add_global_axes(ax, plot_num)
        #
        #
        axcolor = 'lightgoldenrodyellow'
        rax = plt.axes([0.05, 0.7, 0.15, 0.15], facecolor=axcolor)
        #radio = RadioButtons(rax, ('Nodes', 'Elements', 'Supports')) #'Material', 'Section', 
        #                     #label_props={'color': 'cmy', 'fontsize': [12, 12, 12, 12]},
        #                     #radio_props={'s': [16, 16, 16, 16]})
        #
        lines = [s2, s3, s4]
        labels = ['Nodes', 'Elements', 'Supports']
        #label = [True, True, True]
        #
        lines_by_label = {'Nodes': s2, 'Elements': s3, 'Supports': s4}
        #lines_by_label = {l.get_label(): l for l in [s2, s3, s4]}
        #line_colors = [l.get_color() for l in lines_by_label.values()]
        #
        # Make checkbuttons with all plotted lines with correct visibility
        #rax = fig.add_axes([0.05, 0.4, 0.1, 0.15])
        check = CheckButtons( ax=rax,
                              labels=lines_by_label.keys(),
                              actives=[False for l in lines_by_label.values()])
                              #actives=[l.get_visible() for l in lines_by_label.values()])
                              #label_props={'color': line_colors},
                              #frame_props={'edgecolor': line_colors},
                              #check_props={'facecolor': line_colors},)
        #
        #radio.on_clicked(func)
        check.on_clicked(callback)
        #
        # xposition, yposition, width and height
        #ax_check = plt.axes([0.9, 0.001, 0.2, 0.3])
        #plot_button = CheckButtons(ax_check, labels, label)
        #plot_button.on_clicked(func)
        #
        #
        #rfiles.set('plots', file_list)
        if show:
            plt.show()
        else:
            return ax
    #    
    #
#
#
# -------------------------
# Start basic frame 
# -------------------------
#
def init_frame_plot(x_lims, y_lims, z_lims,
                    figsize:tuple = (10, 10)):
    """
    Inititalize the 3D Frame plot

    Args:
        :x_lims: (tuple) min and max x-value
        :y_lims: (tuple) min and max y-value
        :z_lims: (tuple) min and max z-value
    """

    fig = plt.figure(figsize=(10, 10))
    
    ax = plt.axes(projection='3d')
    #ax = fig.add_subplot(projection='3d')
    #ax = plt.subplot(111, projection='3d')

    # Avoid setting same min and max value by adding diff
    diff = (-1e-6, 1e-6)
    ax.set_xlim(*(x+d for x, d in zip(x_lims, diff)))
    ax.set_ylim(*(y+d for y, d in zip(y_lims, diff)))
    ax.set_zlim(*(z+d for z, d in zip(z_lims, diff)))
    #
    set_equal_aspect_3D(ax)
    ax.set_xlabel('X')
    ax.set_ylabel('Y')
    ax.set_zlabel('Z')
    #
    return fig, ax
#
#
# -------------------------
# plot mesh
# -------------------------
#
#
def init_frame(beams, ax, l_width:float = 2.0):
    """ """
    marker = '.'
    for key, beam in beams.items():
        n1, n2 = beam.nodes
        x = [n1.x, n2.x]
        y = [n1.y, n2.y]
        z = [n1.z, n2.z]
        # ----- Undeformed mesh -----
        ax.plot(x, y, z,
                **args_plot(color='k', 
                            marker = marker,
                            l_width=l_width))
    # 
    return ax    
#
#
def init_nodes(nodes, supports, ax):
    """ """
    node_id = set(nodes.keys())
    supp_id = set(supports.keys())
    node_id = node_id - supp_id
    #
    # -----------------
    # nodes
    # -----------------
    marker = 'o'
    for name in node_id:
    #for key, node in nodes.items():
        node = nodes[name]
        xyz = node[:3]
        ax.scatter(*xyz, s=50,
                   **args_scatter(color='orange',
                                  marker=marker,
                                  edgecolors='k'))
    #
    # -----------------
    # supports
    # -----------------
    for name in  supp_id:
        bc = supports[name]
        bcn = bc[:6]
        if all(bcn):
            if sum(bcn) == 0:
                continue
            marker ='s'
            #marker ='_'
        else:
            marker ='^'
        #
        node = nodes[name]
        xyz = node[:3]
        ax.scatter(*xyz, s=50,
                   **args_scatter(color='m',
                                  marker=marker,
                                  #l_width=3, 
                                  edgecolors='k'))
    #
    return ax
#
#
def init_points(nodes, supports, ax):
    """ """
    node_id = set(nodes.keys())
    supp_id = set(supports.keys())
    node_id = node_id - supp_id
    #
    # -----------------
    # nodes
    # -----------------
    marker = 'o'
    for name in node_id:
    #for key, node in nodes.items():
        node = nodes[name]
        xyz = node[:3]
        ax.scatter(*xyz, s=50,
                   **args_scatter(color='orange',
                                  marker=marker,
                                  edgecolors='k'))
    #
    # -----------------
    # supports
    # -----------------
    for name in  supp_id:
        bc = supports[name]
        bcn = bc[:6]
        if all(bcn):
            if sum(bcn) == 0:
                continue
            marker ='s'
            #marker ='_'
        else:
            marker ='^'
        #
        node = nodes[name]
        xyz = node[:3]
        ax.scatter(*xyz, s=50,
                   **args_scatter(color='m',
                                  marker=marker,
                                  #l_width=3, 
                                  edgecolors='k'))
    #
    return ax 
#
#
# -------------------------
#
def add_beams(nodes, beams, ax, f_size:float =10):
              #ax, plot_num, 
              #verbosity:bool=False):
    """ """
    #ax = fig.add_subplot(projection='3d')
    lg = []
    #named_nodes = []
    for key, beam in beams.items():
        n1, n2 = beam.nodes
        #conn = beam.connectivity
        #n1 = nodes[conn[0]]
        #n2 = nodes[conn[1]]
        #named_nodes = [n1, n2]
        #named_nodes.extend(conn[:])
        #x = [n1.x, n2.x]
        #y = [n1.y, n2.y]
        #z = [n1.z, n2.z]
        #xyz = abm.get_all_points(beam_idx)
        #x, y, z = xyz[:, 0], xyz[:, 1], xyz[:, 2]

        # ----- Undeformed mesh -----
        #if PlotItems.undeformed.value in to_show:
        #ax.plot(x, y, z,
        #        **args_plot(color='k', 
        #                    marker = '.', l_width=1.5))
        #
        # ----- Deformed mesh -----
        #if PlotItems.deformed.value in to_show:
        #    d = m.results.get('tensors').get('comp:U')
        #    scale = ps.get('scale_deformation', 1)
        #    xd = x + scale*abm.gbv(d['ux'], beam_idx)
        #    yd = y + scale*abm.gbv(d['uy'], beam_idx)
        #    zd = z + scale*abm.gbv(d['uz'], beam_idx)
        #    ax.plot(xd, yd, zd, **args_plot(m, C.DEFORMED, marker=marker))
        #
        # ----- Forces -----
        #if PlotItems.forces.value in to_show:
        #    d = m.results.get('tensors').get('comp:F')
        #    scale = ps.get('scale_forces', 1)
        #    Fx = scale*abm.gbv(d['Fx'], beam_idx)
        #    Fy = scale*abm.gbv(d['Fy'], beam_idx)
        #    Fz = scale*abm.gbv(d['Fz'], beam_idx)
        #    if ps.get('deform_loads', True):
        #        ax.quiver(xd, yd, zd, Fx, Fy, Fz, color=C.FORCE)
        #    else:
        #        ax.quiver(x, y, z, Fx, Fy, Fz, color=C.FORCE)
        #
        # ----- Moments -----
        #if PlotItems.moments.value in to_show:
        #    d = m.results.get('tensors').get('comp:F')
        #    scale = ps.get('scale_moments', 1)
        #    Fx = scale*abm.gbv(d['Mx'], beam_idx)
        #    Fy = scale*abm.gbv(d['My'], beam_idx)
        #    Fz = scale*abm.gbv(d['Mz'], beam_idx)
        #    if ps.get('deform_loads', True):
        #        ax.quiver(xd, yd, zd, Fx, Fy, Fz, color=C.MOMENT)
        #    else:
        #        ax.quiver(x, y, z, Fx, Fy, Fz, color=C.MOMENT)
        #
        # ----- Beam index -----
        #if verbosity:
        coord = [0,0,0]
        try:
            mid_length = beam.L.value * 0.50
        except AttributeError:
            mid_length = beam.L * 0.50
        #
        normalized = get_vnorm(n1, n2)
        #
        coord[0] = n1.x + normalized[0] * mid_length
        coord[1] = n1.y + normalized[1] * mid_length
        coord[2] = n1.z + normalized[2] * mid_length
        #
        lg.append(ax.text(*coord, str(key), visible=False,
                          **args_text(f_size=f_size,
                                      c_txt='fuchsia',
                                      c_box='b')))
    #
    #
    #add_global_axes(ax)
    #
    return lg
#
#
def add_nodes(nodes, ax, f_size:float =10):
    """ """
    #ax = fig.add_subplot(projection='3d')
    #
    lg = []
    named_nodes = list(nodes.keys())
    # ----- Named nodes -----
    #named_nodes = set(named_nodes)
    for node_id in named_nodes:
        node = nodes[node_id]
        lg.append(ax.text(*node[:3], node.number, visible=False,
                **args_text(f_size=f_size,
                            c_txt='g',
                            c_box='b')))
    #
    return lg
#
#
def add_supports(nodes, supports, ax, f_size:float =10):
    """ """
    #ax = fig.add_subplot(projection='3d')
    lg = []
    for key, bc in supports.items():
        node = nodes[key]
        xyz = node[:3]
        bcn = bc[:6]
        bc_id = [int(item) for item in bcn]
        lg.append( ax.text(*xyz, f'{bc_id}', visible=False,
                    **args_text(f_size=f_size,
                                c_txt='m', c_box='b')))
    #
    return lg
#
#
# -------------------------
# plot material & sections
# -------------------------
#
#
def add_materials(beams, materials, fig, ax,
                  l_width:float = 2):
    """ """
    #ax = fig.add_subplot(projection='3d')
    mname = [str(item) for item in materials.keys()]
    Fy = [[str(key), str(mat.Fy/10**6)]
          for key, mat in materials.items()]
    
    matgrp = defaultdict(list)
    for key, beam in beams.items():
        n1, n2 = beam.nodes
        x = [n1.x, n2.x]
        y = [n1.y, n2.y]
        z = [n1.z, n2.z]
        #
        mat = beam.material
        matgrp[mat.name].append([x, y, z])
    #
    #
    # create colormap
    n_lines = len(matgrp)
    cm = plt.cm.bwr(np.linspace(0, 1, n_lines))    
    #
    # Plot beam with colour accroding to material
    #
    marker = '.'
    lg = []
    for idx, (key, items) in enumerate(matgrp.items()):
        #coord = list(zip(*items))
        #ax.plot(*coord, label=key, 
        #        **args_plot(color=cm[idx], 
        #                    marker= marker,
        #                    l_width=l_width))
        #coord2 = [ for l1, l2 in zip(item)
        #           for item in coord]
        #coord2 = []
        #for item in coord:
        #    end1, end2 = [], []
        #    for lt in item:
        #        end1.extend([lt[0]])
        #        end2.extend([lt[1]])
        #    #list3 = [l1+l2 for l1, l2 in zip(*item)]
        #    #print('--')
        #    coord2.append([end1, end2])
        #
        #ax.plot(*coord2, label=key, 
        #        **args_plot(color=cm[idx], 
        #                    marker= marker,
        #                    l_width=l_width))
        #
        for item in items:
            #lg.extend(ax.plot(*item, #visible=False,
            #                  **args_plot(color=cm[idx], 
            #                              marker= marker,
            #                              l_width=l_width)))
            ax.plot(*item, #label=key, 
                    **args_plot(color=cm[idx], 
                                marker= marker,
                                l_width=l_width))
    #
    #ax.legend(#lg, mname,
    #           #loc='right',
    #           fontsize=12,
    #           numpoints=1, 
    #           bbox_to_anchor=(0, 0))
    #
    # plot table
    #
    columns = ['ID', 'Fy']
    #columns2 = ('Material', )
    #colwdth = [len(item) for item in columns2]
    #colwdth += [len(item) for item in mname]
    #nocol = len(colwdth)
    #colwdth = [item/nocol for item in colwdth]
    #colratios = [round(float(item)/sum(colwdth), 3) for item in colwdth]
    colratios = [.05, .05]
    #
    ax.table(cellText=Fy,
             #rowLabels=mname, 
             rowColours=cm, 
             colLabels=columns,
             colWidths=colratios, 
             loc='right')
    #
    #
    return ax
#
#
def add_sections(beams, sections, fig, ax,
                 l_width:float = 2):
    """ """
    #
    sectdata = [[str(key), str(sec.type)]
                for key, sec in sections.items()]
    #
    secgrp = defaultdict(list)
    for key, beam in beams.items():
        n1, n2 = beam.nodes
        x = [n1.x, n2.x]
        y = [n1.y, n2.y]
        z = [n1.z, n2.z]
        #
        sect = beam.section
        secgrp[sect.name].append([x, y, z])
    #
    # create colormap
    n_lines = len(secgrp)
    cm = plt.cm.bwr(np.linspace(0, 1, n_lines))    
    #
    lg = []
    for idx, (key, items) in enumerate(secgrp.items()):
        #coord = list(zip(*items))
        for item in items:
            #lg.extend(ax.plot(*item, visible=False,
            #                  **args_plot(color=cm[idx], 
            #                              marker = '.', l_width=1.5)))
            ax.plot(*item, #visible=False,
                    **args_plot(color=cm[idx], 
                                marker = '.',
                                l_width=l_width))
    #
    # plot table
    #
    columns = ['ID', 'Type']
    #columns2 = ('Material', )
    #colwdth = [len(item) for item in columns2]
    #colwdth += [len(item) for item in mname]
    #nocol = len(colwdth)
    #colwdth = [item/nocol for item in colwdth]
    #colratios = [round(float(item)/sum(colwdth), 3) for item in colwdth]
    colratios = [.05, .05]
    #
    ax.table(cellText=sectdata,
             #rowLabels=mname, 
             rowColours=cm, 
             colLabels=columns,
             colWidths=colratios, 
             loc='right')
    #    
    #
    return ax
#
#

