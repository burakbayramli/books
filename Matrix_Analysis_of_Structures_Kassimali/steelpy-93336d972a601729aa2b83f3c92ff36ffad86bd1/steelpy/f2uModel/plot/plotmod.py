#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from datetime import datetime
from enum import Enum
from math import ceil
from random import randint
import os

from commonlibs.math.vectors import unit_vector
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.cm as cm
import matplotlib.pyplot as plt
import numpy as np

#from . import MODULE_NAME
#from ._element import GlobalSystem
#from ._log import logger


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


def plot_all(m):
    """Create all plots defined in the model object"""

    #mpp = m.get('post_proc', None)
    #if not mpp:
    #    return
    #if not mpp.get('plot', ()):
    #    return

    #ps = m.get('post_proc').get('plot_settings', {})
    #abm = m.results.get('mesh').get('abm')
    #rfiles = m.results.set_feature('files')

    num_tot = m.get('post_proc').len('plot')
    file_list = []
    for plot_num, _ in enumerate(m.get('post_proc').iter('plot')):
        #logger.info(f"Creating plot {plot_num + 1}/{num_tot}...")
        ax = init_3D_plot(*abm.get_lims())
        add_items_per_beam(m, ax, plot_num)
        #add_global_axes(m, ax, plot_num)
        #add_boundary_conditions(m, ax, plot_num)

        #if ps.get('save', False):
        #    now = datetime.now().strftime("%F_%H%M%S")
        #    ext = 'png'
        #    rand = randint(100, 999)
        #    fname = f"{MODULE_NAME.lower()}_{now}_{plot_num+1:02}_{rand}.{ext}"
        #    fname = os.path.join(os.path.abspath(ps.get('save')), fname)
        #    logger.info(f"Saving plot to file {fname!r}...")
        #    plt.tight_layout()
        #    plt.savefig(fname, dpi=300, format='png')
        #    file_list.append(fname)

    #rfiles.set('plots', file_list)

    #if ps.get('show', False):
    plt.show()
    plt.close('all')


def init_3D_plot(x_lims, y_lims, z_lims):
    """
    Inititalize the 3D plot

    Args:
        :x_lims: (tuple) min and max x-value
        :y_lims: (tuple) min and max y-value
        :z_lims: (tuple) min and max z-value
    """

    plt.figure(figsize=(10, 10))
    ax = plt.axes(projection='3d')

    # Avoid setting same min and max value by adding diff
    diff = (-1e-6, 1e-6)
    ax.set_xlim(*(x+d for x, d in zip(x_lims, diff)))
    ax.set_ylim(*(y+d for y, d in zip(y_lims, diff)))
    ax.set_zlim(*(z+d for z, d in zip(z_lims, diff)))

    set_equal_aspect_3D(ax)
    ax.set_xlabel('X')
    ax.set_ylabel('Y')
    ax.set_zlabel('Z')
    return ax


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


def args_plot(m, color, marker=None):
    args = {
        'linewidth': m.get('post_proc').get('plot_settings', {}).get('linewidth', 2),
        'markersize': m.get('post_proc').get('plot_settings', {}).get('markersize', 5),
        'color': color,
    }
    if marker is not None:
        args['marker'] = marker
    return args


def args_scatter(m, color, marker=None):
    args = {
        'linewidth': m.get('post_proc').get('plot_settings', {}).get('linewidth', 2),
        'color': color,
    }
    return args


def args_text(m, c_txt, c_box):
    args = {
        'fontsize': m.get('post_proc').get('plot_settings', {}).get('fontsize', 10),
        'color': c_txt,
        'bbox': dict(facecolor=c_box, alpha=0.5),
        'horizontalalignment': 'center',
        'verticalalignment': 'bottom',
    }
    return args


def add_global_axes(m, ax, plot_num):
    to_show = m.get('post_proc').get('plot')[plot_num]

    if 'global_axes' in to_show:
        orig = GlobalSystem.Origin
        X = GlobalSystem.X
        Y = GlobalSystem.Y
        Z = GlobalSystem.Z
        ax = _coordinate_system(ax, orig, (X, Y, Z), ('X', 'Y', 'Z'), color=C.GLOBAL_SYS)


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


def add_items_per_beam(m, ax, plot_num):
    ps = m.get('post_proc').get('plot_settings', {})
    to_show = m.get('post_proc').get('plot')[plot_num]
    abm = m.results.get('mesh').get('abm')
    marker = 'o' if 'nodes' in to_show else None

    for beam_idx in abm.beams.keys():
        xyz = abm.get_all_points(beam_idx)
        x, y, z = xyz[:, 0], xyz[:, 1], xyz[:, 2]

        # ----- Undeformed mesh -----
        if PlotItems.undeformed.value in to_show:
            ax.plot(x, y, z, **args_plot(m, C.UNDEFORMED))

        # ----- Deformed mesh -----
        if PlotItems.deformed.value in to_show:
            d = m.results.get('tensors').get('comp:U')
            scale = ps.get('scale_deformation', 1)
            xd = x + scale*abm.gbv(d['ux'], beam_idx)
            yd = y + scale*abm.gbv(d['uy'], beam_idx)
            zd = z + scale*abm.gbv(d['uz'], beam_idx)
            ax.plot(xd, yd, zd, **args_plot(m, C.DEFORMED, marker=marker))

        # ----- Forces -----
        if PlotItems.forces.value in to_show:
            d = m.results.get('tensors').get('comp:F')
            scale = ps.get('scale_forces', 1)
            Fx = scale*abm.gbv(d['Fx'], beam_idx)
            Fy = scale*abm.gbv(d['Fy'], beam_idx)
            Fz = scale*abm.gbv(d['Fz'], beam_idx)
            if ps.get('deform_loads', True):
                ax.quiver(xd, yd, zd, Fx, Fy, Fz, color=C.FORCE)
            else:
                ax.quiver(x, y, z, Fx, Fy, Fz, color=C.FORCE)

        # ----- Moments -----
        if PlotItems.moments.value in to_show:
            d = m.results.get('tensors').get('comp:F')
            scale = ps.get('scale_moments', 1)
            Fx = scale*abm.gbv(d['Mx'], beam_idx)
            Fy = scale*abm.gbv(d['My'], beam_idx)
            Fz = scale*abm.gbv(d['Mz'], beam_idx)
            if ps.get('deform_loads', True):
                ax.quiver(xd, yd, zd, Fx, Fy, Fz, color=C.MOMENT)
            else:
                ax.quiver(x, y, z, Fx, Fy, Fz, color=C.MOMENT)

        # ----- Beam index -----
        if PlotItems.beam_index.value in to_show:
            center = ceil(len(x)/2)
            coord = (x[center], y[center], z[center])
            ax.text(*coord, str(beam_idx), **args_text(m, c_txt=C.TXT_BEAM_IDX, c_box=C.BOX_BEAM_IDX))

        # ----- Named nodes -----
        if PlotItems.node_uids.value in to_show:
            for uid, coord in abm.named_nodes[beam_idx].items():
                ax.text(*coord, uid, **args_text(m, c_txt=C.TXT_NODE_UID, c_box=C.BOX_NODE_UID))


def add_boundary_conditions(m, ax, plot_num):
    to_show = m.get('post_proc').get('plot')[plot_num]
    if PlotItems.bc.value not in to_show:
        return

    r = m.results
    abm = m.results.get('mesh').get('abm')
    mbc = m.get('bc')
    deform = r.get('tensors').get('comp:U')
    ux = deform['ux']
    uy = deform['uy']
    uz = deform['uz']

    # Fixed
    for fix in mbc.iter('fix'):
        uid = fix['node']
        xyz = abm.get_point_by_uid(uid)
        ax.scatter(*xyz, **args_scatter(m, color=C.BOX_BC, marker='s'))
        if PlotItems.bc_id.value in to_show:
            bc_id = get_bc_id(fix['fix'])
            ax.text(*xyz, f'f{bc_id}', **args_text(m, c_txt=C.TXT_BC, c_box=C.BOX_BC))

    ps = m.get('post_proc').get('plot_settings', {})

    for con in mbc.iter('connect'):
        uid1 = con['node1']
        uid2 = con['node2']
        scale = ps.get('scale_deformation', 1)
        X1 = abm.get_point_by_uid(uid=uid1)
        X2 = abm.get_point_by_uid(uid=uid2)
        ux1, uy1, uz1 = abm.gnv(ux, uid1), abm.gnv(uy, uid1), abm.gnv(uz, uid1)
        ux2, uy2, uz2 = abm.gnv(ux, uid2), abm.gnv(uy, uid2), abm.gnv(uz, uid2)
        x1 = X1 + scale*np.asarray([ux1, uy1, uz1])
        x2 = X2 + scale*np.asarray([ux2, uy2, uz2])
        ax.plot(*zip(x1, x2), **args_plot(m, color=C.BOX_BC))
        if PlotItems.bc_id.value in to_show:
            bc_id = get_bc_id(con['fix'])
            ax.text(*(x1+x2)/2, f'c{bc_id}', **args_text(m, c_txt=C.TXT_BC, c_box=C.BOX_BC))


def get_bc_id(constraints):
    id_num = {'ux': 1, 'uy': 2, 'uz': 4, 'tx': 8, 'ty': 16, 'tz': 32, 'all': 63}

    bc_id = 0
    for constraint in constraints:
        bc_id += id_num[constraint]
        if bc_id == id_num['all']:
            break

    return bc_id
