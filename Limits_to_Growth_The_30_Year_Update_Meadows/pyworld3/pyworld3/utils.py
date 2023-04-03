# -*- coding: utf-8 -*-

# © Copyright Charles Vanwynsberghe (2021)

# Pyworld3 is a computer program whose purpose is to run configurable
# simulations of the World3 model as described in the book "Dynamics
# of Growth in a Finite World".

# This software is governed by the CeCILL license under French law and
# abiding by the rules of distribution of free software. You can use,
# modify and/ or redistribute the software under the terms of the CeCILL
# license as circulated by CEA, CNRS and INRIA at the following URL
# "http://www.cecill.info".

# As a counterpart to the access to the source code and rights to copy,
# modify and redistribute granted by the license, users are provided only
# with a limited warranty and the software's author, the holder of the
# economic rights, and the successive licensors have only limited
# liability.

# In this respect, the user's attention is drawn to the risks associated
# with loading, using, modifying and/or developing or reproducing the
# software by the user in light of its specific status of free software,
# that may mean that it is complicated to manipulate, and that also
# therefore means that it is reserved for developers and experienced
# professionals having in-depth computer knowledge. Users are therefore
# encouraged to load and test the software's suitability as regards their
# requirements in conditions enabling the security of their systems and/or
# data to be ensured and, more generally, to use and operate it in the
# same conditions as regards security.

# The fact that you are presently reading this means that you have had
# knowledge of the CeCILL license and that you accept its terms.

from functools import wraps

import matplotlib.pyplot as plt
from matplotlib.ticker import EngFormatter
from matplotlib.image import imread
from numpy import isnan

verbose_debug = False


def requires(outputs=None, inputs=None,
             check_at_init=True, check_after_init=True):
    """
    Decorator generator to reschedule all updates of current loop, if all
    required inputs of the current update are not known.

    """

    def requires_decorator(updater):

        if verbose_debug:
            print("""Define the update requirements...
                  - inputs:  {}
                  - outputs: {}
                  - check at init [k=0]:    {}
                  - check after init [k>0]: {}""".format(inputs, outputs,
                                                         check_at_init,
                                                         check_after_init))
            print("... and create a requires decorator for the update function",
                  updater.__name__)

        @wraps(updater)
        def requires_and_update(self, *args):
            k = args[0]
            go_grant = ((k == 0) and check_at_init or
                        (k > 0) and check_after_init)
            if inputs is not None and go_grant:
                for input_ in inputs:
                    input_arr = getattr(self, input_.lower())
                    if isnan(input_arr[k]):
                        if self.verbose:
                            warn_msg = "Warning, {} unknown for current k={} -"
                            print(warn_msg.format(input_, k), updater.__name__)
                            print("Rescheduling current loop")
                        self.redo_loop = True

            return updater(self, *args)

        return requires_and_update

    return requires_decorator


def plot_world_variables(time, var_data, var_names, var_lims,
                         img_background=None,
                         title=None,
                         figsize=None,
                         dist_spines=0.09,
                         grid=False):
    """
    Plots world state from an instance of World3 or any single sector.

    """
    prop_cycle = plt.rcParams['axes.prop_cycle']
    colors = prop_cycle.by_key()['color']

    var_number = len(var_data)

    fig, host = plt.subplots(figsize=figsize)
    axs = [host, ]
    for i in range(var_number-1):
        axs.append(host.twinx())

    fig.subplots_adjust(left=dist_spines*2)
    for i, ax in enumerate(axs[1:]):
        ax.spines["left"].set_position(("axes", -(i + 1)*dist_spines))
        ax.spines["left"].set_visible(True)
        ax.yaxis.set_label_position('left')
        ax.yaxis.set_ticks_position('left')

    if img_background is not None:
        im = imread(img_background)
        axs[0].imshow(im, aspect="auto",
                      extent=[time[0], time[-1],
                              var_lims[0][0], var_lims[0][1]], cmap="gray")

    ps = []
    for ax, label, ydata, color in zip(axs, var_names, var_data, colors):
        ps.append(ax.plot(time, ydata, label=label, color=color)[0])
    axs[0].grid(grid)
    axs[0].set_xlim(time[0], time[-1])

    for ax, lim in zip(axs, var_lims):
        ax.set_ylim(lim[0], lim[1])

    for ax_ in axs:
        formatter_ = EngFormatter(places=0, sep="\N{THIN SPACE}")
        ax_.tick_params(axis='y', rotation=90)
        ax_.yaxis.set_major_locator(plt.MaxNLocator(5))
        ax_.yaxis.set_major_formatter(formatter_)

    tkw = dict(size=4, width=1.5)
    axs[0].set_xlabel("time [years]")
    axs[0].tick_params(axis='x', **tkw)
    for i, (ax, p) in enumerate(zip(axs, ps)):
        ax.set_ylabel(p.get_label(), rotation="horizontal")
        ax.yaxis.label.set_color(p.get_color())
        ax.tick_params(axis='y', colors=p.get_color(), **tkw)
        ax.yaxis.set_label_coords(-i*dist_spines, 1.01)

    if title is not None:
        fig.suptitle(title, x=0.95, ha="right", fontsize=10)

    plt.tight_layout()
