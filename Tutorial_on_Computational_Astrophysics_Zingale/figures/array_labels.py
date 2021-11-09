import numpy as np
import matplotlib.pylab as plt
import grid_plot as gp

def riemann(with_time=True):

    # grid info
    xmin = 0.0
    xmax = 1.0

    nzones = 2
    ng = 0

    gr = gp.FVGrid(nzones, xmin=xmin, xmax=xmax)

    plt.clf()

    #------------------------------------------------------------------------
    # plot a domain without ghostcells
    gr.draw_grid()

    gr.label_center(0, r"$i$", fontsize="medium")
    gr.label_center(0, r"${\tt a[i]}$", fontsize="medium", extra_voff=-0.3, color="r")

    gr.label_center(1, r"$i+1$", fontsize="medium")
    gr.label_center(1, r"${\tt a[i+1]}$", fontsize="medium", extra_voff=-0.3, color="r")

    gr.label_edge(0, r"$i-\myhalf$", fontsize="medium")
    gr.label_edge(0, r"${\tt aint[i]}$", fontsize="medium", extra_voff=-0.3, color="r")

    gr.label_edge(1, r"$i+\myhalf$", fontsize="medium")
    gr.label_edge(1, r"${\tt aint[i+1]}$", fontsize="medium", extra_voff=-0.3, color="r")

    gr.label_edge(1, r"$i+\mythreehalf$", fontsize="medium", right_edge=True)
    gr.label_edge(1, r"${\tt aint[i+2]}$", fontsize="medium", extra_voff=-0.3, color="r", right_edge=True)


    gr.mark_cell_edge(0, r"$a_{i-\myhalf}$", fontsize="large", color="r")
    gr.mark_cell_edge(1, r"$a_{i+\myhalf}$", fontsize="large", color="r")
    gr.mark_cell_edge(1, r"$a_{i+\mythreehalf}$", fontsize="large", color="r", right_edge=True)

    gr.label_cell_center(0, r"$a_i$", color="r")
    gr.label_cell_center(1, r"$a_{i+1}$", color="r")


    gr.clean_axes(pad_fac=0.125, ylim=(-0.25, 1.0))

    f = plt.gcf()
    f.set_size_inches(7.0,2.0)

    plt.tight_layout()

    plt.savefig("array-labels.png")

if __name__== "__main__":
    riemann()
    riemann(with_time=False)
