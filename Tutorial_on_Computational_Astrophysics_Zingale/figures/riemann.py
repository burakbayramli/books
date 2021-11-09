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
    gr.label_center(1, r"$i+1$", fontsize="medium")

    gr.label_edge(1, r"$i+\myhalf$", fontsize="medium")


    plt.arrow(gr.xc[0]+0.05*gr.dx, 0.5, 0.12*gr.dx, 0,
                shape='full', head_width=0.05, head_length=0.025,
                lw=1, width=0.02,
                edgecolor="none", facecolor="r",
                length_includes_head=True, zorder=100)

    plt.arrow(gr.xc[1]-0.1*gr.dx, 0.5, -0.12*gr.dx, 0,
                shape='full', head_width=0.05, head_length=0.025,
                lw=1, width=0.02,
                edgecolor="none", facecolor="r",
                length_includes_head=True, zorder=100)

    if with_time:
        gr.mark_cell_left_state(1, r"$a_{i+\myhalf,R}^{n+\myhalf}$", fontsize="large",
                                color="b")
        gr.mark_cell_right_state(0, r"$a_{i+\myhalf,L}^{n+\myhalf}$", fontsize="large",
                                 color="b")
    else:
        gr.mark_cell_left_state(1, r"$a_{i+\myhalf,R}$", fontsize="large",
                                color="b")
        gr.mark_cell_right_state(0, r"$a_{i+\myhalf,L}$", fontsize="large",
                                 color="b")

    gr.label_cell_center(0, r"$a_i$")
    gr.label_cell_center(1, r"$a_{i+1}$")


    gr.clean_axes(pad_fac=0.125, ylim=(-0.25, 1.0))

    f = plt.gcf()
    f.set_size_inches(7.0,2.0)

    plt.tight_layout()

    if with_time:
        plt.savefig("riemann-adv.png")
    else:
        plt.savefig("riemann-adv-mol.png")

if __name__== "__main__":
    riemann()
    riemann(with_time=False)
