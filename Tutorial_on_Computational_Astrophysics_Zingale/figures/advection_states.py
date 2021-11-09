import numpy as np
import matplotlib.pylab as plt
import grid_plot as gp

def riemann(with_time=True):

    # grid info
    xmin = 0.0
    xmax = 1.0

    nzones = 1
    ng = 0

    gr = gp.FVGrid(nzones, xmin=xmin, xmax=xmax)

    plt.clf()

    #------------------------------------------------------------------------
    # plot a domain without ghostcells
    gr.draw_grid()

    gr.label_center(0, r"$i$", fontsize="medium")

    gr.label_edge(0, r"$i-\myhalf$", fontsize="medium")
    gr.label_edge(0, r"$i+\myhalf$", fontsize="medium", right_edge=True)


    plt.arrow(gr.xc[0]+0.05*gr.dx, 0.5, 0.12*gr.dx, 0,
                shape='full', head_width=0.05, head_length=0.025,
                lw=1, width=0.02,
                edgecolor="none", facecolor="r",
                length_includes_head=True, zorder=100)

    plt.arrow(gr.xc[0]-0.05*gr.dx, 0.5, -0.12*gr.dx, 0,
                shape='full', head_width=0.05, head_length=0.025,
                lw=1, width=0.02,
                edgecolor="none", facecolor="r",
                length_includes_head=True, zorder=100)

    gr.mark_cell_left_state(0, r"$a_{i-\myhalf,R}$", fontsize="large",
                            color="b")
    gr.mark_cell_right_state(0, r"$a_{i+\myhalf,L}$", fontsize="large",
                             color="b")

    gr.label_cell_center(0, r"$a_i$")

    gr.clean_axes(pad_fac=0.125, ylim=(-0.25, 1.0))

    f = plt.gcf()
    f.set_size_inches(5.0,2.0)

    plt.tight_layout()

    plt.savefig("advection-states.png")

if __name__== "__main__":
    riemann()
    riemann(with_time=False)
