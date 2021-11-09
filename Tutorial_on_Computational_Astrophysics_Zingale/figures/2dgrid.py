import numpy as np
import matplotlib.pyplot as plt
import grid_plot as gp

def simplegrid():

    nzones = 3
    gr = gp.FVGrid2d(nzones, nzones, ng=0)

    # plot a domain without ghostcells
    gr.draw_grid()
    

    #------------------------------------------------------------------------
    # label
    gr.label_cell_center(nzones//2, nzones//2, r"$a_{i,j}$", fontsize="large")
    gr.label_cell_center(nzones//2+1, nzones//2, r"$a_{i+1,j}$", fontsize="large")
    gr.label_cell_center(nzones//2, nzones//2+1, r"$a_{i,j+1}$", fontsize="large")

    # i+1/2,j interface
    gr.mark_cell_left_state_x(nzones//2, nzones//2, r"$a_{i+\myhalf,j,L}$",
                              color="C0")
    gr.mark_cell_right_state_x(nzones//2+1, nzones//2, r"$a_{i+\myhalf,j,R}$",
                               color="C0")


    # i,j+1/2 interface
    gr.mark_cell_left_state_y(nzones//2, nzones//2, r"$a_{i,j+\myhalf,L}$",
                              color="C0")
    gr.mark_cell_right_state_y(nzones//2, nzones//2+1, r"$a_{i,j+\myhalf,R}$",
                               color="C0")

    # grid labels
    gr.label_center_x(nzones//2-1, r"$i-1$")
    gr.label_center_x(nzones//2, r"$i$")
    gr.label_center_x(nzones//2+1, r"$i+1$")

    gr.label_center_y(nzones//2-1, r"$j-1$")
    gr.label_center_y(nzones//2, r"$j$")
    gr.label_center_y(nzones//2+1, r"$j+1$")


    # axes
    gr.clean_axes()
    plt.subplots_adjust(left=0.02,right=0.98,bottom=0.02,top=0.98)

    f = plt.gcf()
    f.set_size_inches(6.0,6.0)

    plt.savefig("2dgrid.png", dpi=150)

if __name__== "__main__":
    simplegrid()
