
import matplotlib.pyplot as plt
import numpy as np
import grid_plot as gp

# plot a simple finite-difference grid

#-----------------------------------------------------------------------------

nzones = 8

# data that lives on the grid
#a = np.array([0.3, 1.0, 0.9, 0.8, 0.25, 0.15, 0.5, 0.55])
a = np.array([0.3, 1.0, 0.9, 0.8, 0.25, 0.1, 0.5, 0.55])

gr = gp.FDGrid(nzones)


plt.clf()

gr.draw_grid()

gr.label_node(nzones//2,   r"$i$",   fontsize="medium")
gr.label_node(nzones//2-1, r"$i-1$", fontsize="medium")
gr.label_node(nzones//2+1, r"$i+1$", fontsize="medium")
gr.label_node(nzones//2-2, r"$i-2$", fontsize="medium")
gr.label_node(nzones//2+2, r"$i+2$", fontsize="medium")


# draw the data
for i in range(nzones):
    gr.draw_data(i, a[i], color="r")


gr.label_value(nzones//2, a[nzones//2], r"$a_i$", color="r")

# label dx
gr.label_dx(gr.ng+nzones//2)

gr.clean_axes(ylim=(-0.5, 1.2))

f = plt.gcf()
f.set_size_inches(10.0,3.0)

plt.savefig("fd_grid_basic.png")
