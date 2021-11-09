
import matplotlib.pyplot as plt
import numpy as np
import grid_plot as gp


def func(x):
    return 0.1*x**3 - 0.25*x + 0.5*x + 0.5 + 0.25*np.sin(3*np.pi*x)

# plot a simple finite-difference grid

#-----------------------------------------------------------------------------

nzones = 8

# data that lives on the grid

gr = gp.FDGrid(nzones)

a = func(gr.xc)

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

# draw a line with the "true" data
xfine = np.linspace(gr.xmin, gr.xmax, 1000)

plt.plot(xfine, func(xfine), color="red", alpha=0.5)

gr.label_value(nzones//2, a[nzones//2], r"$f_i$", color="r")
gr.label_value(nzones//2+1, a[nzones//2+1], r"$f_{i+1}$", color="r")
gr.label_value(nzones//2-1, a[nzones//2-1], r"$f_{i-1}$", color="r")

# label dx
gr.label_dx(gr.ng+nzones//2)

gr.clean_axes(ylim=(-0.5, 1.2))

f = plt.gcf()
f.set_size_inches(10.0,3.0)

plt.savefig("fd_grid.png")
