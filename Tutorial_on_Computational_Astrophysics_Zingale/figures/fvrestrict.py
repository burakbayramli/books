import numpy as np
import matplotlib.pyplot as plt
import grid_plot as gp

# plot two stacked fv grids of different (2x) resolution to show prolongation

#-----------------------------------------------------------------------------

nf = 4
nc = nf//2

grf = gp.FVGrid(nf, voff=2.0)
grc = gp.FVGrid(nc)


plt.clf()

grf.draw_grid()
grc.draw_grid()

grf.label_center(nf//2-2, r"$i-2$")
grf.label_center(nf//2-1, r"$i-1$")
grf.label_center(nf//2,   r"$i$")
grf.label_center(nf//2+1, r"$i+1$")

grc.label_center(nc//2-1, r"$j-1$")
grc.label_center(nc//2,   r"$j$")

grf.label_cell_center(nf//2-2, r"$\phi_{i-2}^f$")
grf.label_cell_center(nf//2-1, r"$\phi_{i-1}^f$")
grf.label_cell_center(nf//2,   r"$\phi_i^f$")
grf.label_cell_center(nf//2+1, r"$\phi_{i+1}^f$")

grc.label_cell_center(nc//2-1, r"$\phi_{j-1}^{c}$")
grc.label_cell_center(nc//2,   r"$\phi_{j}^{c}$")
    

# connect the dots...

plt.plot([grf.xl[nf//2-2], grf.xl[nf//2-2]], [-0.25, 3.25], ":", color="0.5")
plt.plot([grf.xl[nf//2], grf.xl[nf//2]], [-0.25, 3.25], ":", color="0.5")
plt.plot([grf.xr[nf//2+1], grf.xr[nf//2+1]], [-0.25, 3.25], ":", color="0.5")


plt.axis([grf.xmin-0.5*grf.dx,grf.xmax+0.5*grf.dx, -0.5, 3.5])
plt.axis("off")

plt.subplots_adjust(left=0.05,right=0.95,bottom=0.05,top=0.95)

f = plt.gcf()
f.set_size_inches(6.0,5.0)

plt.savefig("fvrestrict.png", dpi=200)

