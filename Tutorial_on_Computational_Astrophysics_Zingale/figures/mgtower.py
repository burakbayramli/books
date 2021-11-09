import matplotlib.pyplot as plt
import grid_plot as gp

# plot two stacked fv grids of different (2x) resolution to show prolongation

#-----------------------------------------------------------------------------

gr = []

nzones = [2, 4, 8, 16]
for nf in nzones:
    gr.append(gp.FVGrid(nf, ng=1, voff=2.0*len(gr)))


plt.clf()

for g in gr:
    g.draw_grid(emphasize_end=1, draw_ghost=1, edge_ticks=0)

f = plt.gcf()
f.set_size_inches(7.0,5.0)

grf = gr[0]
plt.xlim(grf.xmin-1.1*grf.dx,grf.xmax+1.1*grf.dx)

plt.axis("off")

plt.subplots_adjust(left=0.05,right=0.95,bottom=0.05,top=0.95)

plt.savefig("mgtower.png", dpi=200)

