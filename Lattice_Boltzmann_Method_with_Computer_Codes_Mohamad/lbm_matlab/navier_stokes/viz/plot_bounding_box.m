function plot_bounding_box(bmin, bmax)

plot([bmin(1), bmax(1)], [bmin(2), bmin(2)]);
plot([bmax(1), bmax(1)], [bmin(2), bmax(2)]);
plot([bmin(1), bmax(1)], [bmax(2), bmax(2)]);
plot([bmin(1), bmin(1)], [bmin(2), bmax(2)]);

