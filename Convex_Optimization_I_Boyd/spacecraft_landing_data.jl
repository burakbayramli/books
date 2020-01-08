h = 1;
g = 0.1;
m = 10;
Fmax = 10;
p0 = [50,50,100];
v0 = [-10,0,-10];
alpha = 0.5;
gamma = 1;
K = 35;

# use the following code to plot your trajectories
# plot the glide cone (don't modify)
# -------------------------------------------------------
# using PyPlot
# x = linspace(-40,55,30); y = linspace(0,55,30);
# X = repmat(x', length(x), 1);
# Y = repmat(y, 1, length(y));
# Z = alpha*sqrt(X.^2+Y.^2);
# figure();
# grid(true);
# hold(true);
# surf(X, Y, Z, cmap=get_cmap("autumn"));
# xlim([-40, 55]);
# ylim([0, 55]);
# zlim([0, 105]);

# INSERT YOUR VARIABLES HERE:
# -------------------------------------------------------
# Make sure you pass plot and quiver column vectors and
# not row vectors. To plot 3D quiver plots you need to be
# on matplotlib 1.4. Quiver plots in matplotlib are also
# specified by the location of the arrow HEADS and the
# direction, instead of the arrow base
#
# plot(p[:,1], p[:,2], p[:,3]);
# heads = p[1:K,:] + f;
# quiver(heads[:,1], heads[:,2], heads[:,3],
#   f[:,1], f[:,2], f[:,3], length=10);
