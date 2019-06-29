import math, pylab, mpl_toolkits.mplot3d

eps = 1e-5  # fractional error allowed
L = 1.0  # length of each side
N = input('number of grid points on a side -> ')
dy = dx = L/(N-1.0)
x = pylab.array(range(N))*dx
y = pylab.array(range(N))*dy
(x, y) = pylab.meshgrid(x, y)
u0 = pylab.zeros((N, N))
u1 = pylab.zeros((N, N))

# boundary conditions
for j in range(N):
    u1[j,N-1] = u0[j,N-1] = 1.0

# prepare animated plot
pylab.ion()
image = pylab.imshow(u0.T, origin='lower', extent=(0.0, L, 0.0, L))

n = 0  # number of iterations
err = 1.0  # average error per site
while err > eps:
    # update animated plot
    image.set_data(u0.T)
    pylab.title('iteration %d'%n)
    pylab.draw()

    # next iteration in refinement
    n = n+1
    err = 0.0
    for j in range(1, N-1):
        for k in range(1, N-1):
            u1[j,k] = (u0[j-1,k]+u0[j+1,k]+u0[j,k-1]+u0[j,k+1])/4.0
            err += abs(u1[j,k]-u0[j,k])
    err /= N**2
    (u0, u1) = (u1, u0)  # swap old and new arrays for next iteration

# surface plot of final solution
pylab.ioff()
fig = pylab.figure()
axis = fig.gca(projection='3d', azim=-60, elev=20)
surf = axis.plot_surface(x, y, u0.T, rstride=1, cstride=1, cmap=pylab.cm.jet)
axis.contour(x, y, u0.T, 10, zdir='z', offset=-1.0)
axis.set_xlabel('x')
axis.set_ylabel('y')
axis.set_zlabel('u')
axis.set_zlim(-1.0, 1.0)
fig.colorbar(surf)
pylab.show()
