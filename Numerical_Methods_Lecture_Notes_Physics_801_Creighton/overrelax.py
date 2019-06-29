import math, pylab, mpl_toolkits.mplot3d

eps = 1e-5  # fractional error allowed
L = 1.0  # length of each side
N = input('number of grid points on a side -> ')
dy = dx = L/(N-1.0)
x = pylab.array(range(N))*dx
y = pylab.array(range(N))*dy
(x, y) = pylab.meshgrid(x, y)
u = pylab.zeros((N, N))

# boundary conditions
for j in range(N):
    u[j,N-1] = 1.0

# compute over-relaxation parameter
omega = 2.0/(1.0+math.sin(math.pi*dx/L))

# white and black pixels: white have j+k even; black have j+k odd
white = [(j, k) for j in range(1, N-1) for k in range(1, N-1) if (j+k)%2 == 0]
black = [(j, k) for j in range(1, N-1) for k in range(1, N-1) if (j+k)%2 == 1]

# prepare animated plot
pylab.ion()
image = pylab.imshow(u.T, origin='lower', extent=(0.0, L, 0.0, L))

n = 0  # number of iterations
err = 1.0  # average error per site
while err > eps:
    # update animated plot
    image.set_data(u.T)
    pylab.title('iteration %d'%n)
    pylab.draw()

    # next iteration in refinement
    n = n+1
    err = 0.0
    for (j, k) in white+black:  # loop over white pixels then black pixels
        du = (u[j-1,k]+u[j+1,k]+u[j,k-1]+u[j,k+1])/4.0-u[j,k]
        u[j,k] += omega*du
        err += abs(du)
    err /= N**2

# surface plot of final solution
pylab.ioff()
fig = pylab.figure()
axis = fig.gca(projection='3d', azim=-60, elev=20)
surf = axis.plot_surface(x, y, u.T, rstride=1, cstride=1, linewidth=0,
                         cmap=pylab.cm.jet)
wire = axis.plot_wireframe(x, y, u.T, rstride=1+N//50, cstride=1+N//50,
                           linewidth=0.25)
axis.contour(x, y, u.T, 10, zdir='z', offset=-1.0)
axis.set_xlabel('x')
axis.set_ylabel('y')
axis.set_zlabel('u')
axis.set_zlim(-1.0, 1.0)
fig.colorbar(surf)
pylab.show()
