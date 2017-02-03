from scitools.std import *

def diff2Dmesh(f, xmin, xmax, ymin, ymax, nx, ny):
    x = linspace(xmin, xmax, nx+1)
    y = linspace(ymin, ymax, ny+1)
    hx = (xmax - xmin)/float(nx)
    hy = (ymax - ymin)/float(ny)
    z   = zeros((nx+1, ny+1))  # f at mesh points
    for i in xrange(nx+1):
        for j in xrange(ny+1):
            z[i,j] = f(x[i], y[j])

    dfx = zeros((nx+1, ny+1))  # df/dx at mesh points
    for i in xrange(nx):
        for j in xrange(ny+1):
            dfx[i,j] = (z[i+1,j] - z[i,j])/hx

    dfy = zeros((nx+1, ny+1))  # df/dy at mesh points
    for i in xrange(nx+1):
        for j in xrange(ny):
            dfy[i,j] = (z[i,j+1] - z[i,j])/hy
    # Special cases for i=nx and j=ny:
    i = nx
    for j in xrange(ny+1):
        dfx[i, j] = (z[i,j]- z[i-1,j])/hx
    j = ny
    for i in xrange(nx+1):
        dfy[i, j] = (z[i,j] - z[i,j-1])/hy
    return x, y, dfx, dfy
