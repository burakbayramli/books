"""
2-d approximate projection on a cell-centered grid with periodic BCs 

M. Zingale

"""

import sys
import numpy as np
import matplotlib.pyplot as plt

class Grid(object):

    def __init__(self, nx, ny, ng=1, 
                 xmin=0.0, xmax=1.0,
                 ymin=0.0, ymax=1.0):

        self.nx = nx
        self.ny = ny
        self.ng = ng
        
        self.ilo = ng
        self.ihi = ng+nx-1

        self.jlo = ng
        self.jhi = ng+ny-1

        self.dx = (xmax - xmin)/nx
        self.dy = (ymax - ymin)/ny

        self.x = (np.arange(nx+2*ng) - ng + 0.5)*self.dx + xmin
        self.y = (np.arange(ny+2*ng) - ng + 0.5)*self.dy + ymin

        x2d = np.repeat(self.x, 2*ng+ny)
        x2d.shape = (2*ng+nx, 2*ng+ny)
        self.x2d = x2d

        y2d = np.repeat(self.y, 2*ng+nx)
        y2d.shape = (2*ng+ny, 2*ng+nx)
        y2d = np.transpose(y2d)
        self.y2d = y2d

        self.phi = np.zeros((nx + 2*ng, ny + 2*ng), dtype=np.float64)

    def scratch_array(self):
        return np.zeros((self.nx + 2*self.ng, self.ny + 2*self.ng), dtype=np.float64)

    def fill_BC(self, v):
        """ fill periodic BCs """

        if not self.ng == 1:
            sys.exit("invalid ng")

        v[self.ilo-1,:] = v[self.ihi,:]
        v[self.ihi+1,:] = v[self.ilo,:]

        v[:,self.jlo-1] = v[:,self.jhi]
        v[:,self.jhi+1] = v[:,self.jlo]
        
    def norm(self, e):
        """ L2 norm """

        #return np.sqrt(self.dx*self.dy*np.sum((e[gr.ilo:gr.ihi+1,gr.jlo:gr.jhi+1]**2).flat))
        return np.max(np.abs(e))
        
def residual(gr, f):
    """ compute the residual """

    ib = gr.ilo
    ie = gr.ihi
    jb = gr.jlo
    je = gr.jhi

    r = gr.scratch_array()

    r[ib:ie+1,jb:je+1] = f[ib:ie+1,jb:je+1] - \
        (gr.phi[ib-1:ie,jb:je+1] - 2.0*gr.phi[ib:ie+1,jb:je+1] + gr.phi[ib+1:ie+2,jb:je+1])/gr.dx**2 - \
        (gr.phi[ib:ie+1,jb-1:je] - 2.0*gr.phi[ib:ie+1,jb:je+1] + gr.phi[ib:ie+1,jb+1:je+2])/gr.dy**2 
    
    return r

def smooth(gr, f, eps):

    ib = gr.ilo
    ie = gr.ihi
    jb = gr.jlo
    je = gr.jhi

    fnorm = gr.norm(f)
    rnorm = 1.e10

    i = 0
    while (rnorm > eps*fnorm and i < 25000):

        gr.fill_BC(gr.phi)

        gr.phi[ib:ie+1:2,jb:je+1:2] = \
            0.25*(gr.phi[ib+1:ie+2:2,jb  :je+1:2] + \
                  gr.phi[ib-1:ie  :2,jb  :je+1:2] + \
                  gr.phi[ib  :ie+1:2,jb+1:je+2:2] + \
                  gr.phi[ib  :ie+1:2,jb-1:je  :2] - \
                  gr.dx**2*f[ib:ie+1:2,jb:je+1:2])

        gr.phi[ib+1:ie+1:2,jb+1:je+1:2] = \
            0.25*(gr.phi[ib+2:ie+2:2,jb+1:je+1:2] + \
                  gr.phi[ib  :ie  :2,jb+1:je+1:2] + \
                  gr.phi[ib+1:ie+1:2,jb+2:je+2:2] + \
                  gr.phi[ib+1:ie+1:2,jb  :je  :2] - \
                  gr.dx**2*f[ib+1:ie+1:2,jb+1:je+1:2])

        gr.fill_BC(gr.phi)

        gr.phi[ib+1:ie+1:2,jb:je+1:2] = \
            0.25*(gr.phi[ib+2:ie+2:2,jb  :je+1:2] + \
                  gr.phi[ib  :ie  :2,jb  :je+1:2] + \
                  gr.phi[ib+1:ie+1:2,jb+1:je+2:2] + \
                  gr.phi[ib+1:ie+1:2,jb-1:je  :2] - \
                  gr.dx**2*f[ib+1:ie+1:2,jb:je+1:2])

        gr.phi[ib:ie+1:2,jb+1:je+1:2] = \
            0.25*(gr.phi[ib+1:ie+2:2,jb+1:je+1:2] + \
                  gr.phi[ib-1:ie  :2,jb+1:je+1:2] + \
                  gr.phi[ib  :ie+1:2,jb+2:je+2:2] + \
                  gr.phi[ib  :ie+1:2,jb  :je  :2] - \
                  gr.dx**2*f[ib:ie+1:2,jb+1:je+1:2])


        rnorm = gr.norm(residual(gr, f))
        #print rnorm, gr.phi[gr.nx/2,gr.ny/2]

        i += 1


#-----------------------------------------------------------------------------
# an analytic test problem for the smoothing

def true(x,y):
    pi = np.pi
    return np.sin(2.0*pi*x)**2*np.cos(4.0*pi*y) + \
           np.sin(4.0*pi*x)*np.cos(2.0*pi*y)**2


# the righthand side                                                            
def frhs(x,y):
    pi = np.pi
    return 8.0*pi**2*np.cos(4.0*pi*y)*(np.cos(4.0*pi*x) -
                                       np.sin(4.0*pi*x)) - \
           16.0*pi**2*(np.sin(4.0*pi*x)*np.cos(2.0*pi*y)**2 +
                       np.sin(2.0*pi*x)**2 * np.cos(4.0*pi*y))

#-----------------------------------------------------------------------------
# projection example

def udivfree(gr):
    u = -np.sin(np.pi*gr.x2d)**2*np.sin(2.0*np.pi*gr.y2d)
    v =  np.sin(np.pi*gr.y2d)**2*np.sin(2.0*np.pi*gr.x2d)

    return u, v

def phif(gr):
    return 0.1*np.cos(2.0*np.pi*gr.y2d)*np.cos(2.0*np.pi*gr.x2d)

def gradphi(gr, phi):
    gphi_x = gr.scratch_array()
    gphi_y = gr.scratch_array()

    gphi_x[gr.ilo:gr.ihi+1,gr.jlo:gr.jhi+1] = \
        0.5*(phi[gr.ilo+1:gr.ihi+2,gr.jlo:gr.jhi+1] - \
             phi[gr.ilo-1:gr.ihi  ,gr.jlo:gr.jhi+1])/gr.dx

    gphi_y[gr.ilo:gr.ihi+1,gr.jlo:gr.jhi+1] = \
        0.5*(phi[gr.ilo:gr.ihi+1,gr.jlo+1:gr.jhi+2] - \
             phi[gr.ilo:gr.ihi+1,gr.jlo-1:gr.jhi  ])/gr.dy

    return gphi_x, gphi_y

def divU(gr, u, v):
    dU = gr.scratch_array()
    dU[gr.ilo:gr.ihi+1,gr.jlo:gr.jhi+1] = \
        0.5*(u[gr.ilo+1:gr.ihi+2,gr.jlo:gr.jhi+1] - \
             u[gr.ilo-1:gr.ihi  ,gr.jlo:gr.jhi+1])/gr.dx + \
        0.5*(v[gr.ilo:gr.ihi+1,gr.jlo+1:gr.jhi+2] - \
             v[gr.ilo:gr.ihi+1,gr.jlo-1:gr.jhi  ])/gr.dy

    return dU

#-----------------------------------------------------------------------------

nx = 128
ny = 128

gr = Grid(nx, ny)

gr.phi[:] = 0.0


# get the original divergence-free field
ud, vd = udivfree(gr)

udOrig = ud.copy()
vdOrig = vd.copy()


# add the gradient of a scalar
phi = phif(gr)
phiOrig = phi.copy()

gpx, gpy = gradphi(gr, phi)

# pollute the velocity field
ud += gpx
gr.fill_BC(ud)

vd += gpy
gr.fill_BC(vd)


f = divU(gr, ud, vd)

smooth(gr, f, 1.e-7)

# correct the velocity field to recover the divergence free part
gpx, gpy = gradphi(gr, gr.phi)

unew = ud - gpx
vnew = vd - gpy


#-----------------------------------------------------------------------------
# plots

plt.subplot(131)

plt.imshow(np.transpose(phiOrig[gr.ilo:gr.ihi+1,gr.jlo:gr.jhi+1]),
             interpolation="nearest", origin="lower")
plt.colorbar()
plt.title("original phi")


plt.subplot(132)

plt.imshow(np.transpose(gr.phi[gr.ilo:gr.ihi+1,gr.jlo:gr.jhi+1]),
             interpolation="nearest", origin="lower")
plt.colorbar()
plt.title("new phi")

plt.subplot(133)

phie = gr.phi - phiOrig

plt.imshow(np.transpose(phie[gr.ilo:gr.ihi+1,gr.jlo:gr.jhi+1]),
             interpolation="nearest", origin="lower")
plt.colorbar()
plt.title("phi error")


f = plt.gcf()
f.set_size_inches(10.0,4.0)

plt.tight_layout()

plt.savefig("project-phi.png")


# velocities

plt.subplot(321)

plt.imshow(np.transpose(udOrig[gr.ilo:gr.ihi+1,gr.jlo:gr.jhi+1]),
             interpolation="nearest", origin="lower")
plt.colorbar()
plt.title("original u")


plt.subplot(322)

plt.imshow(np.transpose(vdOrig[gr.ilo:gr.ihi+1,gr.jlo:gr.jhi+1]),
             interpolation="nearest", origin="lower")
plt.colorbar()
plt.title("original v")


plt.subplot(323)

plt.imshow(np.transpose(ud[gr.ilo:gr.ihi+1,gr.jlo:gr.jhi+1]),
             interpolation="nearest", origin="lower")
plt.colorbar()
plt.title("\'polluted\' u")


plt.subplot(324)

plt.imshow(np.transpose(vd[gr.ilo:gr.ihi+1,gr.jlo:gr.jhi+1]),
             interpolation="nearest", origin="lower")
plt.colorbar()
plt.title("\'polluted\' v")


plt.subplot(325)

plt.imshow(np.transpose(unew[gr.ilo:gr.ihi+1,gr.jlo:gr.jhi+1]),
             interpolation="nearest", origin="lower")
plt.colorbar()
plt.title("projected u")


plt.subplot(326)

plt.imshow(np.transpose(vnew[gr.ilo:gr.ihi+1,gr.jlo:gr.jhi+1]),
             interpolation="nearest", origin="lower")
plt.colorbar()
plt.title("projected v")

f = plt.gcf()
f.set_size_inches(6.0,8.0)

plt.tight_layout()


plt.savefig("project-u.png")


# compute the error
eu = unew - udOrig
ev = vnew - vdOrig

print "Nx, Ny, L2 norm of error (u, v): ", nx, ny, \
    np.sqrt(gr.dx*gr.dy*np.sum((eu[gr.ilo:gr.ihi+1,gr.jlo:gr.jhi+1]**2).flat)), \
    np.sqrt(gr.dx*gr.dy*np.sum((ev[gr.ilo:gr.ihi+1,gr.jlo:gr.jhi+1]**2).flat))
