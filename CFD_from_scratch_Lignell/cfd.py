import numpy as np
from scipy.sparse import diags
from scipy.sparse.linalg import spsolve
import matplotlib.pyplot as plt

IJ = np.ix_

def set_user_specifications():
    global nx, ny, Lx, Ly, ν, ubc_t, ubc_b, vbc_r, vbc_l, n_τ_run, cfl
    
    nx = 40          # number of grid points in x direction (P-grid)
    ny = 60          # number of grid points in the y direction (P-grid)
    Lx = 1.0          # domain length in x
    Ly = 1.5          # domain length in y
    ν  = 1.0          # kinematic viscosity
    
    ubc_t = 10.0      # u on top boundary
    ubc_b = 0.0       # u on bottom boundary
    vbc_r = 10.0      # v on right boundary
    vbc_l = 0.0       # v on left boundary
    
    n_τ_run = 2       # number of box timescales to run for
    cfl  = 0.05       # timestep size factor

def set_grid_time_vars():
    global x, y, Δx, Δy, tend, Δt, nsteps, u, v, P
    
    x  = np.linspace(0,Lx,nx)     # x-grid
    y  = np.linspace(0,Ly,ny)     # y-grid
    Δx = x[1] - x[0]              # x-grid spacing
    Δy = y[1] - y[0]              # y-grid spacing
    
    τ_box  = (Lx+Ly)/2 / np.max(np.abs([ubc_t, ubc_b, vbc_r, vbc_l]))      # box timescale
    tend   = τ_box * n_τ_run                                               # simulation run time
    Δt     = cfl*np.min([Δx,Δy])/np.max(np.abs([ubc_t,ubc_b,vbc_r,vbc_l])) # timestep size
    nsteps = int(tend/Δt)                                                  # number of timesteps
    Δt     = tend/nsteps                                                   # timestep size
    
    #-------------------- set solution variables
    
    u = np.zeros((nx+1,ny))
    v = np.zeros((nx,ny+1))               
    P = np.zeros((nx,ny))                # P = P/ρ (P_hat)

    
def get_Hu():
    """
    ue, uw, un, us, vn, vs are values on u-cell faces
    These arrays include ALL u-cells (nx+1,ny) for convenience,
    but only interior u-cells (and corresponding face values) are set.
    """
    
    ue   = np.zeros((nx+1,ny))   
    uw   = np.zeros((nx+1,ny))
    un   = np.zeros((nx+1,ny))
    us   = np.zeros((nx+1,ny))
    vn   = np.zeros((nx+1,ny))
    vs   = np.zeros((nx+1,ny))
    τxxe = np.zeros((nx+1,ny))
    τxxw = np.zeros((nx+1,ny))
    τxyn = np.zeros((nx+1,ny))
    τxys = np.zeros((nx+1,ny))
    Hu   = np.zeros((nx+1,ny))
    
    i = np.arange(1,nx)              # u-cell centers in domain interior
    
    ue[i,:] = (u[i+1,:] + u[i,:])/2
    uw[i,:] = (u[i,:]   + u[i-1,:])/2
    
    j = np.arange(0,ny-1)
    un[IJ(i,j)] = (u[IJ(i,j+1)] + u[IJ(i,j)])/2
    un[i,ny-1] = ubc_t
    j = np.arange(1,ny)
    us[IJ(i,j)] = (u[IJ(i,j)] + u[IJ(i,j-1)])/2
    us[i,0] = ubc_b
    
    j = np.arange(0,ny)
    vn[IJ(i,j)] = (v[IJ(i-1,j+1)]+v[IJ(i,j+1)])/2
    vs[IJ(i,j)] = (v[IJ(i-1,j)]  +v[IJ(i,j)])  /2
    
    τxxe[i,:] = -2*ν*(u[i+1,:] - u[i,:])  /Δx
    τxxw[i,:] = -2*ν*(u[i,:]   - u[i-1,:])/Δx
    
    j = np.arange(0,ny-1)
    τxyn[IJ(i,j)] = -ν*(u[IJ(i,j+1)]-u[IJ(i,j)])/Δy - ν*(v[IJ(i,j+1)]-v[IJ(i-1,j+1)])/Δx
    τxyn[i,ny-1]  = -ν*(ubc_t-u[i,ny-1])/(Δy/2)     - ν*(v[i,ny]-v[i-1,ny])/Δx 
    
    j = np.arange(1,ny)
    τxys[IJ(i,j)] = -ν*(u[IJ(i,j)]-u[IJ(i,j-1)])/Δy - ν*(v[IJ(i,j)]-v[IJ(i-1,j)])/Δx
    τxys[i,0]     = -ν*(u[i,0]-ubc_b)/(Δy/2)        - ν*(v[i,0]-v[i-1,0])/Δx
    
    Hu[i,:] = -((ue[i,:]*ue[i,:] - uw[i,:]*uw[i,:])/Δx + (un[i,:]*vn[i,:] - us[i,:]*vs[i,:])/Δy) \
              -((τxxe[i,:] - τxxw[i,:])/Δx + (τxyn[i,:] - τxys[i,:])/Δy)
    
    return Hu

def get_Hv():
    """
    vn, vs, ve, vw, ue, uw are values on v-cell faces
    These arrays include ALL v-cells (nx,ny+1) for convenience,
    but only interior v-cells (and corresponding face values) are set.
    """
    
    vn   = np.zeros((nx,ny+1))   
    vs   = np.zeros((nx,ny+1))
    ve   = np.zeros((nx,ny+1))
    vw   = np.zeros((nx,ny+1))
    ue   = np.zeros((nx,ny+1))
    uw   = np.zeros((nx,ny+1))
    τyyn = np.zeros((nx,ny+1))
    τyys = np.zeros((nx,ny+1))
    τyxe = np.zeros((nx,ny+1))
    τyxw = np.zeros((nx,ny+1))
    Hv   = np.zeros((nx,ny+1))
    
    j = np.arange(1,ny)              # v-cell centers in domain interior
    
    vn[:,j] = (v[:,j+1] + v[:,j])/2
    vs[:,j] = (v[:,j]   + v[:,j-1])/2
    
    i = np.arange(0,nx-1)
    ve[IJ(i,j)] = (v[IJ(i+1,j)] + v[IJ(i,j)])/2
    ve[nx-1,j] = vbc_r
    i = np.arange(1,nx)
    vw[IJ(i,j)] = (v[IJ(i,j)] + v[IJ(i-1,j)])/2
    vw[0,j] = vbc_l
    
    i = np.arange(0,nx)
    ue[IJ(i,j)] = (u[IJ(i+1,j-1)] + u[IJ(i+1,j)])/2
    uw[IJ(i,j)] = (u[IJ(i,j-1)]   + u[IJ(i,j)])  /2
    
    τyyn[:,j] = -2*ν*(v[:,j+1] - v[:,j])  /Δy
    τyys[:,j] = -2*ν*(v[:,j]   - v[:,j-1])/Δy
    
    i = np.arange(0,nx-1)
    τyxe[IJ(i,j)] = -ν*(v[IJ(i+1,j)]-v[IJ(i,j)])/Δx - ν*(u[IJ(i+1,j)]-u[IJ(i+1,j-1)])/Δy
    τyxe[nx-1,j]  = -ν*(vbc_r-v[nx-1,j])/(Δx/2)     - ν*(u[nx,j]-u[nx,j-1])/Δy 
    
    i = np.arange(1,nx)
    τyxw[IJ(i,j)] = -ν*(v[IJ(i,j)]-v[IJ(i-1,j)])/Δx - ν*(u[IJ(i,j)]-u[IJ(i,j-1)])/Δy
    τyxw[0,j]     = -ν*(v[0,j]-vbc_l)/(Δx/2)        - ν*(u[0,j]-u[0,j-1])/Δy
    
    Hv[:,j] = -((vn[:,j]*vn[:,j] - vs[:,j]*vs[:,j])/Δy + (ve[:,j]*ue[:,j] - vw[:,j]*uw[:,j])/Δx) \
              -((τyyn[:,j] - τyys[:,j])/Δy + (τyxe[:,j] - τyxw[:,j])/Δx)
    
    return Hv

def solve_P(h):
    """
    Set up and solve the AP=b system, where A is a matrix, P (=Phat) and b are vectors.
    """
    
    
    nP = nx*ny     # total grid points solved (all P-grid cells)
    
    b    = np.zeros((nx,ny))           # set below
    cP   = np.zeros((nx,ny))           # coefficient of P_i,j; set below
    cPjm = np.full((nx,ny),-h*Δx/Δy)   # coefficient of P_i,j-1; initialized here, specialized below
    cPim = np.full((nx,ny),-h*Δy/Δx)   # coefficient of P_i-1,j; initialized here, specialized below
    cPip = np.full((nx,ny),-h*Δy/Δx)   # coefficient of P_i+1,j; initialized here, specialized below
    cPjp = np.full((nx,ny),-h*Δx/Δy)   # coefficient of P_i,j+1; initialized here, specialized below
    
    #-------------------- 
    
    # Interior
    i = np.arange(1,nx-1); j = np.arange(1,ny-1)
    b[IJ(i,j)]    = -Δy*(u[IJ(i+1,j)]+h*Hu[IJ(i+1,j)]) + Δy*(u[IJ(i,j)]+h*Hu[IJ(i,j)]) - Δx*(v[IJ(i,j+1)]+h*Hv[IJ(i,j+1)]) + Δx*(v[IJ(i,j)]+h*Hv[IJ(i,j)])
    cP[IJ(i,j)]   =  2*h*Δy/Δx + 2*h*Δx/Δy
    
    # Corner bottom left
    i = 0; j = 0
    b[i,j]    = -Δy*(u[i+1,j]+h*Hu[i+1,j])            + Δy*u[i,j]                     - Δx*(v[i,j+1]+h*Hv[i,j+1])          + Δx*v[i,j]
    cP[i,j]   =  h*Δy/Δx + h*Δx/Δy
    cPjm[i,j] =  0.0
    cPim[i,j] =  0.0
    
    # Side bottom
    i = np.arange(1,nx-1); j = 0
    b[i,j]    = -Δy*(u[i+1,j]+h*Hu[i+1,j])            + Δy*(u[i,j]+h*Hu[i,j])         - Δx*(v[i,j+1]+h*Hv[i,j+1])          + Δx*v[i,j]
    cP[i,j]   =  2*h*Δy/Δx + h*Δx/Δy
    cPjm[i,j] =  0.0
    
    # Corner bottom right
    i = nx-1; j = 0
    b[i,j]    = -Δy*u[i+1,j]                          + Δy*(u[i,j]+h*Hu[i,j])         - Δx*(v[i,j+1]+h*Hv[i,j+1])          + Δx*v[i,j]
    cP[i,j]   =  h*Δy/Δx + h*Δx/Δy
    cPjm[i,j] =  0.0
    cPip[i,j] =  0.0
    
    # Side left
    i = 0; j = np.arange(1,ny-1)
    b[i,j]    = -Δy*(u[i+1,j]+h*Hu[i+1,j])            + Δy*u[i,j]                     - Δx*(v[i,j+1]+h*Hv[i,j+1])          + Δx*(v[i,j]+h*Hv[i,j])
    cP[i,j]   =  h*Δy/Δx + 2*h*Δx/Δy
    cPim[i,j] =  0.0
    
    # Side right
    i = nx-1; j = np.arange(1,ny-1)
    b[i,j]    = -Δy*u[i+1,j]                          + Δy*(u[i,j]+h*Hu[i,j])         - Δx*(v[i,j+1]+h*Hv[i,j+1])          + Δx*(v[i,j]+h*Hv[i,j])
    cP[i,j]   =  h*Δy/Δx + 2*h*Δx/Δy
    cPip[i,j] =  0.0
    
    # Corner top left
    i = 0; j = ny-1
    b[i,j]    = -Δy*(u[i+1,j]+h*Hu[i+1,j])            + Δy*u[i,j]                     - Δx*v[i,j+1]                        + Δx*(v[i,j]+h*Hv[i,j])
    cP[i,j]   =  h*Δy/Δx + h*Δx/Δy
    cPim[i,j] =  0.0
    cPjp[i,j] =  0.0
    
    # Side top
    i = np.arange(1,nx-1); j = ny-1
    b[i,j]    = -Δy*(u[i+1,j]+h*Hu[i+1,j])            + Δy*(u[i,j]+h*Hu[i,j])         - Δx*v[i,j+1]                        + Δx*(v[i,j]+h*Hv[i,j])
    cP[i,j]   =  2*h*Δy/Δx + h*Δx/Δy
    cPjp[i,j] =  0.0
    
    # Corner top right
    i = nx-1; j = ny-1
    b[i,j]    = -Δy*u[i+1,j]                          + Δy*(u[i,j]+h*Hu[i,j])         - Δx*v[i,j+1]                        + Δx*(v[i,j]+h*Hv[i,j])
    cP[i,j]   =  h*Δy/Δx + h*Δx/Δy
    cPip[i,j] =  0.0
    cPjp[i,j] =  0.0
    
    #---------------------------------
    
    b    = np.reshape(b,    nP, order='F')
    
    cP   = np.reshape(cP,   nP, order='F')
    cPjm = np.reshape(cPjm, nP, order='F')
    cPim = np.reshape(cPim, nP, order='F')
    cPip = np.reshape(cPip, nP, order='F')
    cPjp = np.reshape(cPjp, nP, order='F')
    
    A = diags([cPjm[nx:], cPim[1:], cP, cPip[:-1], cPjp[:-nx]], [-nx, -1, 0, 1, nx], format='csr')
    
    #---------------------------------
    
    P = spsolve(A,b)
    
    P -= np.average(P)
    
    P = np.reshape(P, (nx,ny),order='F')
    
    return P
    
set_user_specifications()
set_grid_time_vars()

ke = np.zeros(nsteps+1)
times = np.linspace(0,tend,nsteps+1)

for k in range(nsteps):
    
    Hu = get_Hu()
    Hv = get_Hv()
    
    P = solve_P(Δt)
    
    i = np.arange(1,nx)
    u[i,:] = u[i,:] + Δt*Hu[i,:] - Δt*(P[i,:]-P[i-1,:])/Δx
    
    j = np.arange(1,ny)
    v[:,j] = v[:,j] + Δt*Hv[:,j] - Δt*(P[:,j]-P[:,j-1])/Δy
    
    #-----------------------------
    
    U = (u[:-1,:] + u[1:,:])/2
    V = (v[:,:-1] + v[:,1:])/2
    velmag = np.sqrt(U*U + V*V)
    ke[k+1] = 0.5*(np.average(velmag**2))
    
    
#----------- interpolate velocities to the P-grid

U = (u[:-1,:] + u[1:,:])/2    # u-velocity on the P-grid
V = (v[:,:-1] + v[:,1:])/2    # v-velocity on the P-grid
velmag = np.sqrt(U*U + V*V)   # velocity magnitude.

X,Y = np.meshgrid(x,y)

plt.rc('font', size=14)
fig, (ax1, ax2) = plt.subplots(1,2, figsize=(10,10))

ax1.set_aspect('equal', adjustable='box')
ax1.streamplot(x,y,U.T,V.T, density=2.5, linewidth=1, arrowsize=0.001, color=velmag.T)
ax1.set_title(r'$|\vec{v}|$')
ax1.set_xlim([0,Lx])
ax1.set_ylim([0,Ly])
ax1.set_xticks([])
ax1.set_yticks([]);

ax2.set_aspect('equal', adjustable='box')
ax2.contourf(X,Y,P.T,40)
ax2.set_title(r'$P/\rho$')
ax2.set_xlim([0,Lx])
ax2.set_ylim([0,Ly])
ax2.set_xticks([])
ax2.set_yticks([]);

plt.figure(figsize=(3.5,3))
plt.plot(times,ke)
plt.xlabel('time')
plt.ylabel('KE');

plt.show()

