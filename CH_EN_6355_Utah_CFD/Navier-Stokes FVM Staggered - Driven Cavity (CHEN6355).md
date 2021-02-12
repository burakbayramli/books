# A Simple Staggered FV Code for the Navier-Stokes Equations
### Tony Saad <br/> Assistant Professor of Chemical Engineering <br/> University of Utah


```python
import numpy as np
%matplotlib inline
%config InlineBackend.figure_format = 'svg'
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib import cm
plt.rcParams['animation.html'] = 'html5'
import time
```


```python
# define some boiler plate
nx = 16
ny = 16
# dt = 1.0e-3 # timestep size
ν = 0.01 # dynamic viscosity
lx = 1.0
ly = 1.0
dx = lx/nx
dy = ly/ny
t = 0.0

# cell centered coordinates
xx = np.linspace(dx/2.0,lx - dx/2.0,nx, endpoint=True)
yy = np.linspace(dy/2.0,ly - dy/2.0,ny, endpoint=True)
xcc, ycc = np.meshgrid(xx,yy)

# x-staggered coordinates
xxs = np.linspace(0,lx,nx+1, endpoint=True)
xu,yu = np.meshgrid(xxs, yy)

# y-staggered coordinates
yys = np.linspace(0,ly,ny+1, endpoint=True)
xv,yv = np.meshgrid(xx, yys)

Ut = 10.0
Ub = 0.0
Vl = 0.0
Vr = 0.0
print('Reynolds Number:', Ut*lx/ν)
dt = min(0.25*dx*dx/ν, 4.0*ν/Ut/Ut)
print('dt=', dt)

# initialize velocities - we stagger everything in the negative direction. A scalar cell owns its minus face, only.
# Then, for example, the u velocity field has a ghost cell at x0 - dx and the plus ghost cell at lx
u = np.zeros([ny+2, nx+2]) # include ghost cells

# # same thing for the y-velocity component
v = np.zeros([ny +2, nx+2]) # include ghost cells

ut = np.zeros_like(u)
vt = np.zeros_like(u)    

# initialize the pressure
p = np.zeros([nx+2,ny+2]); # include ghost cells

# a bunch of lists for animation purposes
usol=[]
usol.append(u)

vsol=[]
vsol.append(v)

psol = []
psol.append(p)
```

    Reynolds Number: 1000.0
    dt= 0.0004



```python
# build pressure coefficient matrix
Ap = np.zeros([ny+2,nx+2])
Ae = 1.0/dx/dx*np.ones([ny+2,nx+2])
As = 1.0/dy/dy*np.ones([ny+2,nx+2])
An = 1.0/dy/dy*np.ones([ny+2,nx+2])
Aw = 1.0/dx/dx*np.ones([ny+2,nx+2])
# set left wall coefs
Aw[1:-1,1] = 0.0
# set right wall coefs
Ae[1:-1,-2] = 0.0
# set top wall coefs
An[-2,1:-1] = 0.0
# set bottom wall coefs
As[1,1:-1] = 0.0
Ap = -(Aw + Ae + An + As)

def pressure_poisson2(p, b, dx, dy):
    pn = np.empty_like(p)
    it = 0
    err = 1e5
    tol = 1e-8
    maxit = 100
    β = 1.1
    while err > tol and it < maxit:
        pn = p.copy()    
        for i in range(1,nx+1):
            for j in range(1,ny+1):
                ap = Ap[j,i]
                an = An[j,i]
                aso = As[j,i]
                ae = Ae[j,i]
                aw = Aw[j,i]
                rhs = b[j,i] - 1.0*(ae*p[j,i+1] + aw*p[j,i-1] + an*p[j+1,i] + aso*p[j-1,i])
                p[j,i] = β*rhs/ap + (1-β)*p[j,i]
        err = np.linalg.norm(p - pn)
        it += 1
#     print('Poisson Error:', err)        
    return p, err
```


```python
# USE sparse solver
import scipy.linalg
import scipy.sparse
import scipy.sparse.linalg
# build pressure coefficient matrix
Ap = np.zeros([ny,nx])
Ae = 1.0/dx/dx*np.ones([ny,nx])
As = 1.0/dy/dy*np.ones([ny,nx])
An = 1.0/dy/dy*np.ones([ny,nx])
Aw = 1.0/dx/dx*np.ones([ny,nx])
# set left wall coefs
Aw[:,0] = 0.0
# set right wall coefs
Ae[:,-1] = 0.0
# set top wall coefs
An[-1,:] = 0.0
# set bottom wall coefs
As[0,:] = 0.0
Ap = -(Aw + Ae + An + As)

n = nx*ny
d0 = Ap.reshape(n)
# print(d0)
de = Ae.reshape(n)[:-1]
# print(de)
dw = Aw.reshape(n)[1:]
# print(dw)
ds = As.reshape(n)[nx:]
# print(ds)
dn = An.reshape(n)[:-nx]
# print(dn)
A1 = scipy.sparse.diags([d0, de, dw, dn, ds], [0, 1, -1, nx, -nx], format='csr')
plt.matshow((A1.toarray()))
```




    <matplotlib.image.AxesImage at 0x1186ab860>




    
![svg](output_4_1.svg)
    



```python
# while t < tend:
t0 = time.clock()
momtime = 0.0
solvertime = 0.0
nsteps = 10
for n in range(0,nsteps):
    # left wall
    u[1:-1,1] = 0.0
    # right wall
    u[1:-1,-1] = 0.0
    # top wall
    u[-1,1:] = 2.0*Ut - u[-2,1:]
    # bottom wall
    u[0,1:] = 2.0*Ub - u[1,1:]

    # left wall
    v[1:,0] = 2.0*Vl - v[1:,1]
    # right wall
    v[1:,-1] = 2.0*Vr - v[1:,-2]
    # bottom wall
    v[1,1:-1] = 0.0
    # top wall
    v[-1,1:-1] = 0.0    
  

    # do x-momentum first - u is of size (nx + 2) x (ny + 2) - only need to do the interior points
    tic = time.clock()
    for i in range(2,nx+1):
        for j in range(1,ny+1):
            ue = 0.5*(u[j,i+1] + u[j,i])
            uw = 0.5*(u[j,i]   + u[j,i-1])    
            
            un = 0.5*(u[j+1,i] + u[j,i])
            us = 0.5*(u[j,i] + u[j-1,i])            
            
            vn = 0.5*(v[j+1,i] + v[j+1,i-1])
            vs = 0.5*(v[j,i] + v[j,i-1])
            
            # convection = - d(uu)/dx - d(vu)/dy
            convection = - (ue*ue - uw*uw)/dx - (un*vn - us*vs)/dy
            
            # diffusion = d2u/dx2 + d2u/dy2
            diffusion = ν*( (u[j,i+1] - 2.0*u[j,i] + u[j,i-1])/dx/dx + (u[j+1,i] - 2.0*u[j,i] + u[j-1,i])/dy/dy )
            
            ut[j,i] = u[j,i] + dt *(convection + diffusion)
                
    # do y-momentum - only need to do interior points
    for i in range(1,nx+1):
        for j in range(2,ny+1):
            ve = 0.5*(v[j,i+1] + v[j,i])
            vw = 0.5*(v[j,i] + v[j,i-1])    
            
            ue = 0.5*(u[j,i+1] + u[j-1,i+1])
            uw = 0.5*(u[j,i] + u[j-1,i])
            
            vn = 0.5*(v[j+1,i] + v[j,i])
            vs = 0.5*(v[j,i] + v[j-1,i])            

            # convection = d(uv)/dx + d(vv)/dy
            convection = - (ue*ve - uw*vw)/dx - (vn*vn - vs*vs)/dy
            
            # diffusion = d2u/dx2 + d2u/dy2
            diffusion = ν*( (v[j,i+1] - 2.0*v[j,i] + v[j,i-1])/dx/dx + (v[j+1,i] - 2.0*v[j,i] + v[j-1,i])/dy/dy )
            
            vt[j,i] = v[j,i] + dt*(convection + diffusion)            
    # do pressure - prhs = 1/dt * div(uhat)
    # we will only need to fill the interior points. This size is for convenient indexing
    divut = np.zeros([ny+2,nx+2]) 
#     for i in range(1,nx+1):
#         for j in range(1,ny+1):
#             divutilde[j,i] = (ut[j,i+1] - ut[j,i])/dx + (vt[j+1,i] - vt[j,i])/dy
    divut[1:-1,1:-1] = (ut[1:-1,2:] - ut[1:-1,1:-1])/dx + (vt[2:,1:-1] - vt[1:-1,1:-1])/dy

    prhs = 1.0/dt * divut
    toc = time.clock()
    momtime += (toc - tic)
    
    ###### Use the direct solver
#     tic=time.clock()    
#     pressure_poisson2(p,prhs,dx,dy)
#     toc=time.clock()
#     solvertime += toc - tic
#     psol.append(p)

    
    
    ###### Use the sparse linear solver
    tic=time.clock()
#     pt = scipy.sparse.linalg.spsolve(A1,prhs[1:-1,1:-1].ravel()) #theta=sc.linalg.solve_triangular(A,d)
    pt,info = scipy.sparse.linalg.bicg(A1,prhs[1:-1,1:-1].ravel(),tol=1e-10) #theta=sc.linalg.solve_triangular(A,d)
    toc=time.clock()
    solvertime += toc - tic
    p = np.zeros([ny+2,nx+2])
    p[1:-1,1:-1] = pt.reshape([ny,nx])

    # time advance
    u[1:-1,2:-1] = ut[1:-1,2:-1] - dt * (p[1:-1,2:-1] - p[1:-1,1:-2])/dx
    v[2:-1,1:-1] = vt[2:-1,1:-1] - dt * (p[2:-1,1:-1] - p[1:-2,1:-1])/dy       
    
        
#     # Check mass residual
#     divunp1 = np.zeros([ny+2,nx+2])
#     for i in range(1,nx+1):
#         for j in range(1,ny+1):
#             divunp1[j,i] = (u[j,i+1] - u[j,i])/dx + (v[j+1,i] - v[j,i])/dy
#     residual = np.linalg.norm(divunp1.ravel())
#     if residual > 1e-6:
#         print('Mass residual:',np.linalg.norm(divunp1.ravel()))
    
    # save new solutions
#     usol.append(u)
#     vsol.append(v)
    
    t += dt
tend = time.clock()
totaltime = tend - t0
print('total time', totaltime, 's')
print('time per timestep =',totaltime/nsteps, 's')
print('mom assembly time', totaltime - solvertime, 's')
print('solver time', solvertime, 's')
print('solver fraction =', np.ceil(solvertime/(tend - t0)*100),'%')
```

    total time 0.11582800000000049 s
    time per timestep = 0.01158280000000005 s
    mom assembly time 0.04313199999999817 s
    solver time 0.07269600000000231 s
    solver fraction = 63.0 %



```python
divu = (u[1:-1,2:] - u[1:-1,1:-1])/dx + (v[2:,1:-1] - v[1:-1,1:-1])/dy
plt.imshow(divu,origin='bottom')
plt.colorbar()
```




    <matplotlib.colorbar.Colorbar at 0xa239f12b0>




    
![svg](output_6_1.svg)
    



```python
fig = plt.figure(figsize=[6,6],dpi=600)
ucc = 0.5*(u[1:-1,2:] + u[1:-1,1:-1])
vcc = 0.5*(v[2:,1:-1] + v[1:-1,1:-1])
speed = np.sqrt(ucc*ucc + vcc*vcc)
plt.contourf(speed)
```




    <matplotlib.contour.QuadContourSet at 0xa23ac44e0>




    
![svg](output_7_1.svg)
    



```python
x = np.linspace(0,lx,nx)
y = np.linspace(0,ly,ny)
xx,yy = np.meshgrid(x,y)
nn = 1
fig = plt.figure(figsize=[6,6],dpi=600)
plt.quiver(xx[::nn,::nn],yy[::nn,::nn],ucc[::nn,::nn],vcc[::nn,::nn])
plt.xlim([xx[0,0],xx[0,-1]])
plt.ylim([yy[0,0],yy[-1,0]])
# ax.set_xlim([xx[0,0],xx[0,-1]])
plt.streamplot(xx,yy,ucc, vcc, color=np.sqrt(ucc*ucc + vcc*vcc),density=1.5,cmap=plt.cm.autumn,linewidth=1.5)
```




    <matplotlib.streamplot.StreamplotSet at 0xa205db898>




    
![svg](output_8_1.svg)
    



```python

```
