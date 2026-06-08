# CODE FOR LID-DRIVEN CAVITY
import numpy as np
import matplotlib.pyplot as plt
import time

gsize = 50
n = gsize
m = gsize

f = np.zeros((9, n+1, m+1))
feq = np.zeros((9, n+1, m+1))
rho = np.ones((n+1, m+1))
w = [ 4/9, 1/9, 1/9, 1/9, 1/9, 1/36, 1/36, 1/36, 1/36 ]
cx = [ 0, 1, 0, -1, 0, 1, -1, -1, 1 ]
cy = [ 0, 0, 1, 0, -1, 1, 1, -1, -1 ]
u = np.zeros((n+1, m+1))
v = np.zeros((n+1, m+1))


uo = 0.4
sumvelo = 0.0
rhoo = 1.00
rho *= rhoo
dx = 1.0
dy = dx
dt = 1.0
Re = 1000 # Reynolds Number
nu = uo * m / Re # dynamic viscosity
tau = (3*nu + 0.5)
omega = 1/tau
Ma = (dx/(m*np.sqrt(3)))*(omega-0.5)*Re
print(f"Re = {Re}, Ma = {Ma}, Tau = {tau}, nu = {nu}, Grid size = {n+1} x {m+1}")
mstep = 40000 #40000
errmax = 1e-6 #1e-6

u[:,m] = uo # Moving wall BC


def collesion(u, v, f, feq, rho, omega, w, cx, cy, n, m):
    
    t1 = u[:,:]**2 + v[:,:]**2

    for k in range(9):
        
        t2 = u[:,:]*cx[k] + v[:,:]*cy[k]
        feq[k,:,:] = rho[:,:]*w[k]*(1.0 + 3.0*t2 + 4.50*t2*t2 - 1.50*t1)
        f[k,:,:] = omega*feq[k,:,:] + (1. - omega)*f[k,:,:]
    
    return feq, f
        

def streaming(f, n, m):

    # RIGHT TO LEFT
    for i in reversed(range(1, n+1)):
        f[1,i,:] = f[1,i-1,:]
    
    # LEFT TO RIGHT
    for i in range(n):
        f[3,i,:] = f[3, i+1, :]
    
    # TOP TO BOTTOM
    for j in reversed(range(1,m+1)):
        
        f[2,:,j] = f[2,:,j-1]

        for i in reversed(range(1, n+1)):
            f[5,i,j] = f[5,i-1,j-1]
        
        for i in range(n):
            f[6,i,j] = f[6, i+1, j-1]
    # END loop
    
    # BOTTOM TO TOP
    for j in range(m):
        
        f[4,:,j] = f[4,:,j+1]
        
        for i in range(n):
            f[7,i,j] = f[7, i+1, j+1]
        
        #for i in range(n, 0, -1): #n,0,-1 prevoiusly
        for i in reversed(range(1,n+1)):
            f[8,i,j] = f[8, i-1, j+1]
    # END loop
    
    return f
            
def sfbound(f, n, m, uo):
    
    # Bounce back on WEST boundary
    f[1,0,:] = f[3,0,:]
    f[5,0,:] = f[7,0,:]
    f[8,0,:] = f[6,0,:]
    # Bounce back on EAST boundary
    f[3,n,:] = f[1,n,:]
    f[7,n,:] = f[5,n,:]
    f[6,n,:] = f[8,n,:]
    
    # Bounce back on SOUTH boundary
    f[2,:,0] = f[4,:,0]
    f[5,:,0] = f[7,:,0]
    f[6,:,0] = f[8,:,0]
        
    # Moving lid, NORTH boundary
    rhon = f[0,:,m] + f[1,:,m] + f[3,:,m] + 2*(
            f[2,:,m] + f[6,:,m] + f[5,:,m])
        
    f[4,:,m] = f[2,:,m]
    f[8,:,m] = f[6,:,m] + rhon*uo/6 
    f[7,:,m] = f[5,:,m] - rhon*uo/6 
        
    return f

def rhouv(f, rho, u, v, cx, cy, n, m):
    

    rho[:,:] = np.sum(f, axis=0) # sum over all k (axis 0)
    
    # North boundary
    rho[:,m] = f[0,:,m] + f[1,:,m] + f[3,:,m] + 2*(
            f[2,:,m] + f[6,:,m] + f[5,:,m])
    
    usum = np.zeros((n+1,m))
    vsum = np.zeros((n+1,m+1))
    
    for k in range(9):
        
        usum += f[k, :, :m]*cx[k]
        vsum += f[k, :, :]*cy[k]
    
    u[:, :m] = usum/rho[:,:m]
    v[:, :] = vsum/rho[:,:]
       
    return rho, u, v
           
""" MAIN LOOP HERE """
du_plus_dv = np.zeros((n+1, m+1))

start_time = time.time()

for kk in range(1, mstep+1):

    du_plus_dv = -(u + v) # begin error computation
    
    # Execute the algorithm
    feq, f = collesion(u, v, f, feq, rho, omega, w, cx, cy, n, m)
    f = streaming(f, n, m)
    f = sfbound(f, n, m, uo) 
    rho, u, v = rhouv(f, rho, u, v, cx, cy, n, m)
    
    du_plus_dv += (u + v) # continue error computation
    err = np.amax(abs(du_plus_dv))
    
    if kk%500 == 0:
        print(kk, err)
    
    if err <= errmax:
        break

end_time = time.time()
print(f"Iterations = {kk}, Time = {np.round(end_time-start_time,2)} s, error = {err}")


u = np.rot90(u)
v = np.rot90(v)

xmin = 0
xmax = 1
ymin = 0
ymax = 1

xs = np.linspace(xmin, xmax, m+1)
ys = np.linspace(ymin, ymax, m+1)
x, y = np.meshgrid(xs, ys)

# Plot final u
plt.figure(dpi=800)
plt.imshow(u, cmap='turbo', extent = [xmin, xmax, ymin, ymax])
plt.colorbar()
plt.xlabel(r"$x$")
plt.ylabel(r"$y$")
plt.title(r"$x$-velcocity: $u$")
plt.show()

# Plot final v
plt.figure(dpi=800)
plt.imshow(v, cmap='turbo', extent = [xmin, xmax, ymin, ymax])
plt.colorbar()
plt.xlabel(r"$x$")
plt.ylabel(r"$y$")
plt.title(r"$y$-velcocity: $v$")
plt.show()


# Plot streamlines
plt.figure(figsize = (5,5), dpi = 800)
plt.streamplot(x, y, np.flipud(u), np.flipud(v), color='black')  #quiver
plt.xlabel(r"$x$")
plt.ylabel(r"$y$")
plt.title(r"Vector Flow Streamlines")
plt.show()  

N = gsize + 1
title_u = "Re=" + str(Re) + "_N=" + str(N) + "_u.txt"
title_v = "Re=" + str(Re) + "_N=" + str(N) + "_v.txt"

np.savetxt(title_u, u)
np.savetxt(title_v, v)  

half = int(np.floor(N/2))
vertical = u[:,half]
horizontal = v[half,:]

if N%2 == 0:
    
    vertical = (u[:,half] + u[:,half-1])/2
    horizontal = (v[half,:] + v[half-1,:])/2
    
# vertical centerline at num half
#"""
plt.figure(dpi = 800)
plt.plot(ys, vertical, 'r')
plt.plot(ys, vertical, 'k.')
plt.title(r"$u$ along the the vertical centerline $x=0.5$")
plt.xlabel(r"$y$")
plt.ylabel(r"$x$-velocity: $u$")
plt.show()

print(f"u min is {min(vertical)}")

plt.figure(dpi = 800)
plt.plot(xs, horizontal, 'r')
plt.plot(xs, horizontal, 'k.')
plt.title(r"$v$ along the the horizontal centerline $y=0.5$")
plt.xlabel(r"$x$")
plt.ylabel(r"$y$-velocity: $v$")
plt.show()
#"""
print(f"v min is {min(horizontal)}")
print(f"v max is {max(horizontal)}")


print(f"\nu min is {min(vertical*2.5)}")
print(f"v min is {min(horizontal*2.5)}")
print(f"v max is {max(horizontal*2.5)}")




    