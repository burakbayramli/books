import math

# set values for the parameters
nsteps = 200
dtheta = 0.1

# loop incrementing i from 0 to nsteps - 1
for i in range(nsteps):
    theta = i*dtheta
    print theta, math.cos(theta)
