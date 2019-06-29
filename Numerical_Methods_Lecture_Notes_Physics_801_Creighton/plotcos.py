import math, pylab

nsteps = 200
dtheta = 0.1

# create lists of length nsteps for the values of theta and cosine
# initially the values are set to zero
theta = [0.0]*nsteps
cosine = [0.0]*nsteps

# loop to fill in the values of theta and cosine
for i in range(nsteps):
    theta[i] = i*dtheta
    cosine[i] = math.cos(theta[i])

# show a plot
pylab.plot(theta, cosine)
pylab.show()
