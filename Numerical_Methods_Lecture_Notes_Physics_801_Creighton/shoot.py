import math, pylab

g = 9.8  # standard freefall (m/s^2)
v0 = input('initial velocity (m/s) -> ')
xtarget = input('target range (m) -> ')
eps = 0.01  # how close we must get (m)
dt = 0.001  # time step (s)
theta1 = 0.0  # bracketing angle (degrees) that falls too short
theta2 = 45.0  # bracketing angle (degrees) that falls too far
dx = 2*eps  # some initial value > eps
while abs(dx) > eps:
    # guess at the value of theta
    theta = (theta1+theta2)/2.0
    x = [0.0]
    y = [0.0]
    vx = [v0*math.cos(theta*math.pi/180.0)]
    vy = [v0*math.sin(theta*math.pi/180.0)]

    # use Euler's method to integrate projectile equations of motion
    i = 0
    while y[i] >= 0.0:

        # apply finite difference approx to equations of motion
        x += [x[i]+vx[i]*dt]
        y += [y[i]+vy[i]*dt]
        vx += [vx[i]]
        vy += [vy[i]-g*dt]
        i = i+1

    # we hit the ground somewhere between step i-1 and i interpolate to find
    # this location
    xground = x[i-1]+y[i-1]*(x[i]-x[i-1])/(y[i]-y[i-1])

    # update the bounds bracketing the root
    dx = xground-xtarget
    if dx < 0.0:  # too short: update smaller angle
        theta1 = theta
    else:  # too far: update larger angle
        theta2 = theta

# plot the correct trajectory
pylab.plot(x, y)
pylab.plot([xtarget], [0.0], 'o')
pylab.annotate('target', xy=(xtarget, 0), xycoords='data', xytext=(5, 5),
               textcoords='offset points')
pylab.title('trajectory of a projectile with theta = %.2f degrees'%theta)
pylab.xlabel('x (m)')
pylab.ylabel('y (m)')
pylab.ylim(ymin=0.0)
pylab.show()
