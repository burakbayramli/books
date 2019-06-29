import math, pylab

g = 9.8  # standard freefall (m/s^2)
v0 = 10.0  # initial velocity (m/s)
angles = [30.0, 35.0, 40.0, 45.0, 50.0, 55.0] # launch angles (degrees)
dt = 0.01  # time step (s)
for theta in angles:
    x = [0.0]
    y = [0.0]
    vx = [v0*math.cos(theta*math.pi/180.0)]
    vy = [v0*math.sin(theta*math.pi/180.0)]

    # use Euler's method to integrate projectile equations of motion
    i = 0
    while y[i] >= 0.0:

        # extend the lists by appending another point
        x += [0.0]
        y += [0.0]
        vx += [0.0]
        vy += [0.0]

        # apply finite difference approx to equations of motion
        x[i+1] = x[i]+vx[i]*dt
        y[i+1] = y[i]+vy[i]*dt
        vx[i+1] = vx[i]
        vy[i+1] = vy[i]-g*dt
        i = i+1

    # plot the trajectory
    pylab.plot(x, y, label=str(theta)+' degrees')

pylab.title('trajectory of a projectile')
pylab.xlabel('x (m)')
pylab.ylabel('y (m)')
pylab.ylim(ymin=0.0)
pylab.legend()
pylab.show()
