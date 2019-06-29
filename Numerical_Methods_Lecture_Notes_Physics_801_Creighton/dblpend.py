import math, pylab

g = 9.8  # standard freefall (m/s^2)
l = input('pendulum length (meters) -> ')
theta10 = input('initial angle 1 (radians) -> ')
theta20 = input('initial angle 2 (radians) -> ')
dt = input('time step (seconds) -> ')
tmax = input('time to end of simulation (seconds) -> ')
nsteps = int(tmax/dt)
t = [0.0]*nsteps
p1 = [0.0]*nsteps
p2 = [0.0]*nsteps
q1 = [0.0]*nsteps
q2 = [0.0]*nsteps
x1 = [0.0]*nsteps
x2 = [0.0]*nsteps
y1 = [0.0]*nsteps
y2 = [0.0]*nsteps

# initialize
q1[0] = theta10
q2[0] = theta20
x1[0] = l*math.sin(q1[0])
y1[0] = -l*math.cos(q1[0])
x2[0] = l*(math.sin(q1[0])+math.sin(q2[0]))
y2[0] = -l*(math.cos(q1[0])+math.cos(q2[0]))

# use Euler-Cromer method to integrate the double pendulum
for i in range(nsteps-1):
    s = math.sin(q1[i]-q2[i])
    c = math.cos(q1[i]-q2[i])
    D = l**2*(1+s**2)
    A = p1[i]*p2[i]*s/D
    B = (p1[i]**2+2*p2[i]**2-2*p1[i]*p2[i]*c)*s*c*l**2/D**2
    p1[i+1] = p1[i]-(2*g*l*math.sin(q1[i])-A+B)*dt
    p2[i+1] = p2[i]-(g*l*math.sin(q2[i])+A-B)*dt
    q1[i+1] = q1[i]+(p1[i+1]-p2[i+1]*c)/D*dt
    q2[i+1] = q2[i]+(2*p2[i+1]-p1[i+1]*c)/D*dt
    t[i+1] = t[i]+dt

    # put q1 and q2 in range -pi to +pi
    q1[i+1] = (q1[i+1]+math.pi)%(2.0*math.pi)-math.pi
    q2[i+1] = (q2[i+1]+math.pi)%(2.0*math.pi)-math.pi

    # also compute (x,y) locations of points 1 and 2
    x1[i+1] = l*math.sin(q1[i+1])
    y1[i+1] = -l*math.cos(q1[i+1])
    x2[i+1] = l*(math.sin(q1[i+1])+math.sin(q2[i+1]))
    y2[i+1] = -l*(math.cos(q1[i+1])+math.cos(q2[i+1]))

# plot results

pylab.figure()
pylab.title('double pendulum')
pylab.plot(t, q2, label='theta2')
pylab.plot(t, q1, label='theta1')
pylab.xlabel('t (s)')
pylab.ylabel('angle (rad)')
pylab.legend(loc=9)
pylab.grid()
pylab.figure(figsize=(6, 6))
pylab.title('Lissajou curves for the double pendulum')
pylab.plot(q1, q2)
pylab.xlabel('theta1 (rad)')
pylab.ylabel('theta2 (rad)')
minmax = max(abs(min(q1+q2)), abs(max(q1+q2)))
pylab.axis([-minmax, minmax, -minmax, minmax], aspect='equal')
pylab.grid()
pylab.figure(figsize=(6, 6))
pylab.title('double pendulum trace')
pylab.plot(x2, y2)
pylab.xlabel('x (m)')
pylab.ylabel('y (m)')
pylab.axis([-2.1, 2.1, -2.1, 2.1], aspect='equal')
pylab.grid()
pylab.show()
