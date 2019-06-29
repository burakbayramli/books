import math, pylab

# input the parameters
nsteps = input('enter number of steps -> ')
dtheta = input('enter step size (rad) -> ')

# these are 'list comprehensions'
theta = [i*dtheta for i in range(nsteps)]
sine = [math.sin(x) for x in theta]
cosine = [math.cos(x) for x in theta]
pylab.plot(theta, sine, 'o-b', label='sin')  # blue circle symbols with line
pylab.plot(theta, cosine, 'o-r', label='cos')  # red circle symbols with line
pylab.xlabel('theta (rad)')
pylab.ylabel('sin(theta) and cos(theta)')
pylab.legend()
pylab.title('the sine and cosine functions')
pylab.grid()
pylab.show()
