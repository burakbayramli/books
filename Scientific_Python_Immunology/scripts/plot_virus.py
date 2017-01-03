import numpy
from scipy import integrate
import pylab

# x = y[0] = uninfected cells
# y = y[1] = infected cells
# v = y[2] = virus

def f(y, t, gamma, d, a, beta, k, u): 
  return (gamma - d*y[0]- beta*y[0]*y[2],
          beta*y[0]*y[2] - a*y[1],
          k*y[1] - u*y[2])

if __name__ == '__main__':

  y0 = [1e5, 0, 1] 
  t = numpy.arange(0, 50, 0.1) 
  gamma = 1e5
  d = 0.1
  a = 0.5
  beta = 2e-7
  k = 100
  u = 5

  r = integrate.odeint(f, y0, t, args=(gamma, d, a, beta, k, u))

  pylab.plot(t, r, '-')
  pylab.legend(['Uninfected', 'Infected', 'Virus'], loc='upper right')
  pylab.xlabel('Time')
  pylab.ylabel('Concentration')
  pylab.title('Basic viral model')
  pylab.show()
