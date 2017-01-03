from scipy import integrate
import pylab

def f(y, t, k, alpha): 
  return (k*y[0]*y[1],
          -alpha*k*y[0]*y[1])

if __name__ == '__main__':

  y0 = [0.1, 10] 
  t = range(0, 480, 5) 
  k = 0.005
  alpha = 1

  r = integrate.odeint(f, y0, t, args=(k, alpha))

  pylab.plot(t, r, '-o')
  pylab.legend(['Bacteria', 'Nutrients'], loc='lower right')
  pylab.xlabel('Time')
  pylab.ylabel('Concentration')
  pylab.title('Simple ODE model for bacterial growth')
  pylab.show()
