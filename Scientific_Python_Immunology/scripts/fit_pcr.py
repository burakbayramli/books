import numpy
import pylab
from scipy import stats

if __name__ == '__main__':
    xs = numpy.loadtxt('pcr.csv', skiprows=1,
                       delimiter=',', usecols=(0,1))
    x = xs[:,0]
    y = numpy.log2(xs[:,1])
    pylab.plot(x, y, 'x')

    # extract the linear part into new variables x1 and y1
    idx = (y > 8) & (y < 15)
    x1 = x[idx]
    y1 = y[idx]
    
    slope, intercept, r_value, p_value, std_err = stats.linregress(x1,y1)
    pylab.plot(x1, intercept + slope*x1, 'r-')
    pylab.xlabel('Cycle Number')
    pylab.ylabel('Log2(intensity)')
    pylab.show()
