import numpy
import pylab
from scipy import stats

if __name__ == '__main__':
    xs = numpy.loadtxt('dilution.csv', skiprows=1,
                       delimiter=',', usecols=(0,1))
    x = numpy.log2(xs[:,0])
    y = xs[:,1]  
    pylab.plot(x, y, 'x')
    slope, intercept, r_value, p_value, std_err = stats.linregress(x,y)
    pylab.plot(x, intercept + slope*x, 'r-')
    pylab.xlabel('Number of dilutions')
    pylab.show()
