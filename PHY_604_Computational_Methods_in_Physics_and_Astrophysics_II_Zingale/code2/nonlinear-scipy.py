import numpy
from scipy import optimize
import pylab

# based on http://lister.dulci.duhs.duke.edu/~cliburn/summer-school/python/_build/html/model_fitting.html

def resid(avec, x, y):
    """ the residual function -- this is what will be minimized by the
        scipy.optimize.leastsq() routine.  avec is the parameters we
        are optimizing -- they are packed in here, so we unpack to
        begin.  (x, y) are the data points 

        scipy.optimize.leastsq() minimizes:

           x = arg min(sum(func(y)**2,axis=0))
                    y

        so this should just be the distance from a point to the curve,
        and it will square it and sum over the points
        """

    a0, a1 = avec

    # note: if we wanted to deal with error bars, we would weight each
    # residual accordingly
    return y - a0*numpy.exp(a1*x)


# make up some experimental data
a0 = 2.5
a1 = 2./3.
sigma = 2.0

x = numpy.linspace(0.0, 4.0, 25)
y = a0*numpy.exp(a1*x) + sigma*numpy.random.randn(len(x))

pylab.scatter(x,y)
pylab.errorbar(x, y, yerr=sigma, fmt=None, label="_nolegend_")

# initial guesses
a0 = 1
a1 = 1

# fit -- here the args is a tuple of objects that will be added to the
# argument lists for the function to be minimized (resid in our case)
afit, flag = optimize.leastsq(resid, [a0, a1], args=(x, y))

print flag
print afit

p = pylab.plot(x, afit[0]*numpy.exp(afit[1]*x), 
           label=r"$a_0 = $ %f; $a_1 = $ %f" % (afit[0], afit[1]))

pylab.legend(numpoints=1, frameon=False)

pylab.savefig("nonlinear-scipy.png")


