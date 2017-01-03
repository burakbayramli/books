import numpy
import scipy.stats
import matplotlib.pyplot as plt

# this is my own method, without having to 
# use e beta distribution. we simply sample from two 
# dirichlet distributions, and use selected columns for 
# difference calculation.
a = numpy.random.mtrand.dirichlet([295,308,39], size=1000)
b = numpy.random.mtrand.dirichlet([289,332,20], size=1000)

plt.xlim(-0.10,0.10)
plt.hist(b[:,0]-a[:,0], 100)
plt.show()

