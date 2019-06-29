import pylab, scipy.special, math

# read file cepheids.dat and extract data
(P, dP, V, dV) = pylab.loadtxt('cepheids.dat', unpack=True)
N = len(P)  # number of data points

# compute linear correlation coefficient
x = [math.log10(P[i]) for i in range(N)]
y = V
meanx = 0.0
meany = 0.0
M2x = 0.0
M2y = 0.0
C = 0.0
for i in range(N):
    deltax = x[i]-meanx
    deltay = y[i]-meany
    meanx = meanx+deltax/(i+1)
    meany = meany+deltay/(i+1)
    M2x = M2x+deltax*(x[i]-meanx)
    M2y = M2y+deltay*(y[i]-meany)
    C = C+deltax*(y[i]-meany)
r = C/(M2x*M2y)**0.5
p = 1-scipy.special.betainc(0.5, 0.5*(N-2), r**2)
print 'correlation coefficient, r = %f'%r
print 'probability under null hypothesis, p = %f%%'%(100*p)

# plot the data
pylab.errorbar(P, V, fmt='o', xerr=dP, yerr=dV)
pylab.ylim(reversed(pylab.ylim()))  # reverse y-axis
pylab.xscale('log')
pylab.xlabel('Period (days)')
pylab.ylabel('Magnitude')
pylab.title('Period-magnitude relation for Cepheid variables in M101')
pylab.grid(which='both')
pylab.show()
