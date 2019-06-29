import pylab, random, math

N = input('number of data points -> ')
mu = input('mean of true distribution -> ')
sigma = input('standard deviation of true distribution -> ')
seed = input('random number seed -> ')
random.seed(seed)

# generate data
x = [random.gauss(mu, sigma) for i in range(N)]

# compute K-S test statistic
x.sort()
D = 0.0  # biggest difference
for i in range(N):
    # the hypothesized cumulative distribution at this point
    F = 0.5*(1.0+math.erf(x[i]/2**0.5))
    F0 = i/float(N)
    F1 = (i+1)/float(N)

    # compute distance both before and after this point
    d = max(abs(F-i/float(N)), abs(F-(i+1)/float(N)))

    # keep this value if it is the largest so far
    if d > D:
        (D, X) = (d, x[i])
        if abs(F-F0) > abs(F-F1):  # determine the interval
            segment = [F, F0]
        else:
            segment = [F, F1]

# compute p-value for this statistic
K = D*(N**0.5+0.12+0.11/N**0.5)
a = 2.0
b = -2.0*K**2
p = 0.0
eps = 1e-6
for i in range(1, 20):
    term = a*math.exp(b*i**2)
    if abs(term) < eps*p:
        break
    p += term
    a *= -1.0

# plot the empirical and hypothetical distributions
xfine = pylab.arange(-3.0, 3.0, 0.01)
F = [0.5*(1.0+math.erf(xx/2**0.5)) for xx in xfine]
pylab.plot(xfine, F, label='hypothetical')
pylab.plot(x+[float('inf')], [i/float(N) for i in range(N+1)], 'o-',
           drawstyle='steps', label='empirical')
pylab.plot([X, X], segment, 'D--', label='D = %g'%D)
pylab.ylabel('cumulative probability')
pylab.xlabel('x')
pylab.title('p-value = %3g%%'%(100*p))
pylab.legend(loc=0)
pylab.grid()
pylab.show()
