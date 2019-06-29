import pylab, scipy.special, random, math

N = input('number of points -> ')
M = 1+input('polynomial order -> ')
a = pylab.zeros(M)
for j in range(M):
    a[j] = input('polynomial coefficient a%d -> '%j)
seed = input('random seed -> ')
random.seed(seed)

# generate random data with random uncertainties in y
x = pylab.arange(0.5/N, 1.0, 1.0/N)
dy = pylab.array([0.1*random.lognormvariate(0.0, 1.0) for i in range(N)])
y = [sum(a[j]*x[i]**j for j in range(M)) for i in range(N)]
y = pylab.array([random.gauss(y[i], dy[i]) for i in range(N)])

# construct vector b and design matrix X
b = pylab.zeros(N)
X = pylab.zeros((N, M))
for i in range(N):
    b[i] = y[i]/dy[i]
    for j in range(M):
        X[i,j] = x[i]**j/dy[i]

# compute fit parameters ahat and covariance matrix Sigma
(U, w, VT) = pylab.svd(X)
wmax = max(w)
Winv = pylab.zeros((M, N))
Sigma = pylab.zeros((M, M))
eps = 1e-6
for j in range(M):
    if w[j] > eps*wmax:
        Winv[j,j] = 1.0/w[j]
    else:
        Winv[j,j] = 0.0
    Sigma[j,j] = Winv[j,j]**2
ahat = pylab.dot(VT.T, pylab.dot(Winv, pylab.dot(U.T, b)))
Sigma = pylab.dot(VT.T, pylab.dot(Sigma, VT))

# compute chi-square and p-value of the fit
chisq = pylab.norm(pylab.dot(X, ahat)-b)**2
p = 1.0-scipy.special.gammainc((N-M)/2.0, chisq/2.0)

# plot results
xfine = pylab.arange(0.0, 1.0, 0.01)
yfit = [sum(ahat[j]*xx**j for j in range(M)) for xx in xfine]
ytrue = [sum(a[j]*xx**j for j in range(M)) for xx in xfine]
pylab.errorbar(x, y, fmt='o', yerr=dy)
pylab.plot(xfine, yfit, label='fit: a='+str(ahat))
pylab.plot(xfine, ytrue, '--', label='true: a='+str(a))
pylab.xlabel('x')
pylab.ylabel('y')
pylab.title('chi-squared = %f, p-value = %f%%'%(chisq, 100.0*p))
pylab.legend(loc=3)

# plot 90% (1.64-sigma) error ellipses
cos = [math.cos(2.0*math.pi*i/100.0) for i in range(101)]
sin = [math.sin(2.0*math.pi*i/100.0) for i in range(101)]
pylab.figure(figsize=(10, 10))
for j in range(M-1):
    for k in range(j+1, M):
        pylab.subplot(M-1, M-1, (M-1)*j+k)
        (s0, s1, s01) = (Sigma[j,j]**0.5, Sigma[k,k]**0.5, Sigma[j,k])
        r = s01/(s0*s1)
        s = (1.0-r**2)**0.5
        ex = [ahat[j]+3.53**0.5*s0*cos[i] for i in range(len(cos))]
        ey = [ahat[k]+3.53**0.5*s1*(r*cos[i]+s*sin[i]) for i in
              range(len(cos))]
        pylab.plot(ex, ey)
        pylab.plot([ahat[j]], [ahat[k]], 'go')
        pylab.plot([a[j]], [a[k]], 'r*')
        pylab.xlabel('a%d'%j)
        pylab.ylabel('a%d'%k)
        pylab.grid()
pylab.show()
