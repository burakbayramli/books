import pylab, scipy.special, random

N = input('number of points -> ')
a = input('true intercept -> ')
b = input('true slope -> ')
seed = input('random seed -> ')
random.seed(seed)

# generate random data with random uncertainties in y
x = pylab.arange(0.5/N, 1.0, 1.0/N)
dy = [0.1*random.lognormvariate(0.0, 1.0) for i in range(N)]
y = [random.gauss(a+b*x[i], dy[i]) for i in range(N)]

# compute linear regression coefficients ahat and bhat and uncertainties
w = [1.0/dy[i]**2 for i in range(N)]
S = sum(w[i] for i in range(N))
Sx = sum(w[i]*x[i] for i in range(N))
Sy = sum(w[i]*y[i] for i in range(N))
Sxx = sum(w[i]*x[i]**2 for i in range(N))
Sxy = sum(w[i]*x[i]*y[i] for i in range(N))
Delta = S*Sxx-Sx**2
ahat = (Sxx*Sy-Sx*Sxy)/Delta
bhat = (S*Sxy-Sx*Sy)/Delta
da = (Sxx/Delta)**0.5
db = (S/Delta)**0.5

# compute chi-square and p-value of the fit
chisq = sum(w[i]*(y[i]-ahat-bhat*x[i])**2 for i in range(N))
p = 1.0-scipy.special.gammainc((N-2.0)/2.0, chisq/2.0)

# plot the data and the fit
pylab.errorbar(x, y, fmt='o', yerr=dy, label='data: a=%g, b=%g'%(a, b))
pylab.plot([0.0, 1.0], [ahat, ahat+bhat],
           label='fit: a=%.2f$\pm$%.2f, b=%.2f$\pm$%.2f'%(ahat, da, bhat, db))
pylab.xlabel('x')
pylab.ylabel('y')
pylab.title('chi-squared = %f, p-value = %f%%'%(chisq, 100.0*p))
pylab.legend(loc=2)
pylab.show()
