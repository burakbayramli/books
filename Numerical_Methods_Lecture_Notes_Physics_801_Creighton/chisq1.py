import pylab, scipy.special, random, math

N = input('number of measurements -> ')
mu = input('true rate of events -> ')
mu0 = input('rate of events under null hypothesis -> ')
seed = input('random number seed -> ')
random.seed(seed)

# generate data
n = [0]*N  # number of events for each measurement
q = math.exp(-mu)
for i in range(N):
    p = random.random()
    while p > q:
        p *= random.random()
        n[i] += 1

# compute observed and expected distributions
K = max(n)+2  # number of bins; last one is the >max(n) bin
E = [0.0]*K  # expected frequency
O = [0]*K  # observed frequency
factorial = 1  # k!
for k in range(K-1):
    O[k] = n.count(k)
    E[k] = N*mu0**k*math.exp(-mu0)/factorial
    factorial *= k+1
# remaining number in the >max(n) bin
E[K-1] = N-sum(E)

# compute chi-squared statistic
chisq = sum((O[k]-E[k])**2/E[k] for k in range(K))

# compute significance
nu = K-1  # degrees of freedom
p = 1.0-scipy.special.gammainc(0.5*nu, 0.5*chisq)

# plot results
counts = pylab.array(range(K))
pylab.bar(counts-0.3, O, color='b', width=0.3, label='observed')
pylab.bar(counts, E, color='g', width=0.3, label='expected')
labels = [str(k) for k in range(K)]
labels[K-1] = '>'+labels[K-1]
pylab.xticks(counts, labels)
pylab.ylabel('frequency')
pylab.xlabel('counts')
pylab.xlim(xmin=-1, xmax=K)
pylab.legend()
pylab.title('chi-squared = %f, p-value = %f%%'%(chisq, 100.0*p))
pylab.show()
