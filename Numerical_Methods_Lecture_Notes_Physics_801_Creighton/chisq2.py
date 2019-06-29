import pylab, scipy.special, random, math

N1 = input('number of measurements from source 1 -> ')
N2 = input('number of measurements from source 2 -> ')
mu1 = input('rate of events from source 1 -> ')
mu2 = input('rate of events from source 2 -> ')
seed = input('random number seed -> ')
random.seed(seed)

# generate data
n1 = [0]*N1  # number of events for each measurement from source 1
n2 = [0]*N2  # number of events for each measurement from source 2
q1 = math.exp(-mu1)
q2 = math.exp(-mu2)
for i in range(N1):
    p = random.random()
    while p > q1:
        p *= random.random()
        n1[i] += 1
for i in range(N2):
    p = random.random()
    while p > q2:
        p *= random.random()
        n2[i] += 1

# compute observed and expected distributions
K = max(n1+n2)+1
R = [n1.count(k) for k in range(K)]  # frequencies from source 1
S = [n2.count(k) for k in range(K)]  # frequencies from source 1
counts = range(K)

# delete bins where R and S are both zero; note: need to go backwards
for k in reversed(range(K)):
    if R[k] == 0 and S[k] == 0:
        del R[k]
        del S[k]
        del counts[k]

# remaining number of bins
K = len(counts)

# compute chi-squared statistic
Q1 = (float(N2)/float(N1))**0.5
Q2 = 1.0/Q1
chisq = sum((Q1*R[k]-Q2*S[k])**2/(R[k]+S[k]) for k in range(K))

# compute significance
nu = K-1  # degrees of freedom
p = 1.0-scipy.special.gammainc(0.5*nu, 0.5*chisq)

# plot results
counts = pylab.array(counts)
pylab.bar(counts-0.3, R, color='b', width=0.3, label='source 1')
pylab.bar(counts, S, color='g', width=0.3, label='source 2')
pylab.ylabel('normalized frequency')
pylab.xlabel('counts')
pylab.xticks(range(min(counts), max(counts)+1))
pylab.xlim(xmin=min(counts)-1, xmax=max(counts)+1)
pylab.legend()
pylab.title('chi-squared = %f, p-value = %f%%'%(chisq, 100.0*p))
pylab.show()
