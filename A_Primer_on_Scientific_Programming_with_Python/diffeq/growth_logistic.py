from scitools.std import *
x0 = 100              # initial amount of individuals
M = 500               # carrying capacity
rho = 4               # initial growth rate in percent
N = 200               # number of time intervals
index_set = range(N+1)
x = zeros(len(index_set))

# Compute solution
x[0] = x0
for n in index_set[1:]:
    x[n] = x[n-1] + (rho/100.0)*x[n-1]*(1 - x[n-1]/float(M))
print x
plot(index_set, x, 'r', xlabel='time units',
     ylabel='no of individuals', hardcopy='tmp.eps')
