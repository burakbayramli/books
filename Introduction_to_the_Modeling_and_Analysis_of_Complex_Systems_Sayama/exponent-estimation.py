from pylab import *
import networkx as nx
from scipy import stats as st

n = 10000
ba = nx.barabasi_albert_graph(n, 5)
Pk = [float(x) / n for x in nx.degree_histogram(ba)]
domain = range(len(Pk))
ccdf = [sum(Pk[k:]) for k in domain]

logkdata = []
logFdata = []
prevF = ccdf[0]
for k in domain:
    F = ccdf[k]
    if F != prevF:
        logkdata.append(log(k))
        logFdata.append(log(F))
        prevF = F

a, b, r, p, err = st.linregress(logkdata, logFdata)
print 'Estimated CCDF: F(k) =', exp(b), '* k^', a
print 'r =', r
print 'p-value =', p

plot(logkdata, logFdata, 'o')
kmin, kmax = xlim()
plot([kmin, kmax],[a * kmin + b, a * kmax + b]) 
xlabel('log k')
ylabel('log F(k)')
show()
