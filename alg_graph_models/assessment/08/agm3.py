"""
A factor should be created for each edge in the underlying graph.
This factor should give higher values when the two vertices have different
colours than when they are the same colour. Here I have made the
probability 5 times higher in the former case. Different values alter the
rate of convergence.

The other slightly tricky thing is to make eg (0,0) and (0,9) neighbours.

This is a case where Gibbs sampling *fails*. The distribution has two
high modes. Getting from one to the other would take an exceptionally long
time
"""

import sys, cPickle

binvals = (0,1)

from gPy.Variables import declare_variable
from gPy.Parameters import Factor
from gibbs import gibbs_sample
from gPy.Models import FR

for i in range(10):
    for j in range(10):
        declare_variable((i,j),binvals)

x=1
y=5
data = [
    x,  # 0,0
    y,  # 0,1
    y,  # 1,0
    x   # 1,1
    ]

factors = []
for i in range(10):
    for j in range(10):
        factors.append(Factor(((i,j),((i+1)%10,j)),data))
        factors.append(Factor(((i,j),(i,(j+1)%10)),data))

fr = FR(factors)
sample =  gibbs_sample(fr,100,0)
cPickle.dump(sample,open(sys.argv[1],'w'))


