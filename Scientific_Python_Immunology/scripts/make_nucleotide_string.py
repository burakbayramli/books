mapper = {0: 'A', 1: 'C', 2: 'T', 3: 'G'}
import numpy
x = [mapper[n] for n in numpy.random.randint(0,4,1000)]
print ''.join(x)
