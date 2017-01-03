from gPy.Examples import asia
from gPy.Samplers import BNSampler
import sys
output = open(sys.argv[1],'w')

sampler = BNSampler(asia)
for v in sampler.variables():
    print >>output, '%s:%s' % (v,','.join(asia.values(v)))
print >>output, ','.join(sampler.variables())
for i in xrange(300):
    print >>output, ','.join(sampler.forward_sample())
