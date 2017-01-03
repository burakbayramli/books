from gPy.Examples import asia
from gPy.Samplers import BNSampler
import sys
output = open(sys.argv[1],'w')

sampler = BNSampler(asia)
for v in sampler.variables():
    print >>output, '%s:%s' % (v,','.join(asia.values(v)))
print >>output, ','.join(sampler.variables())
dkt = {}
for i in xrange(300):
    inst = ','.join(sampler.forward_sample())
    try:
        dkt[inst] += 1
    except KeyError:
        dkt[inst] = 1

for inst, count in dkt.items():
    print >>output, '%s,%s' % (inst,count)
