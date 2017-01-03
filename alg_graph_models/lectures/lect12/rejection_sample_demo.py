from gPy.Examples import asia
from gPy.Samplers import BNSampler

sampler = BNSampler(asia)
sampler.condition({'XRay':'abnormal'})
print "Samples with 'None's are rejected" 
for i in xrange(100):
    print sampler.rejection_sample()
