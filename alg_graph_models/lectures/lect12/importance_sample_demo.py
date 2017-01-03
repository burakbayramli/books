from gPy.Examples import asia
from gPy.Samplers import BNSampler

sampler = BNSampler(asia)
sampler.condition({'XRay':'abnormal'})
for i in xrange(100):
    print sampler.importance_sample()
