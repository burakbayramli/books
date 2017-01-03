from gPy.Data import Data2
from gPy.IO import read_csv
import sys, gzip

data = Data2(read_csv(open('/home/jc/godot/research/icml08/data/asia_100.data')),rmin=3)
#for k, v in data._data.items():
#    print k, v
#print data.marginal(['VisitAsia'])
#print data.marginal(['VisitAsia','TbOrCa','XRay','Dyspnea'])

#data = Data2(read_csv(gzip.open('/home/jc/godot/research/icml08/data/insurance_100.data.gz')),rmin=5)
#for k, v in data._data.items():
#    print k, v
#sys.exit()
data._test(['VisitAsia','TbOrCa','XRay','Dyspnea'])
#data._test(['Accident','ILiCost'])
sys.exit()
print data.marginal(['Accident','ILiCost'])
vs = sorted(data._variables)
for v in vs:
    for w in vs:
        for z in vs:
            print v,w,z, data.marginal(frozenset([v,w,z]))

