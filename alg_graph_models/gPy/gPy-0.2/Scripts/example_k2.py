from gPy.LearningUtils import *
from gPy.Examples import asia
from gPy.K2 import *
from gPy.Models import CBN

# generate 500 samples of the Asia network
data = CausalWorld(asia)
data.observe(5000)

# for comparison
true_model = asia
true_adg = asia.adg()

# search for the ADG
found_adg = K2(max_parents_family=3).search(data, true_adg.topological_order())
# fit a model to the data using the ADG
found_model = CBN.from_adg_data(found_adg, data)

print 'True ADG\n', true_adg
print 'Found ADG\n', found_adg
print 'Structural Hamming distance:',shd(found_adg,true_adg)
print 'BDeu scores of fitted models of'
print 'found ADG:', bdeu(found_adg,data)
print ' true ADG:', bdeu(true_adg,data)
print 'KL-divergence of fitted found model from true model:',dkl(true_model,found_model)

