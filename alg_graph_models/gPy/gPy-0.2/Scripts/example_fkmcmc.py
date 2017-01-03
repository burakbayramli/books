from gPy.LearningUtils import *
from gPy.Examples import asia
from gPy.K2 import *
from gPy.FKMCMC import fk_exp_graph
from gPy.Models import CBN

# generate 500 samples of the Asia network
data = CausalWorld(asia)
data.observe(500)

# for comparison
true_model = asia
true_adg = asia.adg()

# produce a sample of ADGs using K2 to search
search = K2(max_parents_family=3)
# generate 5*2 = 10 ADGs from every 7th step of the MCMC using 2
# `independent' order chains
found_adgs = fk_exp_graph(search, data,
                            num_samples = 5,
                            sample_every = 7,
                            num_orderings = 2)

# fit models to the data using the ADGs
found_models = [CBN.from_adg_data(found_adg, data) for found_adg in found_adgs]

print 'Structural Hamming distance:',[shd(found_adg,true_adg) for found_adg in found_adgs]
print 'BDeu scores of fitted models of'
print 'found ADG:', [bdeu(found_adg,data) for found_adg in found_adgs]
print ' true ADG:', bdeu(true_adg,data)
print 'KL-divergence of fitted found model from true model:',
print [dkl(true_model,found_model) for found_model in found_models]

