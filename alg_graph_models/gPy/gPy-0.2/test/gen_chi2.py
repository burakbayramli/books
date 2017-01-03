# code for some manual Chi2 testing I did.
from gPy.Models import BN
from gPy.Parameters import CPT, Factor
from gPy.Variables import Domain
from gPy.LearningUtils import CausalWorld

def disp(fn, samples):
    f = open(fn, 'w')
    fact = samples.makeFactor(samples.variables())
    for var in fact.variables():
        print >>f, var,
    print >>f, 'count'
    for inst in fact.insts():
        for i in inst:
            print >>f, i,
        print >>f, fact[inst]
    f.close()

bn0 = BN(domain=Domain(), new_domain_variables={'a': [0,1], 'b':[0,1]})
bn0.add_cpts([CPT(Factor(variables=['a'], data=[0.5, 0.5]),child='a')
             ,CPT(Factor(variables=['a','b'], data=[0.3, 0.7, 0.4, 0.6]),child='b')
             ])
w = CausalWorld(bn0)
samples = w.observe(10000)
disp('two_depend', samples)

bn1 = BN(domain=Domain(), new_domain_variables={'a': [0,1], 'b':[0,1]})
bn1.add_cpts([CPT(Factor(variables=['a'], data=[0.5, 0.5]),child='a')
             ,CPT(Factor(variables=['b'], data=[0.3, 0.7]),child='b')
             ])
w = CausalWorld(bn1)
samples = w.observe(10000)
disp('two_independ', samples)

bn2 = BN(domain=Domain(), new_domain_variables={'a': [0,1,2], 'b':[0,1,2]})
bn2.add_cpts([CPT(Factor(variables=['a'], data=[1.0/3.0, 1.0/3.0, 1.0/3.0]),child='a')
             ,CPT(Factor(variables=['a','b'], data=[0.2, 0.6, 0.2, 0.5, 0.4, 0.1, 0.0, 1.0, 0.0]),child='b')
             ])
w = CausalWorld(bn2)
samples = w.observe(10000)
disp('three_depend', samples)

bn3 = BN(domain=Domain(), new_domain_variables={'a': [0,1,2], 'b':[0,1,2]})
bn3.add_cpts([CPT(Factor(variables=['a'], data=[1.0/3.0, 1.0/3.0, 1.0/3.0]),child='a')
             ,CPT(Factor(variables=['b'], data=[0.3, 0.7, 0.0]),child='b')
             ])
w = CausalWorld(bn3)
samples = w.observe(10000)
disp('three_independ', samples)

