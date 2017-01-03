from gPy.Data import CompactFactor, IncrementalCompactFactor
from gPy.Parameters import Factor
from gPy.Variables import Domain
from utils_test import same_factor, ManyModelsTest, ManySmallModelsTest, rand_factor_data
from gPy.Utils import powerset
from random import randint, random
from gPy.Samplers import ForwardSampler
from gPy.LearningUtils import CausalWorld
import unittest
import psyco

psyco.profile()

num_runs = 10
num_runs = 1

class TestCompactFactor(unittest.TestCase):
    def runTest(self):
        num_vars = 5
        num_vals = 10
        for i in xrange(num_runs):
            vs = dict([('V'+str(i),range(num_vals)) for i in xrange(num_vars)])
            # should get some random data with 0.
            f = Factor(variables = vs.keys()
                    ,data = [abs(randint(0,5)) for i in xrange(num_vals**num_vars)]
                    ,domain = Domain()
                    ,new_domain_variables=vs
                    ,check = True)
            records = []
            for inst in f.insts():
                records.append(inst + (f[inst],))
            rawdata = (vs.keys(), vs, vs.keys(), records)

            cf = CompactFactor(rawdata)
            for variables in powerset(vs.keys()):
                g = f.copy(copy_domain=True)
                g.marginalise_away(g.variables() - frozenset(variables))
                self.assert_(same_factor(g,cf.makeFactor(variables),verbose=True))

class TestIncrementalCompactFactor(unittest.TestCase):
    def runTest(self):
        num_vars = 5
        num_vals = 10
        for i in xrange(num_runs):
            vs = dict([('V'+str(i),range(num_vals)) for i in xrange(num_vars)])
            # should get some random data with 0.
            f = Factor(variables = vs.keys()
                    ,data = [abs(randint(0,5)) for i in xrange(num_vals**num_vars)]
                    ,domain = Domain()
                    ,new_domain_variables=vs
                    ,check = True)
            records = []
            for inst in f.insts():
                records.append(inst + (f[inst],))
            rawdata = (vs.keys(), vs, vs.keys(), records)

            cf = IncrementalCompactFactor(rawdata)
            for variables in powerset(vs.keys()):
                g = f.copy(copy_domain=True)
                g.marginalise_away(g.variables() - frozenset(variables))
                self.assert_(same_factor(g,cf.makeFactor(variables),verbose=True))


# test a batch update.
# TODO: multiple incremental updates
class TestIncrementalUpdate(unittest.TestCase):
    def runTest(self):
        num_vars = 5
        num_vals = 10
        for x in xrange(num_runs):
            vs = dict([('V'+str(i),range(num_vals)) for i in xrange(num_vars)])
            # should get some random data with 0.
            f = Factor(variables = vs.keys()
                    ,data = [abs(randint(0,20)) for i in xrange(num_vals**num_vars)]
                    #,data = [5, 0, 1, 2]
                    #,data = [13,0,0,20]
                    #,data = [14,15,20,16]
                    ,domain = Domain()
                    ,new_domain_variables=vs
                    ,check = True)
            records = []
            for inst in f.insts():
                records.append(inst + (f[inst],))
            rawdata = (vs.keys(), vs, vs.keys(), records)
            #print records

            cf = IncrementalCompactFactor(rawdata, rmin=0)
            #print 'old tree:'
            #print cf

#            g = Factor(variables = vs.keys()
#                    #,data = [13,2,0,0]
#                    ,data = [14,15,0,0]
#                    ,domain = Domain()
#                    ,new_domain_variables=vs
#                    ,check = True)
            g = f.copy(copy_domain=True)
            def swap_some(x):
                r = random()
                if x == 0:
                    if r <= 0.5:
                        return randint(1,5)
                    return 0
                elif r <= 0.5:
                    return 0
                return x
            def invert_some(x):
                return 5-x
            g.map(swap_some)
            #print g
            f += g
            records = []
            for inst in g.insts():
                records.append(inst + (g[inst],))
            rawdata = (vs.keys(), vs, vs.keys(), records)
            cf.update(rawdata)
            #print 'new tree:'
            #print cf

            for variables in powerset(vs.keys()):
                g = f.copy(copy_domain=True)
                g.marginalise_away(g.variables() - frozenset(variables))
                self.assert_(same_factor(g,cf.makeFactor(variables),verbose=True))

class TestADBDeu(ManyModelsTest):
    def tryModel(self, model):
        world = CausalWorld(model.copy())
        world.observe(1000)
        # and again to check updates...
        world.observe(1000)
        adg = model.adg()
        data = world.intervention_data(frozenset())
        for child in adg.topological_order():
            family_bdeu = data.family_score(child, adg.parents(child))
            self.assertAlmostEquals(family_bdeu, model[child].get_counts(data).bdeu_score())

class TestIncrementalADTree(ManyModelsTest):
    def tryModel(self, model):
        # generate some samples
        cf = []
        sampler = ForwardSampler(model)
        samples = sampler.samples(100)
        cf.append(CompactFactor(samples,domain=Domain()))
        icf = IncrementalCompactFactor(samples, domain=Domain())

        for i in xrange(10):
            samples = sampler.samples(100)
            cf.append(CompactFactor(samples,domain=Domain()))
            icf.update(samples)

            # see if the sum of the CPT Factors in cf match that of icf
            for child in model.variables():
                family = model.adg().parents(child) | set([child])
                a = icf.makeFactor(family)
                b = cf[0].makeFactor(family)
                for f in cf[1:]:
                    b += f.makeFactor(family)
                self.assert_(same_factor(a,b))


suite = unittest.makeSuite(TestCompactFactor)
suite.addTest(TestIncrementalCompactFactor())
suite.addTest(TestIncrementalUpdate())
suite.addTest(TestADBDeu())
suite.addTest(TestIncrementalADTree())
if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
