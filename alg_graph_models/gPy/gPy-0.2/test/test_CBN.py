import unittest
from gPy.Models import CBN
from gPy.Samplers import GibbsSampler
from gPy.Data import CompactFactor
from gPy.Variables import Domain
from utils_test import distribution_of, same_factor, ManySmallModelsTest

class TestCBN(unittest.TestCase):
    def setUp(self):
        from gPy.Examples import minibn, asia
        self._minibn = minibn.copy(copy_domain=True)
        self._minibn_do = CBN.from_bn(self._minibn.copy(copy_domain=True))
        self._minibn_do.intervene({'Smoking': frozenset(['smoker'])})

        self._asia = asia.copy(copy_domain=True)
        self._asia_do = CBN.from_bn(self._asia.copy(copy_domain=True))
        self._asia_do.intervene({'Dyspnea': frozenset(['present'])})

#class TestDot(TestCBN):
#    def runTest(self):
#        from util.dotwriter import dot
#        dot(self._asia.adg(),name='asia_bn') 
#        dot(self._asia_do.adg(),name='cancer')

class TestNoParents(ManySmallModelsTest):
    def tryModel(self, model):
        for var in model.variables():
            # intervene at each variable in turn
            model_do = CBN.from_bn(model.copy(copy_domain=True))
            # check that the hyperedges correspond to all factors
            self.assertEquals(frozenset([v for v,x in model_do.items()]),
                              frozenset(model_do._hypergraph.hyperedges()))
            val = frozenset([set(model.values(var)).pop()])
            model_do.intervene({var: val})
            self.assertEquals(len(model_do.parents(var)), 0)
            self.assertEquals(model_do.values(var), val)
            self.assertEquals(len(model_do[var].data()), 1)
            self.assertEquals(frozenset([v for v,x in model_do.items()]),
                              frozenset(model_do._hypergraph.hyperedges()))
            self.assert_(model_do[var].data()[0] > 0)
            for other_var in model.variables():
                if other_var == var:
                    continue
                self.assertEquals(model_do.parents(other_var), model.parents(other_var))
                if var in model.parents(other_var):
                    self.assertEquals(frozenset(model_do[other_var].variables()), frozenset(model[other_var].variables()))
                    for inst in model_do[other_var].insts():
                        self.assertAlmostEquals(model_do[other_var][inst], model[other_var][inst])
                else:
                    self.assert_(same_factor(model_do[other_var], model[other_var]))

# can also test that GraphCI reads off the appropriate CIs?

class TestGibbsEstimation(TestCBN):
    def runTest(self):
        samples = GibbsSampler(self._minibn).samples(100000)
        data = CompactFactor(samples,domain=Domain())

        p = CBN.from_bn(self._minibn.copy(copy_domain=True))
        p.estimate_parameters(data)
        self.failUnless(same_factor(distribution_of(p), distribution_of(self._minibn), dp=2, verbose=True))

class TestGibbsInterventionEstimation(TestCBN):
    def runTest(self):
        samples = GibbsSampler(self._minibn_do).samples(100000)
        data = CompactFactor(samples,domain=Domain())

        p = self._minibn_do.copy(copy_domain=True)
        p.estimate_parameters(data)
        self.failUnless(same_factor(distribution_of(p), distribution_of(self._minibn_do), dp=2, verbose=True))

suite = unittest.makeSuite(TestCBN)
suite.addTest(TestNoParents())
suite.addTest(TestGibbsEstimation())
suite.addTest(TestGibbsInterventionEstimation())
if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
