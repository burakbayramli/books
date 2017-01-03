import unittest
from gPy.Models import CBN
from gPy.LearningUtils import CausalWorld
from utils_test import distribution_of, same_factor

num_samples = 100000
class TestCausalWorld(unittest.TestCase):
    def setUp(self):
        from gPy.Examples import minibn
        self._world = CausalWorld(minibn)
        self._skel = minibn
        self._intervention = {'Smoking': frozenset(['nonsmoker'])}
        self._query = CBN.from_bn(minibn.copy(copy_domain=True))
        self._query.intervene(self._intervention)

class TestObserveCausalWorld(TestCausalWorld):
    def runTest(self):
        data = self._world.observe(num_samples)
        p = CBN.from_bn(self._skel.copy(copy_domain=True))
        print 'learning from',data.size(),'samples: BDeu score:',p.bdeu_score(data)
        p.estimate_parameters(data)
        self.failUnless(same_factor(distribution_of(p), distribution_of(self._skel), dp=2, verbose=True))

class TestQueryCausalWorld(TestCausalWorld):
    def runTest(self):
        data = self._world.query(self._intervention, num_samples)
        p = self._query.copy(copy_domain=True)
        print 'learning from',data.size(),'samples: BDeu score:',p.bdeu_score(data)
        p.estimate_parameters(data)
        self.failUnless(same_factor(distribution_of(p), distribution_of(self._query), dp=2, verbose=True))

suite = unittest.makeSuite(TestCausalWorld)
suite.addTest(TestObserveCausalWorld())
suite.addTest(TestQueryCausalWorld())
if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
