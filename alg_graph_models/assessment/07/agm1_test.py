import unittest

from gPy.Models import JFR
from agm1 import pedigree

class Testagm1(unittest.TestCase):

    def setUp(self):
        jf = JFR(pedigree.copy(),modify=True)
        jf.calibrate()
        self.jf = jf
        self.g1 = [0.2, 0.3, 0.2, 0.1, 0.1, 0.1]
        self.g12 = [0.2025, 0.27, 0.225, 0.09, 0.15, 0.0625]

    def test_g1(self):
        probs = self.jf.var_marginal('G1').data()
        for i, p in enumerate(probs):
            self.assertAlmostEqual(p,self.g1[i])

    def test_g12(self):
        probs = self.jf.var_marginal('G12').data()
        for i, p in enumerate(probs):
            self.assertAlmostEqual(p,self.g12[i])
    
suite = unittest.makeSuite(Testagm1)

if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
