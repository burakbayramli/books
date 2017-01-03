import unittest

from agm3 import HM_q3

class Testagm3(unittest.TestCase):

    def setUp(self):
        from gPy.Examples import asia
        self.ex = HM_q3(asia)

    def test_gibbs(self):
        x = self.ex.gibbs_sample(200,10)
        self.assertEqual(len(x),200)
        self.assertEqual(type(x),type(()))
        for it in x:
            self.assertEqual(len(it),8)
            self.assertEqual(type(it),type(()))
        for i in range(8):
            vals = set()
            for it in x:
                vals.add(it[i])
            self.assert_(len(vals) <= 2)
        
suite = unittest.makeSuite(Testagm3)

if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
