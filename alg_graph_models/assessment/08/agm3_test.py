import unittest
from gPy.Models import FR
from gibbs import gibbs_sample

import sys
sys.argv.append('delete_me')

from agm3 import factors

class Testagm3(unittest.TestCase):

    def test_format(self):
        fr = FR(factors)
        sample =  gibbs_sample(fr,100,0)
        self.assertEqual(type(sample),type(('foo','barr')))
        self.assertEqual(len(sample),100)
        for s in sample:
            self.assertEqual(len(s),100)
            for x in s:
                self.assertEqual(x in [0,1],True)
        
suite = unittest.makeSuite(Testagm3)

if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
