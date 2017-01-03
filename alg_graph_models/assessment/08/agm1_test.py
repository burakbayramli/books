import unittest

from agm1 import bnq1, ci

class Testagm1(unittest.TestCase):

    def test_ci(self):
        self.assertEqual(ci(bnq1,
                            frozenset(['G5','G6']),
                            frozenset(['G1','G2','G3']),
                            frozenset(['G4'])
                            ),
                         True)
        self.assertEqual(ci(bnq1,
                            frozenset(['G5']),
                            frozenset(['G6']),
                            frozenset(['G4'])
                            ),
                         True)
        self.assertEqual(ci(bnq1,
                            frozenset(['G2']),
                            frozenset(['G3']),
                            frozenset(['G1'])
                            ),
                         True)


    def test_pairs(self):
        for v in bnq1.vertices():
            v = frozenset([v])
            for w in bnq1.vertices():
                w = frozenset([w])
                if v != w:
                    self.assertEqual(ci(bnq1,v,w,frozenset()),False)

        
suite = unittest.makeSuite(Testagm1)

if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
