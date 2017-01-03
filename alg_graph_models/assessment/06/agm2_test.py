import unittest

from agm2 import HM_q2
from timeit import Timer
places = 5
import gPy.Parameters
gPy.Parameters.precision = 8

class Testagm2(unittest.TestCase):

    def setUp(self):
        from gPy.Examples import asia
        self.variables = asia.variables()
        self.exhm = HM_q2(asia)
        self.exhm_cond = self.exhm.copy(copy_domain=True)
        self.exhm_cond.condition({'TbOrCa':['true']},keep_class=True)

    def test_clever_elimination(self):
        for variable in self.variables:
            others = self.variables - set([variable])
            m1 = self.exhm.copy()
            m1.variable_elimination(others)
            m2 = self.exhm.copy()
            m2.clever_variable_elimination(others)
            self.samefactor(m1[[variable]],m2[[variable]])
 
    def test_clever_elimination2(self):
        for variable in self.variables:
            others = self.variables - set([variable])
            m1 = self.exhm_cond.copy()
            m1.variable_elimination(others)
            f1 = m1[[variable]]
            f1 /= f1.z()
            m2 = self.exhm_cond.copy()
            m2.clever_variable_elimination(others)
            f2 = m2[[variable]]
            f2 /= f2.z()
            self.samefactor(f1,f2)


    def samefactor(self,tf1,tf2):
        self.assertEqual(tf1.variables(),tf2.variables())
        for i, val in enumerate(tf1.data()):
            self.assertAlmostEqual(val,tf2.data()[i],places)

suite = unittest.makeSuite(Testagm2)

if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
