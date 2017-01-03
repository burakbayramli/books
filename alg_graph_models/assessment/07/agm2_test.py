import unittest

places = 6

class Testagm2(unittest.TestCase):

    def setUp(self):
        from gPy.Examples import asia
        from gPy.Parameters import CPT
        from gPy.Models import JFR
        from agm2 import myBN

        cpts = []
        for cpt in asia:
            if cpt.child() not in ('Smoking', 'Cancer', 'Bronchitis'):
                cpts.append(cpt)
        cpts.append(CPT((asia['Cancer']*asia['Smoking']).sumout(['Smoking']),
                        child='Cancer'))
        cpts.append(CPT((asia['Bronchitis']*asia['Smoking']).sumout(['Smoking']),
                        child='Bronchitis'))
        self.bn = myBN(cpts)
        jf = JFR(self.bn.copy(),modify=True)
        jf.calibrate()
        self.order = ('Cancer', 'Bronchitis', 'TbOrCa', 'XRay',
                      'VisitAsia', 'Tuberculosis', 'Dyspnea')
        self.margs = []
        for v in self.order:
            # these checked to be correct with Netica
            self.margs.append(jf.var_marginal(v))
        
        
    def test_basic(self):
        fimargs = self.bn.forward_inference()
        for i, v in enumerate(self.order):
            self.samefactor(fimargs[v],self.margs[i])

    def samefactor(self,tf1,tf2):
        self.assertEqual(tf1.variables(),tf2.variables())
        for i, val in enumerate(tf1.data()):
            self.assertAlmostEqual(val,tf2.data()[i],places)

    
suite = unittest.makeSuite(Testagm2)

if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
