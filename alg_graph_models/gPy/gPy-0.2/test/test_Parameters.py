import unittest

import sys
from gPy.Parameters import *
from gPy.IO import read_dnet, read_csv    # read_dnet test in test_IO.py
from gPy.Models import BN
places = 5

class TestParameters(unittest.TestCase):

    def setUp(self):
        from gPy.Variables import Domain
        self.domain = Domain()
        self.bnm = BN(domain=self.domain)
        self.bnm.from_dnet(read_dnet('Asia.dnet'))
        self.cptdict = {}

        # taken directly from Netica output
        self.marginals = [
            Factor((('VisitAsia'),),
                   [0.99,0.01]),
            Factor((('Tuberculosis'),),
                   [0.9896,0.0104]),
            Factor((('Smoking'),),
                   [0.5,0.5]),
            Factor((('Cancer'),),
                   [0.945,0.055]),
            Factor((('TbOrCa'),),
                   [0.93517, 0.064828]),
            Factor((('XRay'),),
                   [0.11029, 0.88971]),
            Factor((('Bronchitis'),),
                   [0.55,0.45]),
            Factor((('Dyspnea'),),
                   [0.56403,0.43597])            
            ]
        # taken directly from Netica output
        self.cond_marginals = [
            Factor((('VisitAsia'),),
                   [0.95192,0.048077]),
            Factor((('Tuberculosis'),),
                   [0,1]),
            Factor((('Smoking'),),
                   [0.52381,0.47619]),
            #other marginals are conditional on these values
            #Factor((('Cancer'),),
            #       [1,0]),
            #Factor((('TbOrCa'),),
            #       [0,1]),
            Factor((('XRay'),),
                   [0.98, 0.02]),
            Factor((('Bronchitis'),),
                   [0.55714,0.44286]),
            Factor((('Dyspnea'),),
                   [0.21143,0.78857])            
            ]
        for cpt in self.bnm:
            self.cptdict[cpt.child()] = cpt

        self.rawdata = read_csv(open('alarm_1K.dat'))

        
    def test_str(self):
        out = ''
        for node in ['VisitAsia','Tuberculosis','Smoking','Cancer',
                     'TbOrCa','XRay','Bronchitis','Dyspnea']:
            out += str(self.cptdict[node])
        self.assertEqual(out,open('Asia.cpts').read())

    def test_cptcheck(self):
        # be nice to check that this is printed out
        errmsg = """
        For child:	Dyspnea
        For row:	Bronchitis=Absent, Dyspnea=Absent, TbOrCa=True
        Sum was:	 0.90 (should be 1.0)
        """
        def bnfromfile(filename): x= BN(domain=self.domain); x.from_dnet(read_dnet(filename)); return x
        self.assertRaises(CPTError,bnfromfile,open('Asia_wrong.dnet'))

    def test_multinplace(self):
        factor = self.cptdict['VisitAsia'] * self.cptdict['Tuberculosis']
        fid = id(factor)
        factor *= self.cptdict['Bronchitis']
        self.assertEqual(fid,id(factor))
        factor2 = self.cptdict['VisitAsia'] * \
                  self.cptdict['Tuberculosis'] * self.cptdict['Bronchitis'] 
        self.samefactor(factor,factor2)
    
    def test_scalarprod(self):
        for factor in self.bnm:
            for scalar in 0,0.3,7:
                tf1 = scalar * factor
                tf2 = factor * scalar
                self.samefactor(tf1,tf2)
                for i, val in enumerate(tf1._data):
                    self.assertAlmostEqual(val,scalar * factor._data[i],places)

    def test_instbyname(self):
        dyspnea = self.cptdict['Dyspnea']
        datum = dyspnea[{'Dyspnea':'Present',
                                       'Bronchitis':'Present',
                                       'TbOrCa':'False'}]
        self.assertAlmostEqual(datum,0.8,places)
        
    def test_getitem(self):
        dyspnea = self.cptdict['Dyspnea']
        datum = dyspnea['Present','Present','False']
        self.assertAlmostEqual(datum,0.8,places)

    def test_z(self):
        factor = 1
        order = ['VisitAsia','Tuberculosis','Smoking',
                 'Cancer','TbOrCa','XRay','Bronchitis',
                 'Dyspnea']
        for name in order:
            newfactor = factor * self.cptdict[name]
            factor *= self.cptdict[name]
            self.samefactor(factor,newfactor)
            self.assertAlmostEqual(factor.z(),1,places)
        self.assertEqual(str(factor),open('Asia.joint').read())
        factor = 1
        for cpt in self.cptdict.values():
            factor *= cpt
        self.assertAlmostEqual(factor.z(),1,places)

    def test_marginals(self):
        joint = 1
        for cpt in self.bnm:
            joint *= cpt
        order = ['VisitAsia','Tuberculosis','Smoking',
                 'Cancer','TbOrCa','XRay','Bronchitis',
                 'Dyspnea']
        varset = set(order)
        for i, var in enumerate(order):
            tmp = varset.copy()
            tmp.remove(var)
            self.samefactor(self.marginals[i],joint.sumout(tmp))
            
    def test_factorprodcommute(self):
        for f1 in self.bnm:
            for f2 in self.bnm:
                tf1 = f1 * f2
                tf2 = f2 * f1
                self.samefactor(tf1,tf2)

    def test_restrict(self):
        bnm = self.bnm.copy(copy_domain=True)
        given = {'Cancer':['Absent'],'TbOrCa':['True']}
        bnm.condition(given)
        joint = 1
        for cpt in bnm:
            joint *= cpt
        order = ['VisitAsia','Tuberculosis','Smoking',
                 'XRay','Bronchitis','Dyspnea']
        varset = set(order)
        varset.update(['Cancer','TbOrCa'])
        for i, var in enumerate(order):
            marginal = joint.sumout(varset - set([var]))
            marginal /= marginal.z()
            self.samefactor(self.cond_marginals[i],marginal)

    def samefactor(self,tf1,tf2):
        self.assertEqual(tf1.variables(),tf2.variables())
        tf2data = tf2.data()
        for i, val in enumerate(tf1.data()):
            self.assertAlmostEqual(val,tf2data[i],places)

suite = unittest.makeSuite(TestParameters)

if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
