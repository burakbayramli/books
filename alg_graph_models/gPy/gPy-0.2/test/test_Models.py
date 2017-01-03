import unittest

import sys
from gPy.Models import *
from gPy.IO import read_dnet,read_csv
from gPy.Variables import SubDomain

places = 5

class TestModels(unittest.TestCase):

    def setUp(self):
        from gPy.Variables import Domain
        self.bnm = BN(domain=Domain()) # don't use default domain
        self.bnm.from_dnet(read_dnet('Asia.dnet'))
        self.cptdict = self.bnm.factors
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


    def test_HMve(self):
        order = ['VisitAsia','Tuberculosis','Smoking',
                 'Cancer','TbOrCa','XRay','Bronchitis',
                 'Dyspnea']
        varset = set(order)
        for i, var in enumerate(order):
            bn = self.bnm.copy(True)
            bn.variable_elimination(varset - set([var]))
            marginal = bn.factor([var])
            self.samefactor(self.marginals[i],marginal)


    def test_HMve_clever(self):
        order = ['VisitAsia','Tuberculosis','Smoking',
                 'Cancer','TbOrCa','XRay','Bronchitis',
                 'Dyspnea']
        varset = set(order)
        for i, var in enumerate(order):
            bn = self.bnm.copy(True)
            bn.variable_elimination(varset - set([var]),False)
            marginal = bn.factor([var])
            self.samefactor(self.marginals[i],marginal)


    def test_HMcopydomain(self):
        domain = self.bnm._domain.copy()
        bn = self.bnm.copy(copy_domain=True)
        bn.condition({'Cancer':['Absent'],'TbOrCa':['True']})
        for variable, values in domain.items():
            self.assertEqual(values,self.bnm._domain[variable])

    def test_HMve_cond(self):
        order = ['VisitAsia','Tuberculosis','Smoking',
                 'XRay','Bronchitis',
                 'Dyspnea']
        varset = set(order)
        varset.update(['Cancer','TbOrCa'])
        for i, var in enumerate(order):
            bn = self.bnm.copy(copy_domain=True)
            bn.condition({'Cancer':['Absent'],'TbOrCa':['True']})
            bn.variable_elimination(varset - set([var]))
            marginal = bn.factor([var])
            marginal /= marginal.z()
            self.samefactor(self.cond_marginals[i],marginal)


    def test_HMve_cond_clever(self):
        order = ['VisitAsia','Tuberculosis','Smoking',
                 'XRay','Bronchitis',
                 'Dyspnea']
        varset = set(order)
        varset.update(['Cancer','TbOrCa'])
        for i, var in enumerate(order):
            bn = self.bnm.copy(copy_domain=True)
            bn.condition({'Cancer':['Absent'],'TbOrCa':['True']})
            bn.variable_elimination(varset - set([var]),False)
            marginal = bn.factor([var])
            marginal /= marginal.z()
            self.samefactor(self.cond_marginals[i],marginal)

    def test_ipf2(self):
        # just tests termination
        alarm = CompactFactor(read_csv(open('alarm_1K.dat')))
        vs = list(alarm.variables())
        vs.append(vs[0])
        marginals = {}
        model = RFR()
        for i in range(len(vs)-1):
            hyperedge = frozenset(vs[i:i+2])
            model *= Factor(hyperedge)
            marginals[hyperedge] = alarm[hyperedge].normalised()
        model.ipf(marginals,0.001)


    def test_ipf(self):
        from gPy.Examples import nondecomp
        joint = 1
        for factor in nondecomp:
            joint *= factor
        joint = joint.normalised()
        marginals = {}
        vs = nondecomp.variables()
        model = RFR()
        for factor in nondecomp:
            fv = factor.variables()
            model *= Factor(fv)
            marginals[fv] = joint.copy().marginalise_away(vs - fv)
        model.ipf(marginals,0.00001)
        fitted_joint = 1
        for factor in model:
            fitted_joint *= factor
        fitted_joint = fitted_joint.normalised()
        self.samefactor(joint,fitted_joint)

    def test_jfm_pre(self):
         order = ['VisitAsia','Tuberculosis','Smoking','Cancer',
                 'TbOrCa','XRay','Bronchitis',
                 'Dyspnea']
         jfm = JFR(self.bnm.copy(True).make_decomposable(order))
         joint = 1
         for cpt in self.bnm:
             joint *= cpt
         joint2 = 1
         for factor in jfm:
             joint2 *= factor
         self.samefactor(joint,joint2)

    def test_jfm_pre2(self):
        order = ['VisitAsia','Tuberculosis','Smoking','Cancer',
                 'TbOrCa','XRay','Bronchitis',
                 'Dyspnea']
        jfm = JFR(self.bnm.copy(True).make_decomposable(order))
        jfm.calibrate()
        joint = 1
        for cpt in self.bnm:
            joint *= cpt
        joint2 = 1
        for clique_factor in jfm:
            joint2 *= clique_factor
        for separator_factor in jfm.separator_factors():
            joint2 /= separator_factor
        self.samefactor(joint,joint2)

             
    def test_jfm(self):
        order = ['VisitAsia','Tuberculosis','Smoking','Cancer',
                 'TbOrCa','XRay','Bronchitis',
                 'Dyspnea']
        jfm = JFR(self.bnm.copy(True).make_decomposable(order))
        jfm.calibrate()
        for i, var in enumerate(order):
            for clique, factor in jfm.items():
                if var in clique:
                    f = factor.copy()
                    f.marginalise_away(f.variables() - set([var]))
                    self.samefactor(self.marginals[i],f)
            for sep, factor in jfm.separator_items():
                clique1, clique2 = tuple(sep)
                if var in clique1 & clique2:
                    f = factor.copy()
                    f.marginalise_away(f.variables() - set([var]))
                    self.samefactor(self.marginals[i],f)

    def test_jfm_cond(self):
        order = ['VisitAsia','Tuberculosis','Smoking','XRay',
                 'Bronchitis',
                 'Dyspnea']
        foo = self.bnm.copy(True)
        jfm = JFR(foo.make_decomposable(order))
        jfm.condition({'Cancer':['Absent'],'TbOrCa':['True']})
        jfm.calibrate()
        for i, var in enumerate(order):
            for clique, factor in jfm.items():
                if var in clique:
                    f = factor.copy()
                    f.marginalise_away(f.variables() - set([var]))
                    f /= f.z()
                    self.samefactor(self.cond_marginals[i],f)
            for sep, factor in jfm.separator_items():
                clique1, clique2 = tuple(sep)
                if var in clique1 & clique2:
                    f = factor.copy()
                    f.marginalise_away(f.variables() - set([var]))
                    f /= f.z()
                    self.samefactor(self.cond_marginals[i],f)



    def samefactor(self,tf1,tf2):
        self.assertEqual(tf1.variables(),tf2.variables())
        for i, val in enumerate(tf1.data()):
            self.assertAlmostEqual(val,tf2.data()[i],places)

        
suite = unittest.makeSuite(TestModels)

if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
