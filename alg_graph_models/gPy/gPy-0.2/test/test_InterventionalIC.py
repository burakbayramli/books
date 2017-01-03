from gPy.PC import GraphCI, InterventionalICPattern
from gPy.Models import BN, CBN
from gPy.Parameters import CPT, Factor
from gPy.Variables import Domain
from utils_test import ManySmallModelsTest, ManyModelsTest
from gPy.Utils import powerset
import unittest

class TestInter(unittest.TestCase):
    def setUp(self):
        # model a -> b
        bn = {}
        self.arr = []
        bn[0] = BN(domain=Domain(), new_domain_variables={'a': [0,1], 'b':[0,1]})
        bn[0].add_cpts([CPT(Factor(variables=['a'], data=[0.5, 0.5]),child='a')
                       ,CPT(Factor(variables=['a','b'], data=[0.3, 0.7, 0.4, 0.6]),child='b')
                       ])
        self.arr.append([('a','b')])
        bn[1] = BN(domain=Domain(), new_domain_variables={'a': [0,1], 'b':[0,1], 'c':[0,1]})
        bn[1].add_cpts([CPT(Factor(variables=['a'], data=[0.5, 0.5]),child='a')
                       ,CPT(Factor(variables=['a','b'], data=[0.3, 0.7, 0.4, 0.6]),child='b')
                       ,CPT(Factor(variables=['c','b'], data=[0.1, 0.9, 0.2, 0.8]),child='c')
                       ])
        self.arr.append([('a','b'),('b','c')])
        self.cbn = [CBN.from_bn(bn[i]) for i in bn.keys()]

    def runTest(self):
        for i, cbn in enumerate(self.cbn):
            obs, a, b = frozenset(), frozenset('a'), frozenset('b')
            ci = {}
            ga = cbn.copy()
            ga.intervene({'a':frozenset([0])})
            ga = ga.adg()
            gb = cbn.copy()
            gb.intervene({'b':frozenset([0])})
            gb = gb.adg()
            gab = cbn.copy()
            gab.intervene({'a':frozenset([0]), 'b':frozenset([0])})
            gab = gab.adg()
            ci[obs] = GraphCI(cbn.adg())
            ci[a] = GraphCI(ga)
            ci[b] = GraphCI(gb)
            ci[a|b] = GraphCI(gab)
            self.go(obs,a,b,self.arr[i],ci)

    def go(self,obs,a,b,arr,ci):
        pass

class TestObs(TestInter):
    def go(self,obs,a,b,arr,ci):
        # observational 
        iic = InterventionalICPattern({obs:ci[obs]})
        self.assertEquals(iic.arrows(), [])
        self.assertEquals(iic.lines(), arr)

class TestInterA(TestInter):
    def go(self,obs,a,b,arr,ci):
        # manipulate a
        iic = InterventionalICPattern({obs:ci[obs], a:ci[a]})
        self.assertEquals(iic.lines(), [])
        self.assertEquals(iic.arrows(), arr)

class TestInterB(TestInter):
    def go(self,obs,a,b,arr,ci):
        # manipulate b
        iic = InterventionalICPattern({obs:ci[obs], b:ci[b]})
        self.assertEquals(iic.lines(), [])
        self.assertEquals(iic.arrows(), arr)

class TestInterAB(TestInter):
    def go(self,obs,a,b,arr,ci):
        # manipulate a and b
        iic = InterventionalICPattern({obs:ci[obs], a|b:ci[a|b]})
        self.assertEquals(iic.lines(), [arr[0]])
        xs = [x for x, y in arr if y == 'c' or x == 'c']
        if xs:
            self.assertEquals(iic.arrows(),[arr[1]])
        else:
            self.assertEquals(iic.arrows(),[])

# test that if we perform every intervention, and gather the GraphCIs
# for each, InterventionalICPattern recovers the true ADG
class TestIICGraph(ManySmallModelsTest):
    def tryModel(self, model):
        # perform every interventional query
        ci = {}
        for xs in powerset(model.variables()):
            # build the intervention q
            q = {}
            for x in xs:
                q[x] = frozenset([set(model.values(x)).pop()])

            # generate the model
            model_do = CBN.from_bn(model.copy(copy_domain=True))
            model_do.intervene(q)
            ci[frozenset(q.keys())] = GraphCI(model_do.adg())

        iic = InterventionalICPattern(ci)
        self.assertEquals(iic.lines(), [])
        self.assertEquals(iic.orient(), model.adg())

# test that if we perform one optimal intervention then the true adg is
# recovered
class TestIICGraph(ManySmallModelsTest):
    def tryModel(self, model):
        model_do = CBN.from_bn(model.copy(copy_domain=True))
        # perform every interventional query
        ci = {}
        for q in model_do.good_interventions():
            # generate the model
            model_do = CBN.from_bn(model.copy(copy_domain=True))
            model_do.intervene(q)
            ci[frozenset(q.keys())] = GraphCI(model_do.adg())

        iic = InterventionalICPattern(ci)
        self.assertEquals(iic.lines(), [])
        self.assertEquals(iic.orient(), model.adg())

# test if we perform one fewer of the optimal interventions, do we
# still find the adg? hopefully not.

# now try from some data

suite = unittest.makeSuite(TestInter)
suite.addTest(TestObs())
suite.addTest(TestInterA())
suite.addTest(TestInterB())
suite.addTest(TestInterAB())
suite.addTest(TestIICGraph())
suite.addTest(TestIICGraph())
if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
