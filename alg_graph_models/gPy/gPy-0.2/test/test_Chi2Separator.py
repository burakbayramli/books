from gPy.PC import X2Separator, G2Separator, PCCI, ICPattern
from gPy.Data import CompactFactor
from gPy.Variables import Domain
from gPy.Examples import asia
from gPy.Utils import subsetn
from gPy.Graphs import Graph
from gPy.Utils import pairs, powerset
import unittest
from utils_test import xor
from gPy.IO import read_csv

#   problem: how do I know that Chi2Separator is correctly calculating the statistics (v, k)?
#  solution: generate some data and compare against a reputable source (GNU R)
# two binary test cases:
#     P(a)P(a|b) ; P(a = 0) = 0.5, P(b = 0| a = 0) = 0.3, P(b = 0|a = 1) = 0.4)
#     P(a)P(b) P(a = 0) = 0.5, P(b = 0) = 0.3
# data generation:
# > from gPy.Models import BN
# > from gPy.Parameters import CPT, Factor
# > from gPy.Variables import Domain
# > from gPy.LearningUtils import CausalWorld
# > bn0 = BN(domain=Domain(), new_domain_variables={'a': [0,1], 'b':[0,1]})
# > bn0.add_cpts([CPT(Factor(variables=['a'], data=[0.5, 0.5]),child='a')
# >              ,CPT(Factor(variables=['a','b'], data=[0.3, 0.7, 0.4, 0.6]),child='b')
# >              ])
# > w = CausalWorld(bn0)
# > samples = w.observe(10000)
# > print samples
# > bn1 = BN(domain=Domain(), new_domain_variables={'a': [0,1], 'b':[0,1]})
# > bn1.add_cpts([CPT(Factor(variables=['a'], data=[0.5, 0.5]),child='a')
# >              ,CPT(Factor(variables=['b'], data=[0.3, 0.7]),child='b')
# >              ])
# > w = CausalWorld(bn1)
# > samples = w.observe(10000)
# > print samples
#
# test in R using below data:
# > independ <- read.table('independ.data',header=TRUE)
# > depend <- read.table('depend.data', header=TRUE)
# > idm <- matrix(independ$count,nrow=2)
# > dm <- matrix(depend$count,nrow=2)
# > chisq.test(dm,correct=FALSE)
#         Pearson's Chi-squared test
# X-squared = 109.1547, df = 1, p-value < 2.2e-16
# > chisq.test(idm,correct=FALSE)
#         Pearson's Chi-squared test
# X-squared = 0.1236, df = 1, p-value = 0.7252


# TODO: test G2

class TestX2R(unittest.TestCase):
    def runTest(self):
        # results from R version 2.6.2 (2008-02-08) ISBN 3-900051-07-0
        r_dx2 = 109.1547
        r_dp  = 0
        r_ddf = 1
        r_ix2 = 0.1236
        r_ip  = 0.7252
        r_idf = 1

        vars = ['a','b']
        vals = {'a': [0,1], 'b':[0,1]}
        ddata = (vars,vals,vars,[(0,0,1509), (0,1,3538), (1,0,1974), (1,1,2979)])
        iddata = (vars,vals,vars,[(0,0,1474), (0,1,3610), (1,0,1441), (1,1,3475)])

        x2d = X2Separator(CompactFactor(ddata, domain=Domain()))
        x2i = X2Separator(CompactFactor(iddata, domain=Domain()))

        dp, dx2, dd = x2d.test_independ('a','b',set())

        self.assertEquals(dd,r_ddf)
        self.assertAlmostEquals(dx2,r_dx2,4)
        self.assertAlmostEquals(dp,r_dp,4)

        ip, ix2, id = x2i.test_independ('a','b',set())

        self.assertEquals(id,r_idf)
        self.assertAlmostEquals(ix2,r_ix2,4)
        self.assertAlmostEquals(ip,r_ip,4)
        print '     x2 depend: p =',dp,'x2 =',dx2,'d =',dd
        print '   x2 independ: p =',ip,'x2 =',ix2,'d =',id

        # regression (NOT known correct! NOT from R)
        r_dg2 = 109.389800878 
        r_dp  = 0
        r_ddf = 1
        r_ig2 = 0.123551704118
        r_ip  = 0.725213854926 
        r_idf = 1

        g2d = G2Separator(CompactFactor(ddata, domain=Domain()))
        g2i = G2Separator(CompactFactor(iddata, domain=Domain()))
        dp, dg2, dd = g2d.test_independ('a','b',set())
        self.assertEquals(dd,r_ddf)
        self.assertAlmostEquals(dg2,r_dg2,4)
        self.assertAlmostEquals(dp,r_dp,4)

        ip, ig2, id = g2i.test_independ('a','b',set())
        self.assertEquals(id,r_idf)
        self.assertAlmostEquals(ig2,r_ig2,4)
        self.assertAlmostEquals(ip,r_ip,4)
        print '!!   g2 depend: p =',dp,'g2 =',dg2,'d =',dd
        print '!! g2 independ: p =',ip,'g2 =',ig2,'d =',id


class TestX2MissingR(unittest.TestCase):
    def runTest(self):
        # results from R version 2.6.2 (2008-02-08) ISBN 3-900051-07-0
        r_dx2 = 3453.429
        r_dp = 2.2e-16
        r_ddf = 4
        # regression (NOT known correct! NOT from R)
        r_ix2 = 1.08094376504
        r_ip = 0.58247332853
        r_idf = 2

        vars = ['a','b']
        vals = {'a': [0,1,2], 'b':[0,1,2]}
        ddata  = (vars,vals,vars,[(0,0,640),(0,1,1947),(0,2,648),(1,0,1709),(1,1,1364),(1,2,335),(2,0,0),(2,1,3357),(2,2,0)])
        iddata = (vars,vals,vars,[(0,0,994),(0,1,2359),(0,2,0),(1,0,1027),(1,1,2312),(1,2,0),(2,0,989),(2,1,2319),(2,2,0)])

        x2d = X2Separator(CompactFactor(ddata, domain=Domain()))
        x2i = X2Separator(CompactFactor(iddata, domain=Domain()))

        dp, dx2, dd = x2d.test_independ('a','b',set())
        self.assertEquals(dd,r_ddf)
        self.assertAlmostEquals(dx2,r_dx2,3)
        self.assertAlmostEquals(dp,r_dp,4)

        ip, ix2, id = x2i.test_independ('a','b',set())
        self.assertEquals(id,r_idf)
        self.assertAlmostEquals(ix2,r_ix2,4)
        self.assertAlmostEquals(ip,r_ip,4)
        # since there are more zero counts there should be fewer dof.
        self.assert_(id < dd)

        print '     x2 depend: p =',dp,'x2 =',dx2,'d =',dd
        print '!! x2 independ: p =',ip,'x2 =',ix2,'d =',id


        # regression (NOT known correct! NOT from R)
        r_dg2 = 4224.3017675
        r_dp = 0.0
        r_ddf = 4
        r_ig2 = 1.07917600592
        r_ip = 0.582988392395
        r_idf = 2

        g2d = G2Separator(CompactFactor(ddata, domain=Domain()))
        g2i = G2Separator(CompactFactor(iddata, domain=Domain()))
        dp, dg2, dd = g2d.test_independ('a','b',set())
        self.assertEquals(dd,r_ddf)
        self.assertAlmostEquals(dg2,r_dg2,4)
        self.assertAlmostEquals(dp,r_dp,4)

        ip, ig2, id = g2i.test_independ('a','b',set())
        self.assertEquals(id,r_idf)
        self.assertAlmostEquals(ig2,r_ig2,4)
        self.assertAlmostEquals(ip,r_ip,4)
        print '!!   g2 depend: p =',dp,'g2 =',dg2,'d =',dd
        print '!! g2 independ: p =',ip,'g2 =',ig2,'d =',id


class TestNoData(unittest.TestCase):
    def runTest(self):
        # construct factors of various sizes with no data
        for sz in xrange(6):
            vars = ['V'+str(i) for i in xrange(sz)]
            vals = dict([(v,[0,1]) for v in vars])
            data = (vars,vals,vars,[])
            for v_on in subsetn(vars, sz):
                inst = []
                for v in vars:
                    if v in v_on:
                        inst.append(1)
                    else:
                        inst.append(0)
                data[3].append(tuple(inst+[0]))
            d = CompactFactor(data,domain=Domain())
            x2 = X2Separator(d)
            g2 = G2Separator(d)
            for a,b in pairs(vars):
                for s in powerset(set(vars) - set([a,b])):
                    x2p, x2s, x2d = x2.test_independ(a, b, set(s))
                    g2p, g2s, g2d = g2.test_independ(a, b, set(s))
                    # one degree of freedom
                    self.assertEquals(x2d, 0)
                    self.assertEquals(g2d, 0)
                    # default to independent
                    self.assertEquals(x2p, 1)
                    self.assertEquals(g2p, 1)
                    # zero statistics (no data)
                    self.assertEquals(x2s, 0)
                    self.assertEquals(g2s, 0)

class TestG2TetradXor(unittest.TestCase):
    def runTest(self):
        data = CompactFactor(read_csv(open('tetrad_xor.csv')),domain=Domain())
        ci = PCCI(G2Separator(data))
        print ci._ind
        for a,b in pairs(data.variables()):
            if a == 'X1' and b == 'X2' or a == 'X2' and b == 'X1':
                self.assert_(ci.has_independence(a, b))
                self.assert_(not ci.has_independence_involving(a,b,'X3'))
            else:
                print a,b
                self.assert_(not ci.has_independence(a,b))
        data = CompactFactor(read_csv(open('tetrad_xor.csv')),domain=Domain())
        ci = PCCI(G2Separator(data))
        for a,b in pairs(data.variables()):
            if a == 'X1' and b == 'X2' or a == 'X2' and b == 'X1':
                self.assert_(ci.has_independence(a, b))
                self.assert_(not ci.has_independence_involving(a,b,'X3'))
            else:
                print a,b
                self.assert_(not ci.has_independence(a,b))

class TestG2TetradAsia(unittest.TestCase):
    def setUp(self):
        self._asia_pdag = asia.adg().essential_graph()
        self._tetrad_pdag = Graph(vertices=self._asia_pdag.vertices()
                                 ,arrows=[('Tuberculosis','TbOrCa'),('Cancer','TbOrCa')]
                                 ,lines=[('Smoking','Cancer'),('Smoking','Bronchitis')
                                        ,('Bronchitis','Dyspnea')])

    def runTest(self):
        data = CompactFactor(read_csv(open('tetrad_asia.csv')),domain=Domain())
        ci = PCCI(G2Separator(data))
        g = ICPattern(ci)
        self.assertEquals(g.shd(self._asia_pdag),5)
        self.assertEquals(self._tetrad_pdag.shd(self._asia_pdag),4)
        # I think tetrad is wrong (in terms of implementation)
        self.assertEquals(g.shd(self._tetrad_pdag),1)

suite = unittest.makeSuite(TestX2R)
suite.addTest(TestX2MissingR())
suite.addTest(TestNoData())
suite.addTest(TestG2TetradXor())
suite.addTest(TestG2TetradAsia())
if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
