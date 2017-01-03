from utils_test import cbn_test_cases, ManyModelsTest, ManySmallModelsTest, rand_factor_data
from gPy.Examples import asia
from gPy.Graphs import DirectedCycleError
from gPy.Parameters import CPT,Factor
from gPy.Utils import swap, is_finite
from gPy.LearningUtils import *
from gPy.Models import CBN
from random import choice, uniform
import unittest

class TestSHD(ManyModelsTest):
    def sane(self,a,b):
        # reflexive is zero
        self.assertEquals(a.shd(a),0)
        self.assertEquals(b.shd(b),0)
        # non-negative
        self.assert_(a.shd(b) >= 0)
        # symmetric
        self.assertEquals(a.shd(b), b.shd(a))

    def tryModel(self, bn):
        g = bn.adg().copy()
        eg = g.essential_graph()

        self.sane(g,eg)

        # the SHD between a graph and its essential graph is equal to
        # the number of undirected edges in the essential graph
        self.assertEquals(eg.shd(g),len(eg.lines()))

        if g.arrows():
            # pick a directed edge at random and remove increases distance by 1
            h = g.copy()
            x, y = choice(h.arrows())
            h.remove_arrow(x, y)
            self.assertEquals(h.shd(g),1)
            self.sane(h,g)
            self.sane(h,eg)
            # relate this change to the essential graph
            if (x,y) in eg.lines() or (y,x) in eg.lines():
                self.assertEquals(eg.shd(h), eg.shd(g))
            else:
                self.assertEquals(eg.shd(h), eg.shd(g) + 1)

            # pick a directed edge at random and orient differently increases distance by 1
            h = g.copy()
            x, y = choice(h.arrows())
            h.remove_arrow(x, y)
            self.assertEquals(h.shd(g),1)
            self.sane(h,g)
            self.sane(h,eg)
            # relate this change to the essential graph
            if (x,y) in eg.lines() or (y,x) in eg.lines():
                self.assertEquals(eg.shd(h), eg.shd(g))
            else:
                self.assertEquals(eg.shd(h), eg.shd(g) + 1)

        # pick an undirected edge at random and orient correctly decreases distance by 1
        if eg.lines():
            h = eg.copy()
            x, y = choice(h.lines())
            h.remove_line(x, y)
            if g.is_parent(x, y):
                h.add_arrow(x, y)
            else:
                h.add_arrow(y, x)
            self.sane(h,g)
            self.sane(eg,h)
            self.assertEquals(h.shd(g), eg.shd(g) - 1)

        # pick an unconnected pair (if it exists) and add an edge (changes distance by 1)
        unk = set(g.arrows()) | set(g.lines())
        unk |= set(map(swap, unk))
        unk = set([(x, y) for x in g.vertices() for y in g.vertices() if x != y]) - unk
        if unk:
            h = g.copy()
            while unk:
                x, y = choice(list(unk))
                try:
                    h.add_arrow(x, y)
                    self.sane(h,g)
                    self.sane(h,eg)
                    self.assertEquals(h.shd(g),1)
                    self.assertEquals(h.shd(eg),eg.shd(g)+1)
                except DirectedCycleError:
                    unk.remove((x,y))
                break

class TestBDeu(ManyModelsTest):
    def tryModel(self, model):
        data = CausalWorld(model.copy())
        data.observe(1000)
        self.assertAlmostEquals(bdeu(model.adg(), data), model.bdeu_score(data)[0])

class TestDKLScale(ManySmallModelsTest):
    def tryModel(self, model):
        kl = dkl(model,model)
        self.assert_(is_finite(kl))

        cbn = CBN.from_bn(model.copy(copy_domain=True))
        v = choice(tuple(cbn.variables()))
        f = cbn[v]
        dat = rand_factor_data(len(f.data()))
        change_one = None
        for i,(a,b) in enumerate(zip(f.data(),dat)):
            if round(a-b,4) == 0:
                dat[i] += uniform(1.0,100.0)

        cbn._replace_factor( v
                          , CPT(Factor(variables=f.variables()
                               ,data=dat
                               ,domain=cbn), v, cpt_force=True))
        ikl = dkl(model,cbn)
        self.assert_(is_finite(ikl))
        self.assert_(ikl >= kl)
        kl = dkl(cbn,cbn)
        self.assert_(is_finite(kl))
        ikl_ = dkl(cbn,model)
        self.assert_(ikl_ >= kl)

class TestDKL(ManySmallModelsTest):
    def tryModel(self, model):
        self.assertAlmostEquals(dkl(model,model),0)
        cbn = CBN.from_bn(model.copy(copy_domain=True))
        v = choice(tuple(cbn.variables()))
        f = cbn[v]
        dat = rand_factor_data(len(f.data()))
        change_one = None
        for i,(a,b) in enumerate(zip(f.data(),dat)):
            if round(a-b,4) == 0:
                dat[i] += 10.0
                break

        cbn._replace_factor( v
                          , CPT(Factor(variables=f.variables()
                               ,data=dat
                               ,domain=cbn), v, cpt_force=True))
        kl = dkl(model,cbn)
        self.assert_(kl > 0)
        kl_ = dkl(cbn,model)
        self.assert_(kl_ > 0)

suite = unittest.makeSuite(TestBDeu)
suite.addTest(TestSHD())
suite.addTest(TestDKLScale())
suite.addTest(TestDKL())
if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
