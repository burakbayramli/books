import unittest
from gPy.PC import GraphCI
from gPy.Graphs import ADG, UGraph, EssentialGraph
from utils_test import ManySmallModelsTest

num_runs = 1000
class TestCI(ManySmallModelsTest):
    def same_ci(self,aci,bci, verbose=False):
        a = aci.independences()
        b = bci.independences()
        ia = frozenset(a.keys())
        ib = frozenset(b.keys())
        self.assertEqual(ia,ib)
        if verbose:
            print 'independences for a:'
            print aci
            print 'independences for b:'
            print bci
        for k in ia:
            msg = '%s _|_ %s | %s   !=   %s _|_ %s | %s' % (k[0],k[1],a[k],k[0],k[1],b[k])
            self.assertEqual(frozenset(a[k]),frozenset(b[k]),msg)

    def same_graph(self,a,b):
        self.assertEqual(frozenset(a.vertices()), frozenset(b.vertices()))
        for v in a.vertices():
            if frozenset(a.parents(v)) != frozenset(b.parents(v)) or frozenset(a.neighbours(v)) != frozenset(b.neighbours(v)):
                self._assert(False)
#                from util.dotwriter import dot
#                dot(a)
#                dot(b)

            self.assertEqual(frozenset(a.parents(v)), frozenset(b.parents(v)))
            self.assertEqual(frozenset(a.neighbours(v)), frozenset(b.neighbours(v)))

class TestRandUGraphCI(TestCI):
    def tryModel(self, model):
        ci = GraphCI(model.adg(), undirected=True, moralise=True)
        eci = GraphCI(model.adg().essential_graph(), undirected=True, moralise=True)
        self.same_graph(model.adg().moralise(), model.adg().essential_graph().moralise())
        self.same_ci(ci,eci)

class TestRandEssentialGraph(TestCI):
    def tryModel(self, model):
        eg = model.adg().essential_graph()
        remodel = EssentialGraph(vertices=eg.vertices(),lines=eg.lines(),arrows=eg.arrows()).orient()
        self.same_graph(model.adg().essential_graph(), remodel.essential_graph())
        self.same_ci(GraphCI(model.adg()) ,GraphCI(remodel))

class TestGraphCI(TestCI):
    def setUp(self):
        super(TestGraphCI, self).setUp()
        self.g = list(xrange(4))
        self.indeps = list(xrange(4))
        self.g[0] = UGraph(vertices=['a','b','c','d','e']
                      ,lines=[('a','b'),('a','c'),('b','d')
                             ,('b','c'),('c','d'),('c','e')
                             ])
        self.indeps[0] = {('b', 'e'): [frozenset(['c'])], ('d', 'e'):
                [frozenset(['c'])], ('a', 'd'): [frozenset(['c', 'b'])], ('a',
                    'e'): [frozenset(['c'])]}
        self.g[1] = ADG(vertices=['a','b','c','d','e','f']
                   ,arrows=[('a','b'),('b','c'),('d','a')
                           ,('d','e'),('d','f'),('e','c')
                           ,('c','f')
                           ])
        self.indeps[1] = {('e', 'f'): [frozenset(['c', 'd'])], ('a', 'f'):
                [frozenset(['b', 'd']), frozenset(['c', 'd'])], ('a', 'e'):
                [frozenset(['d'])], ('b', 'f'): [frozenset(['a', 'c', 'e']),
                    frozenset(['c', 'd'])], ('c', 'd'): [frozenset(['b',
                        'e']), frozenset(['a','e'])], ('b', 'e'): [frozenset(['a']),
                            frozenset(['d'])], ('a', 'c'): [frozenset(['b',
                                'd']), frozenset(['b', 'e'])], ('b', 'd'):
                            [frozenset(['a'])]}
        self.g[2] = ADG(vertices=['a','b','c','d']
                    ,arrows=[('a','b'),('b','c'),('c','d')])
        self.indeps[2] = {('a', 'd'): [frozenset(['c']), frozenset(['b'])]
                         ,('a', 'c'): [frozenset(['b'])]
                         ,('b', 'd'): [frozenset(['c'])]}
        self.g[3] = ADG(vertices=['a','b','c','d'],arrows=[('a','c'),('b','c'),('c','d')])
        self.indeps[3] = {('a','c'): [frozenset([])]
                         ,('b','c'): [frozenset([])]
                         ,('d','c'): [frozenset([])]
                         }

    def testUndirected(self):
        self.same_indep(self.g[0], True, False, self.indeps[0])

    def testDirected(self):
        self.same_indep(self.g[1], False, False, self.indeps[1])

    def testChain(self):
        self.same_indep(self.g[2], True, True, self.indeps[2])

    def same_indep(self,g,undir,mor,indep):
        found_indep = GraphCI(g,undir,mor).independences()
        self.assertEqual(frozenset(found_indep.keys()), frozenset(indep.keys()))
        for k in found_indep.keys():
            self.assertEqual(frozenset(found_indep[k]), frozenset(indep[k]))

    def testAsia(self):
        from gPy.Examples import asia
        indep =  GraphCI(asia.adg(), True, True).independences()
        for k in indep.keys():
            print k,':',indep[k]

suite = unittest.makeSuite(TestCI)
suite.addTest(TestRandUGraphCI())
suite.addTest(TestRandEssentialGraph())
suite.addTest(TestGraphCI())
if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
