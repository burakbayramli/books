import unittest

import sys
from gPy.Graphs import *
from gPy.Hypergraphs import *
from gPy.IO import read_dnet, read_csv    # read_dnet test in test_IO.py
from gPy.Models import BN
places = 5

class TestStructures(unittest.TestCase):

    def setUp(self):
        from gPy.Variables import Domain
        bnm = BN(domain=Domain())
        bnm.from_dnet(read_dnet('Asia.dnet'))
        self.hypergraph = bnm._hypergraph
        self.adg = bnm._adg
        self.tarjan = UGraph(range(1,11),
                             ((1,2),(1,3),(2,3),(2,10),(3,10),(4,5),
                              (4,7),(5,6),(5,9),(5,7),(6,7),(6,9),
                              (7,8),(7,9),(8,9),(8,10),(9,10)))
        self.tarjan2 = UGraph(range(1,10),
                              ((1,4),(1,3),(2,3),(2,7),(3,5),(3,6),
                               (4,5),(4,8),(5,6),(5,8),(6,7),(6,9),
                               (7,9),(8,9)))
        self.tarjan3 = UGraph(range(1,10),
                              ((1,4),(1,3),(2,3),(2,7),(3,5),(3,6),
                               (4,5),(4,8),(5,6),(5,8),(6,7),(6,9),
                               (7,9),(8,9),
                               (3,4),(3,7),(4,6),(4,7),(5,7),(6,8),(7,8)))
        self.tarjanh1 = Hypergraph([[3,4],[2,4],[1,2,3]])
        self.tarjanh2 = Hypergraph([[3,4],[2,4],[1,2,3],[2,3,4]])
        self.graph1 = UGraph('ABCDEF',('AB','AC','BD','CE','EF'))
        self.graph2 = UGraph('ABCDEF',('AB','AC','BD','CE','EF','BC','CD','DE'))


    def test_maxcardh(self):
        def choose(set):
            he = frozenset(max([sorted(x) for x in set]))
            set.remove(he)
            return he
        eliminated_in, receiver = self.tarjanh1.maximum_cardinality_search(choose)
        self.assertEqual(eliminated_in,{frozenset([2, 4]): frozenset([2]), frozenset([3, 4]): frozenset([3, 4]), frozenset([1, 2, 3]): frozenset([1])})
        self.assertEqual(receiver, {frozenset([2, 4]): frozenset([3, 4]), frozenset([1, 2, 3]): frozenset([2, 4])})
        def tmpfun(): self.tarjanh1.join_forest()
        self.assertRaises(DecomposabilityError,tmpfun)
        
        eliminated_in, receiver = self.tarjanh2.maximum_cardinality_search(choose)
        self.assertEqual(eliminated_in,{frozenset([3, 4]): frozenset([3, 4]), frozenset([1, 2, 3]): frozenset([1]), frozenset([2, 3, 4]): frozenset([2])})
        self.assertEqual(receiver,{frozenset([2, 4]): frozenset([2, 3, 4]), frozenset([1, 2, 3]): frozenset([2, 3, 4]), frozenset([2, 3, 4]): frozenset([3, 4])})
        self.assertEqual(self.tarjanh2.join_forest(choose),UForest(
            (frozenset([2, 4]), frozenset([2, 3, 4]), frozenset([1, 2, 3]), frozenset([3, 4])),
            ((frozenset([2, 4]),frozenset([2, 3, 4])), (frozenset([1, 2, 3]), frozenset([2, 3, 4])),
             (frozenset([2, 3, 4]), frozenset([3, 4])))))

    def test_isdecomp(self):
        self.assertEqual(self.tarjanh1.is_decomposable(),False)
        self.assertEqual(self.tarjanh2.is_decomposable(),True)
        self.assertEqual(self.hypergraph.is_decomposable(),False)

    def test_graham(self):
        self.assertEqual(self.tarjanh1.copy().grahams(),Hypergraph([[2,4],[3,4],[2,3]]))
        self.assertEqual(self.tarjanh2.copy().grahams(),Hypergraph([[]]))
        self.assertEqual(self.hypergraph.copy().grahams(),
                         Hypergraph([['Bronchitis','Smoking'], ['Bronchitis','TbOrCa'],
                                     ['Cancer', 'Smoking'], ['Cancer', 'TbOrCa']]))
        from gPy import Variables
        Variables.clear_default_domain() # bad will put stuff in here
        from gPy.Examples import bad
        self.assertEqual(bad('d1',10).grahams(),Hypergraph([[]]))
        self.assertEqual(bad('d2',20).grahams(),Hypergraph([[]]))
        self.assertEqual(bad('d3',40).grahams(),Hypergraph([[]]))
        self.assertEqual(bad('d4',10).grahams(),Hypergraph([[]]))
        Variables.clear_default_domain() # bad will put stuff in here
                

    def test_maxcard(self):
        def choose(set):
            v = max(set)
            set.remove(v)
            return v
        alpha, alpha_inv = self.tarjan.maximum_cardinality_search(choose)
        self.assertEqual(alpha_inv,range(1,11))

    def test_maxcard2(self):
        def choose(set):
            v = max(set)
            set.remove(v)
            return v
        alpha, alpha_inv = self.tarjan2.maximum_cardinality_search(choose)
        self.assertEqual(alpha_inv,range(1,10))

    def test_triangulate(self):
        def choose(set):
            v = max(set)
            set.remove(v)
            return v
        alpha, alpha_inv = self.tarjan.maximum_cardinality_search(choose)
        tmp = self.tarjan.copy()
        self.assertEqual(tmp,self.tarjan)

    def test_triangulate2(self):
        def choose(set):
            v = max(set)
            set.remove(v)
            return v
        alpha, alpha_inv = self.tarjan2.maximum_cardinality_search(choose)
        tmp = self.tarjan2.copy()
        tmp.triangulate(alpha_inv)
        self.assertEqual(tmp,self.tarjan3)

    def test_triangulate3(self):
        g = self.graph1.copy()
        g.triangulate('ABCDEF')
        self.assertEqual(g,self.graph2)
        g = self.graph1.copy()
        g.triangulate('DBACEF')
        self.assertEqual(g,self.graph1)

    def test_is_triangulated(self):
        self.assertEqual(self.tarjan.is_triangulated(),True)
        self.assertEqual(self.tarjan2.is_triangulated(),False)
        self.assertEqual(self.tarjan3.is_triangulated(),True)
        self.assertEqual(self.hypergraph.two_section().is_triangulated(),False)

    def test_in(self):
        self.assertEqual(('Bronchitis', 'Smoking') in self.hypergraph,True)
        self.assertEqual(['Bronchitis'] in self.hypergraph,False)

    def test_repr(self):
        self.assertEqual(self.hypergraph,eval(repr(self.hypergraph)))

    def test_cp(self):
        self.assertEqual(self.hypergraph.copy(),self.hypergraph)

    def test_graph(self):
        self.assertEqual(self.hypergraph.two_section(),self.adg.moralise())

    def test_rhs(self):
        self.assertEqual(self.hypergraph.redundant_hyperedges(),
                         {frozenset(['VisitAsia']): set([frozenset(['Tuberculosis', 'VisitAsia'])]),
                          frozenset(['Smoking']): set([frozenset(['Bronchitis','Smoking']), frozenset(['Cancer','Smoking'])])})

    def test_isred(self):
        self.assertEqual(self.hypergraph.is_reduced(),False)
        cp = self.hypergraph.copy()
        cp.remove_hyperedges([['VisitAsia'],['Smoking']])
        self.assertEqual(cp.is_reduced(),True)

    def test_red(self):
        reds = set(self.hypergraph.redundant_hyperedges())
        self.assertEqual(reds,set([frozenset(['VisitAsia']), frozenset(['Smoking'])]))
        self.assertEqual(ReducedHypergraph(self.hypergraph.copy(),modify=True),Hypergraph(set([frozenset(['XRay', 'TbOrCa']), frozenset(['Smoking', 'Bronchitis']), frozenset(['VisitAsia', 'Tuberculosis']), frozenset(['TbOrCa', 'Dyspnea', 'Bronchitis']), frozenset(['Smoking', 'Cancer']), frozenset(['TbOrCa', 'Cancer', 'Tuberculosis'])])))

    # def test_would_be(self):
#         self.assertEqual(self.hypergraph.would_be_redundant(['Cancer', 'TbOrCa']),True)
#         self.assertEqual(self.hypergraph.would_be_redundant(['Cancer', 'TbOrCa','Tuberculosis']),True)
#         self.assertEqual(self.hypergraph.would_be_redundant(['Cancer', 'TbOrCa','Bronchitis']),False)
#         self.assertEqual(self.hypergraph.would_be_redundant([]),True)

    def test_cycle(self):
        cp = self.adg.copy()
        def tmpfun(x,y): cp.add_arrow(x,y)
        self.assertRaises(DirectedCycleError,tmpfun,'Dyspnea','VisitAsia')
        self.assertRaises(DirectedCycleError,tmpfun,'Dyspnea','Bronchitis')

    def test_cliques(self):
        self.assertEqual(ReducedHypergraph(self.hypergraph.copy(),modify=True),
                         self.adg.moralise().hypergraph())

    def test_paths(self):
        paths=set()
        for path in self.tarjan.paths(1):
            paths.add(tuple(path[1]))
        self.assert_(set([tuple([1]), (1,2),(1,2,10),(1,2,10,3),(1,2,10,8),
                         (1,2,10,8,9)]) <= paths)
        loops=set()
        for path in self.tarjan.paths(1):
            if self.tarjan.is_neighbour(1,path[1][-1]) and len(path[1]) > 2:
                loops.add(tuple(path[1]))
        self.assertEqual(loops,set([(1,2,3),(1,3,2),(1,2,10,3),(1,3,10,2)]))
                
    def test_jf(self):
        cp = ReducedHypergraph(self.tarjan.hypergraph(),modify=True)
        self.assertEqual(cp.join_forest_ve(),UForest(
            (frozenset([8, 9, 7]), frozenset([2, 3, 10]), frozenset([8, 9, 10]),
             frozenset([4, 5, 7]), frozenset([1, 2, 3]), frozenset([9, 5, 6, 7])),
            ((frozenset([8, 9, 10]), frozenset([8, 9, 7])),
             (frozenset([9, 5, 6, 7]), frozenset([8, 9, 7])),
             (frozenset([8, 9, 10]), frozenset([2, 3, 10])),
             (frozenset([1, 2, 3]), frozenset([2, 3, 10])),
             (frozenset([9, 5, 6, 7]),frozenset([4, 5, 7])))))
        self.assertEqual(cp.join_forest_ve(),cp.join_forest())

    def test_makejf(self):
        hg = self.hypergraph.copy()
        jf = JoinForest(hg,modify=True,
                        elimination_order=['VisitAsia','Tuberculosis','Smoking',
                                           'Cancer','TbOrCa','XRay','Bronchitis',
                                           'Dyspnea'])
        self.assertEqual(jf._uforest,UForest(
            [frozenset(['VisitAsia', 'Tuberculosis']),
             frozenset(['XRay', 'TbOrCa', 'Dyspnea', 'Bronchitis']),
             frozenset(['Smoking', 'Cancer', 'Bronchitis']),
             frozenset(['TbOrCa', 'Cancer', 'Bronchitis']),
             frozenset(['TbOrCa', 'Tuberculosis', 'Cancer'])],
            [(frozenset(['TbOrCa', 'Tuberculosis', 'Cancer']),frozenset(['VisitAsia', 'Tuberculosis'])),
             (frozenset(['TbOrCa', 'Cancer', 'Bronchitis']),frozenset(['TbOrCa', 'Tuberculosis', 'Cancer'])),
             (frozenset(['TbOrCa', 'Cancer', 'Bronchitis']),frozenset(['Smoking', 'Cancer', 'Bronchitis'])),
             (frozenset(['XRay', 'TbOrCa', 'Dyspnea', 'Bronchitis']),frozenset(['TbOrCa', 'Cancer', 'Bronchitis']))]))
        hg = self.hypergraph.copy()
        jf = JoinForest(hg,modify=True,
                        elimination_order=['VisitAsia','Tuberculosis','XRay','Dyspnea',
                                           'Cancer','TbOrCa','Bronchitis','Smoking'])
        self.assertEqual(jf._uforest,UForest(
            [frozenset(['Smoking', 'TbOrCa', 'Bronchitis']),
              frozenset(['XRay', 'TbOrCa']),
              frozenset(['Smoking', 'TbOrCa', 'Cancer']),
              frozenset(['VisitAsia', 'Tuberculosis']),
              frozenset(['TbOrCa', 'Dyspnea', 'Bronchitis']),
              frozenset(['TbOrCa', 'Tuberculosis', 'Cancer'])],
            [(frozenset(['Smoking', 'TbOrCa', 'Cancer']),frozenset(['Smoking', 'TbOrCa', 'Bronchitis'])),
             (frozenset(['XRay', 'TbOrCa']),frozenset(['Smoking', 'TbOrCa', 'Bronchitis'])),
             (frozenset(['TbOrCa', 'Dyspnea', 'Bronchitis']),frozenset(['Smoking', 'TbOrCa', 'Bronchitis'])),
             (frozenset(['TbOrCa', 'Tuberculosis', 'Cancer']),frozenset(['Smoking', 'TbOrCa', 'Cancer'])),
             (frozenset(['TbOrCa', 'Tuberculosis', 'Cancer']),frozenset(['VisitAsia', 'Tuberculosis']))]))
             
suite = unittest.makeSuite(TestStructures)

if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
