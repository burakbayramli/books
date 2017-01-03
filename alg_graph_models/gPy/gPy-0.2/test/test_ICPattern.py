import unittest
from gPy.PC import ICPattern, GraphCI, PCCI, GraphSeparator
from test_GraphCI import TestCI

num_runs = 1000
class TestICPattern(TestCI):
    def tryModel(self, model):
        ci = GraphCI(model.adg())
        g = ICPattern(ci)
        self.same_graph(g,model.adg().essential_graph())

class TestICPatternUndirMoral(TestCI):
    def tryModel(self, model):
        ci = GraphCI(model.adg(), undirected=True, moralise=True)
        g = ICPattern(ci)
        self.same_graph(g.moralise(),model.adg().essential_graph().moralise())

class TestICPatternOrder(TestCI):
    def tryModel(self, model):
        ci = GraphCI(model.adg())
        g = ICPattern(ci, model.adg().topological_order())
        self.same_graph(g,model.adg())

class TestPCCIGraph(TestCI):
    def tryModel(self, model):
        ci = PCCI(GraphSeparator(model.adg()))
        g = ICPattern(ci)
        self.same_graph(g,model.adg().essential_graph())

class TestPCCIGraphOrder(TestCI):
    def tryModel(self, model):
        order = model.adg().topological_order()
        ci = PCCI(GraphSeparator(model.adg()), order)
        g = ICPattern(ci, order)
        self.same_graph(g,model.adg())

suite = unittest.makeSuite(TestICPattern)
suite.addTest(TestICPatternUndirMoral())
suite.addTest(TestICPatternOrder())
suite.addTest(TestPCCIGraph())
suite.addTest(TestPCCIGraphOrder())
if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
