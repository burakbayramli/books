import unittest
from utils_test import same_factor, ManyModelsTest, rand_vars, rand_factor
from math import log
import operator
import random

# test the following operations:
# +, -, *, map(log)
# on Factor
# DOES NOT test the case where different number of variables
# are present in each factor.

num_runs = 1000
class TestOperator(unittest.TestCase):
    def uniop(self,op):
        for run in xrange(num_runs):
            vs = rand_vars(min_vars = 1, max_vars = 10, min_vals = 2, max_vals = 4)
            f = rand_factor(vs)
            r = f.copy().map(op)
            for rd,fd in zip(r._data, f._data):
                self.assertAlmostEquals(rd, op(fd))

    def binop(self,op):
        for run in xrange(num_runs):
            vs = rand_vars(min_vars = 1, max_vars = 10, min_vals = 2, max_vals = 4)
            f = rand_factor(vs)
            g = rand_factor(vs)
            r = op(f,g)
            for rd,fd,gd in zip(r._data, f._data, g._data):
                self.assertAlmostEquals(rd, op(fd, gd))

class TestLog(TestOperator):
    def runTest(self):
        self.uniop(log)

class TestAdd(TestOperator):
    def runTest(self):
        self.binop(operator.add)

class TestSub(TestOperator):
    def runTest(self):
        self.binop(operator.sub)

class TestMul(TestOperator):
    def runTest(self):
        self.binop(operator.mul)

class TestExtend(ManyModelsTest):
    def tryModel(self, orig):
        orig  = orig.copy(copy_domain=True)
        for child in orig.variables():
            model = orig.copy(copy_domain=True)
            vals = model.values(child)
            this_one = random.choice(tuple(vals))
            model.condition({child: frozenset([this_one])}, keep_class=True)
            for f in model:
                if child not in f.variables():
                    self.assert_(same_factor(f, orig[f.child()]))
                    continue

                f = f.copy(copy_domain=True)
                f.data_extend({child: vals}, keep_class=True)
                f.change_domain_variable(child, vals)
                self.assertEquals(f.variables(), orig[f.child()].variables())
                for variable in f.variables():
                    self.assertEquals(f.values(variable),orig[f.child()].values(variable))
                i = sorted(f.variables()).index(child)
                for inst in f.insts():
                    if inst[i] == this_one:
                        self.assertEquals(f[inst], orig[f.child()][inst])
                    else:
                        self.assertEquals(f[inst], 0)

class TestExtendIdent(ManyModelsTest):
    def tryModel(self, orig):
        orig  = orig.copy(copy_domain=True)
        for factor in orig:
            f = factor.copy(copy_domain=True)
            extension = dict([(v,f.values(v)) for v in f.variables()])
            f.data_extend(extension)
            self.assert_(same_factor(f,factor))

suite = unittest.makeSuite(TestOperator)
suite.addTest(TestLog())
suite.addTest(TestAdd())
suite.addTest(TestSub())
suite.addTest(TestMul())
suite.addTest(TestExtend())
suite.addTest(TestExtendIdent())
if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
