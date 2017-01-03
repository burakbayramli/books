import unittest
from gPy.Utils import *
from gPy.gPyC import chisqprob
from math import log, exp
import random, time, operator

def factorial(n):
    if n <= 1:
        return 1
    return reduce(operator.mul, xrange(1,n+1))

def binom_fac(n,m):
    return factorial(n) / (factorial(m) * factorial(n - m))

class TestBinom(unittest.TestCase):
    def runTest(self):
        for i in xrange(100):
            for j in xrange(i):
                self.assertEqual(binom(i,j), binom_fac(i,j))

class TestLogSumExp(unittest.TestCase):
    def runTest(self):
        for i in xrange(10000):
            sz = random.randrange(10,200)
            self.xs = [random.gauss(0,1) for i in xrange(sz)]
            self.assertAlmostEqual(logsumexp(self.xs), log(sum([exp(x) for x in self.xs])))


class TestChiSqProb(unittest.TestCase):
    def runTest(self):
        # these values are from pchisq as found in R version 2.5.1
        # it would be nice if there were some properties of chisqprob
        # that could be verified but I could not find any.
        self.assertAlmostEquals(chisqprob(1,1),0.6826895,6)
        for k in xrange(100):
            self.assertEquals(chisqprob(0,k), 0)
        for i,val in enumerate([0.0000000,0.6826895,0.6321206,0.6083748,
                                0.5939942,0.5841198,0.5768099,0.5711201,
                                0.5665299,0.5627258,0.5595067]):
            self.assertAlmostEquals(chisqprob(i,i),val,6)

class TestSubsets(unittest.TestCase):
    def setUp(self):
        a = [1,2,3]
        b = frozenset([(), (1,), (2,), (3,),(1,2),(1,3),(2,3),(1,2,3)])
        self.test_cases = [(a,b)]

    def runTest(self):
        pass

class TestSubsetn(TestSubsets):
    def runTest(self):
        for a, b in self.test_cases:
            for i in xrange(0,len(a)+1):
                xs = []
                for x in subsetn(a,i):
                    # check necessity
                    x = tuple(x)
                    self.failUnless(x in b)
                    xs.append(x)

                xs = frozenset(xs)
                for y in b:
                    if len(y) != i:
                        continue
                    # check sufficiency
                    self.failUnless(y in xs)

class TestPowerset(TestSubsets):
    def runTest(self):
        for a, b in self.test_cases:
            xs = []
            for x in powerset(a):
                xs.append(tuple(x))
            xs = frozenset(xs)
            self.assertEquals(xs, b)

class TestPowersetn(TestSubsets):
    def runTest(self):
        for a, b in self.test_cases:
            for i in xrange(0,len(a)+1):
                xs = []
                for x in powersetn(a,i):
                    # check necessity
                    x = tuple(x)
                    self.failUnless(x in b)
                    xs.append(x)

                xs = frozenset(xs)
                for y in b:
                    if len(y) > i:
                        continue
                    # check sufficiency
                    self.failUnless(y in xs)

class TestIsNan(unittest.TestCase):
    def runTest(self):
        self.assert_(not is_nan(infinity))
        self.assert_(not is_nan(negative_infinity))

        self.assert_(is_nan(infinity-infinity))
        self.assert_(not is_nan(infinity-negative_infinity))
        self.assert_(not is_nan(negative_infinity-infinity))
        self.assert_(is_nan(negative_infinity-negative_infinity))

        self.assert_(not is_nan(infinity+infinity))
        self.assert_(is_nan(infinity+negative_infinity))
        self.assert_(is_nan(negative_infinity+infinity))
        self.assert_(not is_nan(negative_infinity+negative_infinity))

        for x in xrange(100):
            self.assert_(not is_nan(x))
            self.assert_(not is_nan(-x))

class TestIsFinite(unittest.TestCase):
    def runTest(self):
        finites = [infinity, negative_infinity, infinity-negative_infinity]
        for a in finites:
            self.assert_(not is_finite(a))
            for b in finites:
                self.assert_(not is_finite(a + b))
                self.assert_(not is_finite(a - b))
                self.assert_(not is_finite(a * b))
                self.assert_(not is_finite(a / b))
        for x in xrange(100):
            self.assert_(is_finite(x))
            self.assert_(is_finite(-x))

class TestTimeout(unittest.TestCase):
    def runTest(self):
        for exp_t in xrange(1,10):
            t = Timeout(exp_t)
            ticker = tic()
            obs_t = 0
            try:
                try:
                    t.start()
                    while True:
                        time.sleep(10)
                except TimedOut:
                    obs_t = toc(ticker,1)
            finally:
                t.clear()
            self.assertAlmostEquals(obs_t, exp_t, 1)

class TestTiming(unittest.TestCase):
    def runTest(self):
        for exp_t in xrange(10):
            ticker = tic()
            time.sleep(exp_t)
            obs_t = toc(ticker,1)
            self.assertAlmostEquals(obs_t, exp_t, 1)

suite = unittest.makeSuite(TestBinom)
suite.addTest(TestLogSumExp())
suite.addTest(TestChiSqProb())
suite.addTest(TestSubsets())
suite.addTest(TestSubsetn())
suite.addTest(TestPowerset())
suite.addTest(TestPowersetn())
suite.addTest(TestIsNan())
suite.addTest(TestIsFinite())
suite.addTest(TestTimeout())
suite.addTest(TestTiming())
if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
