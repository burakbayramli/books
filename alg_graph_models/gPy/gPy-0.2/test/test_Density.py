from utils_test import generate_dense_bn
import unittest

class TestDensity(unittest.TestCase):
    def runTest(self):
        for density in xrange(8):
            for run in xrange(30):
                model = generate_dense_bn(density)
                for v in model.variables():
                    self.assertEquals(density, len(model.adg().parents(v)|model.adg().children(v)))

suite = unittest.makeSuite(TestDensity)
if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
