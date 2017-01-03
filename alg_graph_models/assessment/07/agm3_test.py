import unittest

from agm3 import sample_trajectories

class Testagm3(unittest.TestCase):

    def setUp(self):
        self.ins = [((('LEFT','blue'),('UP','green'),('DOWN','green'),('LEFT','red')), 10),
                    ((('LEFT','blue'),('UP','green'),('DOWN','green'),('LEFT','red')), 100),
                    ((('LEFT','green'),('UP','green'),('UP','green'),('LEFT','red')), 1000),
                    ((('LEFT','green'),('UP','green'),('UP','green'),('LEFT','red'),
                      ('LEFT','green'),('UP','green'),('UP','green'),('LEFT','red')), 1000)]

    def test_basic(self):
        for history, sample_size in self.ins:
            dkt = sample_trajectories(history,sample_size)
            z = 0
            for trajectory, prob in dkt.items():
                self.assertEqual(len(trajectory),len(history))
                self.assert_(prob <= 1 and prob > 0)
                z += prob
                for pos in trajectory:
                    x, y = pos
                    self.assert_(type(x),int)
                    self.assert_(type(y),int)
            self.assertAlmostEqual(z,1,places=5)
    
suite = unittest.makeSuite(Testagm3)

if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
