import unittest

import sys
from gPy.Data import *
from gPy.IO import read_dnet, read_csv    # read_dnet test in test_IO.py
from gPy.Models import BN
places = 5

class TestParameters(unittest.TestCase):

    def setUp(self):
        self.rawdata = read_csv(open('alarm_1K.dat'))

    def test_bigdata(self):
        alarm = CompactFactor(self.rawdata)
        for varset in (
            ['CO','MINVOLSET'],
            ['CO'],
            ['HRBP','EXPCO2','PRESS','VENTTUBE'],
            ['DISCONNECT','MINVOLSET','VENTMACH','VENTTUBE','VENTLUNG','VENTALV','ARTCO2','PRESS','SHUNT']
            ):
            factor = alarm[varset]
            self.assertEqual(factor.z(),1000)
            tmp = factor.copy()
            tmp.zero()
            tmp.inc_from_rawdata(self.rawdata)
            self.samefactor(factor,tmp)
            self.assertEqual(alarm[[]].z(),1000)

    def test_bigdata_sql(self):
        alarm = Data(self.rawdata)
        for varset in (
            ['CO','MINVOLSET'],
            ['CO'],
            ['HRBP','EXPCO2','PRESS','VENTTUBE'],
            ['DISCONNECT','MINVOLSET','VENTMACH','VENTTUBE','VENTLUNG','VENTALV','ARTCO2','PRESS','SHUNT']
            ):
            factor = alarm[varset]
            self.assertEqual(factor.z(),1000)

    def test_smalldata(self):
        rawdata = read_csv(open('fake_data'))
        dat = CompactFactor(rawdata)
        self.assertEqual(dat.makeFactor([]).z(),5)
        self.samefactor(dat.makeFactor(['bar']),Factor(['bar'],[2,2,1]))
        self.samefactor(dat.makeFactor(['blah']),Factor(['blah'],[3,1,1]))
        self.samefactor(dat.makeFactor(['foo']),Factor(['foo'],[3,1,1,0]))
        self.samefactor(dat.makeFactor(['bar','foo']),
                        Factor(['bar','foo'],[2,0,0,0,1,1,0,0,0,0,1,0]))

    def test_smalldata_sql(self):
        rawdata = read_csv(open('fake_data'))
        dat = Data(rawdata)
        self.assertEqual(dat.makeFactor([]).z(),5)
        self.samefactor(dat.makeFactor(['bar']),Factor(['bar'],[2,2,1]))
        self.samefactor(dat.makeFactor(['blah']),Factor(['blah'],[3,1,1]))
        self.samefactor(dat.makeFactor(['foo']),Factor(['foo'],[3,1,1,0]))
        self.samefactor(dat.makeFactor(['bar','foo']),
                        Factor(['bar','foo'],[2,0,0,0,1,1,0,0,0,0,1,0]))


    def samefactor(self,tf1,tf2):
        self.assertEqual(tf1.variables(),tf2.variables())
        tf2data = tf2.data()
        for i, val in enumerate(tf1.data()):
            self.assertAlmostEqual(val,tf2data[i],places)

suite = unittest.makeSuite(TestParameters)

if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
