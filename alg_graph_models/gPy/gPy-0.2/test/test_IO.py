import unittest

import sys


places = 5

class TestIO(unittest.TestCase):

    def setUp(self):
        from gPy.Graphs import ADG
        asia_adg = ADG()
        asia_adg.add_vertices(['VisitAsia', 'Tuberculosis', 'Smoking', 'Cancer',
                       'TbOrCa', 'XRay', 'Bronchitis', 'Dyspnea'])
        asia_adg.put_arrows((['VisitAsia','Tuberculosis'],
                        ['Tuberculosis','TbOrCa'],
                        ['Cancer','TbOrCa'],
                        ['Smoking','Cancer'],
                        ['Smoking','Bronchitis'],
                        ['Bronchitis','Dyspnea'],
                        ['TbOrCa','Dyspnea'],
                        ['TbOrCa','XRay']))
        self.asia_adg = asia_adg
        self.asia_cpts = {
            'VisitAsia': (
            ['Visit',     'No_Visit'], [],
            [
            ((0.01,        0.99),       [])
            ]),
            'Tuberculosis': (
            ['Present',    'Absent'],     ['VisitAsia'],
            [
            ((0.05,        0.95),         ['Visit']),   
            ((0.01,        0.99),         ['No_Visit'])
            ]),
            'Smoking': (
            ['Smoker',       'NonSmoker'], [],
            [
            ((0.5,         0.5),           [])
            ]),
            'Cancer': (
            ['Present',      'Absent'],    ['Smoking'],
            [
            ((0.1,         0.9),           ['Smoker']),    
            ((0.01,        0.99),          ['NonSmoker'])
            ]),
            'TbOrCa': (
            ['True',        'False'],       ['Tuberculosis', 'Cancer'],
            [
            ((1,           0),              ['Present',      'Present']), 
            ((1,           0),              ['Present',      'Absent']),  
            ((1,           0),              ['Absent',       'Present']), 
            ((0,           1),              ['Absent',       'Absent'])
            ]),
            'XRay': (
            ['Abnormal',     'Normal'],    ['TbOrCa'],
            [
            ((0.98,        0.02),          ['True']),   
            ((0.05,        0.95),          ['False'])
            ]),
            'Bronchitis': (
            ['Present',      'Absent'],    ['Smoking'],
            [
            ((0.6,         0.4),           ['Smoker']),    
            ((0.3,         0.7),           ['NonSmoker'])
            ]),
            'Dyspnea': (
            ['Present',      'Absent'],    ['TbOrCa', 'Bronchitis'],
            [
            ((0.9,         0.1),           ['True',   'Present']),    
            ((0.7,         0.3),           ['True',   'Absent']),     
            ((0.8,         0.2),           ['False',  'Present']),    
            ((0.1,         0.9),           ['False',  'Absent'])
            ])
            }
        
    def testdnet(self):
        from gPy.IO import read_dnet
        from gPy.Models import BN
        from gPy.Variables import Domain
        bnm = BN(domain=Domain())
        bnm.from_dnet(read_dnet('Asia.dnet'))
        self.samegraph(bnm.adg(),self.asia_adg)
        for name, cpt_in_file in self.asia_cpts.items():
            cpt = bnm[name]
            self.samecpt(cpt,cpt_in_file,cpt.child())

    def samecpt(self,cpt,cpt_in_file,child):
        varvalues, parents, valuelines = cpt_in_file  # make tuple
        for values, parentinst in valuelines:
            dkt = dict(zip(parents,parentinst))
            for i, value in enumerate(values):
                dkt[child] = varvalues[i]
                self.assertAlmostEqual(value,cpt[dkt],places)
        
    def samegraph(self,g1,g2):
        for attr in ('_pa','_ch','_ne'):
            d1 = getattr(g1,attr)
            d2 = getattr(g2,attr)
            for x in d1:
                self.assertEqual(d1[x],d2[x]) 


suite = unittest.makeSuite(TestIO)

if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)

