import unittest

from agm2 import find_minimal_separator
from gPy.Examples import asia
asia_adg = asia.adg()

cases = (
    ('VisitAsia','Tuberculosis',None),
    ('VisitAsia','Smoking',frozenset()),
    ('Bronchitis','Cancer',frozenset(['Smoking']))
    )

cases2 = (
    ('Smoking','Dyspnea',2),
    ('VisitAsia','XRay',1),
    ('VisitAsia','Dyspnea',1),
    )


class Testagm2(unittest.TestCase):

    def test_asia(self):
        for case in cases:
            self.assertEqual(
                find_minimal_separator(asia_adg,case[0],case[1]),
                case[2])

    def test_asia2(self):
        for case in cases2:
            sep = find_minimal_separator(asia_adg,case[0],case[1])
            self.assertEqual(len(sep),case[2])


suite = unittest.makeSuite(Testagm2)

if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
