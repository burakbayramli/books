import os
import sys
import unittest

exclude = frozenset([
    'test_ADTree.py',
    'test_CBN.py',
    'test_Factor.py',
    'test_Fun.py',
    'test_Chi2Separator.py',
    'test_GraphCI.py',
    'test_ICPattern.py',
    'test_InterventionalIC.py',
    'test_Metrics.py',
    'test_CausalWorld.py',
    'test_Density.py'])

tests = []
for name in os.listdir('.'):
    if name.startswith('test_') and name.endswith('.py'):
        if name not in exclude:
            tests.append(name[:-3])
tests.sort()
for test in tests:
    module = __import__(test)
    unittest.TextTestRunner(verbosity=2).run(module.suite)

