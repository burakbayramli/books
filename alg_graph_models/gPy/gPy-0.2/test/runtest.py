import os
import sys
import unittest

tests = []
for name in os.listdir('.'):
    if name.startswith('test_') and name.endswith('.py'):
            tests.append(name[:-3])
tests.sort()
for test in tests:
    module = __import__(test)
    unittest.TextTestRunner(verbosity=2).run(module.suite)

