#!/usr/bin/env python

import sys
import os
import re
import random

for i in range(100000):
  print ("%s.%s.%s.%s" % (random.randint(0, 255), random.randint(0, 255), random.randint(0, 255), random.randint(0, 255)))

sys.exit(0)
