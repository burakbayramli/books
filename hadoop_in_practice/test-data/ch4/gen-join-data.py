#!/usr/bin/env python

import sys
import os
import re
import random

names=['anne','joe','alison','mike','marie','jim','bob','mary','dave','dude']
states=['CA','VA','NY','OR','CO']

# user  age  location

for name in names:
  age = random.randint(20, 80)
  state = states[random.randint(0, len(states)-1)]
  print "%s\t%s\t%s" % (name, age, state)

# user  event   IP

events=['login','logout','new_tweet','view_user']
ips=[]

for i in range(10):
  ips.append("%s.%s.%s.%s" % (random.randint(0, 255), random.randint(0, 255), random.randint(0, 255), random.randint(0, 255)))

for i in range(10000):
  x=int(random.normalvariate(5,1))
  if x >= 0 and x < len(names):
    name = names[x]
    event = events[random.randint(0, len(events)-1)]
    ip = ips[random.randint(0, len(ips)-1)]
    print >> sys.stderr, "%s\t%s\t%s" % (name, event, ip)

sys.exit(0)
