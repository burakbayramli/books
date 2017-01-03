#!/usr/bin/env python

import sys
import os
import re
import random

names=['anne','joe','alison','mike','marie','jim','bob','mary','dave','dude']
tweets=["dude where's my car?",'dude! you got a tattoo! ','Hey, look, the Monkees! They were a huge influence on the Beatles.',"I got robbed by a sweet old lady on a motorized cart. I didn't even see it coming.","That's what we're gonna call it. I got worms!"]

for i in range(1000):
  name = names[random.randint(0, len(names)-1)]
  tweet = tweets[random.randint(0, len(tweets)-1)]
  print "%s\t%s" % (name, tweet)

sys.exit(0)
