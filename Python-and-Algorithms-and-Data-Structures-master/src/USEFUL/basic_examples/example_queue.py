#!/usr/bin/env python

__author__ = "bt3"

import Queue

q = Queue.Queue()

for i in range(10):
    q.put(i)

for i in range(10):
    print q.get(i)
