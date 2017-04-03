import time

then = None
def k(m):
  global then
  now = time.time()
  if then: print m, now - then
  else: print m
  then = now
