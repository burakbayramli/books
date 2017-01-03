# test_mean.py
from mean import *

def test_ints():
  num_list=[1,2,3,4,5]
  obs = mean(num_list)
  exp = 3
  assert obs == exp

def test_zero():
  num_list=[0,2,4,6]
  obs = mean(num_list)
  exp = 3
  assert obs == exp

def test_double():
  num_list=[1,2,3,4]
  obs = mean(num_list)
  exp = 2.5
  assert obs == exp

def test_long():
  big = 100000000
  obs = mean(range(1,big))
  exp = big/2.0
  assert obs == exp
