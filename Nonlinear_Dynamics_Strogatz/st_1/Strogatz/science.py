%matplotlib notebook
from sympy import init_session, symbols
import numpy as np
import matplotlib.pyplot as plt
import scipy as sp
import pandas as pd
import theano as T
#from sympy.geometry import *
init_session()
a,b,c = symbols('a b c')
r, theta, phi = symbols('r theta phi', positive=True)
print('Loaded a b c theta and phi')
print('Load Theano as T')