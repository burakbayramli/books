""" From "COMPUTATIONAL PHYSICS", 3rd Ed, Enlarged Python eTextBook  
    by RH Landau, MJ Paez, and CC Bordeianu
    Copyright Wiley-VCH Verlag GmbH & Co. KGaA, Berlin;  Copyright R Landau,
    Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2015.
    Support by National Science Foundation"""

# TuneNumpy.py: Comparison of NumPy op versus for loop

from datetime import datetime
import numpy as np

def f(x):               # A function requiring some computation
    return x**2-3*x + 4
x = np.arange(1e5)      # An array of 100,000 integers

for j in range(0, 3):  # Repeat comparison three time
    
    t1 = datetime.now()
    y = [f(i) for i in x]        # The for loop
    t2 = datetime.now()
    print (' For for loop,         t2-t1 =', t2-t1)  

    t1 = datetime.now()
    y = f(x)                    # Vectorized evaluation
    t2 = datetime.now()
    print (' For vector function,  t2-t1 =',  t2-t1) 
