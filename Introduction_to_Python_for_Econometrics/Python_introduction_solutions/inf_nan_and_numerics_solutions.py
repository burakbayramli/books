"""Solutions for 'Inf, NaN and Numerics' chapter.  

Solutions file used IPython demo mode.  To play, run

from IPython.lib.demo import Demo
demo = Demo(inf_nan_and_numerics_solutions.py')
 
and then call 

demo()

to play through the code in steps.
"""
# <demo> auto
from __future__ import print_function
from __future__ import print_function
from numpy import log, exp, finfo
# <demo> --- stop ---
# Exercise 1
# Obviously 1000
x = log(exp(1000))
# <demo> --- stop ---
# Exercise 2
eps = finfo(float).eps
0 == eps/10
x = 1 + eps/10 - 1
x == 0
# <demo> --- stop ---
# Exercise 3
.1 == (.1 + eps/10)
# <demo> --- stop ---
# Exercise 4
x = 1.0 * 10**120
y = 1.0 * 10**120 + 1 * 10**102
print("type(x):")
print(type(x))
print("type(y):")
print(type(y))
x == y
# <demo> --- stop ---
# Exercise 5
x = 10**120
y = 10**120 + 10**102
print("type(x):")
print(type(x))
print("type(y):")
print(type(y))
x==y
# <demo> --- stop ---
# Exercise 6
x = 2.0
count = 0
while x != 1.0:
    count += 1
    x = 1.0 + (x-1.0)/2.0
    print(count,x)

print("count:")
print(count)
print("x:")
print(x)
print("2**(-count+1):")
print(2**(-count+1))
print("eps:")
print(eps)

