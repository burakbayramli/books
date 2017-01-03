"""Solutions for 'Standard Data Types' chapter.  

Solutions file used IPython demo mode.  To play, run

from IPython.lib.demo import Demo
demo = Demo('standard_datatypes_solutions.py')
 
and then call 

demo()

to play through the code in steps.
"""
# <demo> auto
from __future__ import print_function
from __future__ import print_function
import copy
# <demo> --- stop ---
# Exercise 1
a = 4
b = 3.1415
c = 1.0
d = 2+4j
e = 'Hello'
f = 'World'
# <demo> --- stop ---
# Exercise 2
print("a:")
print(a)
print("type(a):")
print(type(a))
print("b:")
print(b)
print("type(b):")
print(type(b))
print("c:")
print(c)
print("type(c):")
print(type(c))
print("d:")
print(d)
print("type(d):")
print(type(d))
print("e:")
print(e)
print("type(e):")
print(type(e))
print("f:")
print(f)
print("type(f):")
print(type(f))
# <demo> --- stop ---
# Exercise 3
# Added
print("a + b:")
print(a + b)
print("a + c:")
print(a + c)
print("a + d:")
print(a + d)
print("b + c:")
print(b + c)
print("b + d:")
print(b + d)
print("c + d:")
print(c + d)
print("e + f:")
print(e + f)
# <demo> --- stop ---
# Subtracted - No e - f
print("a - b:")
print(a - b)
print("a - c:")
print(a - c)
print("a - d:")
print(a - d)
print("b - c:")
print(b - c)
print("b - d:")
print(b - d)
print("c - d:")
print(c - d)
# <demo> --- stop ---
# Multiplied - Note e *a, f * a
print("a * b:")
print(a * b)
print("a * c:")
print(a * c)
print("a * d:")
print(a * d)
print("b * c:")
print(b * c)
print("b * d:")
print(b * d)
print("c * d:")
print(c * d)
print("e * a:")
print(e * a)
print("f * a:")
print(f * a)
# <demo> --- stop ---
# Divided
print("a / b:")
print(a / b)
print("a / c:")
print(a / c)
print("a / d:")
print(a / d)
print("b / c:")
print(b / c)
print("b / d:")
print(b / d)
print("c / d:")
print(c / d)
# <demo> --- stop ---
# Exercise 4
print("type(a + b):")
print(type(a + b))
print("type(a + c):")
print(type(a + c))
print("type(a + d):")
print(type(a + d))
print("type(b + c):")
print(type(b + c))
print("type(b + d):")
print(type(b + d))
print("type(c + d):")
print(type(c + d))
print("type(e + f):")
print(type(e + f))
# <demo> --- stop ---
# Subtracted - No e - f
print("type(a - b):")
print(type(a - b))
print("type(a - c):")
print(type(a - c))
print("type(a - d):")
print(type(a - d))
print("type(b - c):")
print(type(b - c))
print("type(b - d):")
print(type(b - d))
print("type(c - d):")
print(type(c - d))
# <demo> --- stop ---
# Multiplied - Note e *a, f * a
print("type(a * b):")
print(type(a * b))
print("type(a * c):")
print(type(a * c))
print("type(a * d):")
print(type(a * d))
print("type(b * c):")
print(type(b * c))
print("type(b * d):")
print(type(b * d))
print("type(c * d):")
print(type(c * d))
print("type(e * a):")
print(type(e * a))
print("type(f * a):")
print(type(f * a))
# <demo> --- stop ---
# Divided
print("type(a / b):")
print(type(a / b))
print("type(a / c):")
print(type(a / c))
print("type(a / d):")
print(type(a / d))
print("type(b / c):")
print(type(b / c))
print("type(b / d):")
print(type(b / d))
print("type(c / d):")
print(type(c / d))
# <demo> --- stop ---
# Exercise 5
ex = 'Python is an interesting and useful language for numerical computing!'
print("ex[:6]:")
print(ex[:6])
print("ex[0:6]:")
print(ex[0:6])
print("ex[:-63]:")
print(ex[:-63])
print("ex[-69:-63]:")
print(ex[-69:-63])

print("ex[-1:]:")
print(ex[-1:])
print("ex[68]:")
print(ex[68])
print("ex[68:]:")
print(ex[68:])

print("ex[-10:-1]:")
print(ex[-10:-1])
print("ex[59:68]:")
print(ex[59:68])

print("ex[21:23]:")
print(ex[21:23])
print("ex[-48:-46]:")
print(ex[-48:-46])

print("ex[::-1]:")
print(ex[::-1])
print("ex[68::-1]:")
print(ex[68::-1])

print("ex[5::-1]:")
print(ex[5::-1])
# <demo> --- stop ---
# Exercise 6
print("(a,):")
print((a,))
print("tuple([a]):")
print(tuple([a]))

print("[a]:")
print([a])
print("list((a,)):")
print(list((a,)))
# <demo> --- stop ---
# Exercise 7
x = [[1,0.5],[0.5,1]]
# <demo> --- stop ---
# Exercise 8
y = x
print("y:")
print(y)
print("x:")
print(x)
y[0][0] = 1.61
print("y:")
print(y)
print("x:")
print(x)
# <demo> --- stop ---
# Exercise 9
z = x[:]
y[0][0] = 1j
print("x:")
print(x)
print("y:")
print(y)
print("z:")
print(z)
# <demo> --- stop ---
# Exercise 10
w = copy.deepcopy(x)
w[0][0] = -1
print("w:")
print(w)
print("x:")
print(x)
# <demo> --- stop ---
# Exercise 11
x = [4, 3.1415, 1.0, 2+4j, 'Hello', 'World']
# Copy since single level and immutable data
y = x[:] 
print("y:")
print(y)
print("y.pop(2):")
print(y.pop(2))
print("y:")
print(y)
y = x[:]
del y[2] #NOPRINT
print("y:")
print(y)
y = x[:]
print("y.remove(1.0):")
print(y.remove(1.0))
print("y:")
print(y)
print("x.extend([1.0, 2+4j, 'Hello']):")
print(x.extend([1.0, 2+4j, 'Hello']))
# Makes a copy
print("x[::-1]:")
print(x[::-1])
# Implace
print("x.reverse():")
print(x.reverse())
print("x.count('Hello'):")
print(x.count('Hello'))
# <demo> --- stop ---
# Exercise 12
x = {'alpha': 1.0, 'beta': 3.1415, 'gamma': -99}
print("x:")
print(x)
print("x['alpha']:")
print(x['alpha'])
# <demo> --- stop ---
x = [4, 3.1415, 1.0, 2+4j, 'Hello', 'World']
print("x.extend([1.0, 2+4j, 'Hello']):")
print(x.extend([1.0, 2+4j, 'Hello']))
# Set removes dumplicates
x = set(x)
# <demo> --- stop ---