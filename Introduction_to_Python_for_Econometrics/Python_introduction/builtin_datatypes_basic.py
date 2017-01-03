from __future__ import division
from __future__ import print_function
import copy as cp
import sys
from pylab import *
from numpy import *
# End Imports


x = 1.0
X = 1.0
X1 = 1.0
X1 = 1.0
x1 = 1.0
dell = 1.0
dellreturns = 1.0
dellReturns = 1.0
_x = 1.0
x_ = 1.0

x, y, z = 1, 3.1415, 'a'

x = 1
type(x)
x = 1.0
type(x)
x = float(1)
type(x)

x = 1.0
type(x)
x = 1j
type(x)
x = 2 + 3j
x
x = complex(1)
x

x = 1
type(x)
x = 1.0
type(x)
x = int(x)
type(x)

x = 1
x
type(x)
x = 1L
x
type(x)
x = long(2)
type(x)
y = 2
type(y)
x = y ** 64  # ** is denotes exponentiation, y^64 in TeX
x

x = True
type(x)
x = bool(1)
x
x = bool(0)
x

x = 'abc'
type(x)
y = '"A quotation!"'
print(y)

text = 'Python strings are sliceable.'
text[0]
text[10]
L = len(text)
try:
    text[L] # Error
except:
    print("Error detected in: text[L] # Error")
    error = sys.exc_info()
    print("Error type: " + str(error[0]) + " Error message: " + str(error[1]))
text[L-1]
text[:10]
text[10:]

x = []
type(x)
x=[1,2,3,4]
x
x = [[1,2,3,4], [5,6,7,8]]
x
x = [[1,2,3,4] , [5,6,7]]
x
x = [1,1.0,1+0j,'one',None,True]
x

x = [0,1,2,3,4,5,6,7,8,9]
x[0]
x[5]
try:
    x[10] # Error
except:
    print("Error detected in: x[10] # Error")
    error = sys.exc_info()
    print("Error type: " + str(error[0]) + " Error message: " + str(error[1]))
x[4:]
x[:4]
x[1:4]
x[-0]
x[-1]
x[-10:-1]

x = [[1,2,3,4], [5,6,7,8]]
x[0]
x[1]
x[0][0]
x[0][1:4]
x[1][-4:-1]

x = [0,1,2,3,4,5,6,7,8,9]
x.append(0)
x
len(x)
x.extend([11,12,13])
x
x.pop(1)
x
x.remove(0)
x

x = [0,1,2,3,4,5,6,7,8,9]
del x[0]
x
x[:3]
del x[:3]
x
del x[1:3]
x
del x[:]
x

x =(0,1,2,3,4,5,6,7,8,9)
type(x)
x[0]
x[-10:-5]
x = list(x)
type(x)
x = tuple(x)
type(x)
x= ([1,2],[3,4])
x[0][1] = -10
x # Contents can change, elements cannot

x =(2)
type(x)
x = (2,)
type(x)
x = tuple([2])
type(x)

x = xrange(10)
type(x)
print(x)
list(x)
x = xrange(3,10)
list(x)
x = xrange(3,10,3)
list(x)
y = range(10)
type(y)
y

data = {'age': 34, 'children' : [1,2], 1: 'apple'}
type(data)
data['age']

data['age'] = 'xyz'
data['age']

data['name'] = 'abc'
data

del data['age']
data

x = set(['MSFT','GOOG','AAPL','HPQ','MSFT'])
x
x.add('CSCO')
x
y = set(['XOM', 'GOOG'])
x.intersection(y)
x = x.union(y)
x
x.remove('XOM')

x = 1
y = x
id(x)
id(y)
x = 2.0
id(x)
id(y)

x = [1, 2, 3]
y = x
y[0] = -10
y
x

x = [1, 2, 3]
y = x[:]
id(x)
id(y)

x=[[0,1],[2,3]]
y = x[:]
y
id(x[0])
id(y[0])
x[0][0]
id(x[0][0])
id(y[0][0])
y[0][0] = -10.0
y
x

x=[[0,1],[2,3]]
y = cp.deepcopy(x)
y[0][0] = -10.0
y
x

