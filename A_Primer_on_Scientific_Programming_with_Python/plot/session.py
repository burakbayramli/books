import time

n = 5                 # no of points along the x axis
def f(x):
    return x**3       # sample function

dx = 1./(n-1)         # spacing between x points
pairs = []            # list of all [x,y] pairs
xlist = []            # list of all x coordinates
ylist = []            # list of all y coordinates
for i in range(n):
    x = i*dx;  y = f(x)
    pairs.append([x, y])
    xlist.append(x)
    ylist.append(y)

pairs
xlist
ylist

# Alternative, more compact programming with list comprehension
xlist = [i*dx for i in range(n)]
ylist = [f(x) for x in xlist]
pairs = [[x, y] for x, y in zip(xlist, ylist)]

import numpy as np
a = np.zeros(n)           # array of n zeros
x2 = np.array(xlist)      # turn list x into array x2
y2 = np.array(ylist)
x2
y2

# Alternatively, compute arrays directly
n = len(x2)
x2 = np.linspace(0, 1, n)   # n numbers from 0 to 1
y2 = np.zeros(n)
for i in range(n):
    y2[i] = f(x2[i])

# Direct calculation on complete arrays (all elements)
y2 = f(x2)
y2

Cdegrees = [-30 + i*10 for i in range(3)]
Fdegrees = [9./5*C + 32 for C in Cdegrees]
table = [[C, F] for C, F in zip(Cdegrees, Fdegrees)]
print table
table2 = np.array(table)    # turn list into array
print table2
table2
table2.shape
table[1][0]     # table[1] is [-20,4], whose index 0 holds -20
table2[1,0]     # common indexing
table2[1][0]    # this works too

for i in range(table2.shape[0]):
    for j in range(table2.shape[1]):
        print 'table2[%d,%d] = %g' % (i, j, table2[i,j])

table2[0:table2.shape[0], 1]  # 2nd column (index 1)
table2[0:, 1]                 # same
table2[:, 1]                  # same


# sin(x):
points = 9, 17, 65   # tuple with points
for n in points:
    x = np.linspace(0, 4*np.pi, n)
    y = np.sin(x)

# Heaviside function
def H(x):
    if x < 0:
        return 0
    else:
        return 1

# Demonstrate that this function is not vectorized
x = np.linspace(-4, 4, 4)
try:
    y = H(x)
except Exception, e:
    print e

# We must either use an explicit loop over x[i] points
# or we may turn H into a vectorized function using numpy.vectorize
Hv = np.vectorize(H)

import matplotlib.pyplot as plt
points = 3, 9, 101, 501
for n in points:
    x = np.linspace(-4, 4, n)
    y = Hv(x)
    plt.plot(x, y)
    plt.plot([0,0], [0,1], 'r--')
    plt.title('Heaviside function with %d points' % n)
    plt.axis([x[0], x[-1], -0.1, 1.1])
    plt.savefig('tmp_H%d.eps' % n)
    plt.show()

x = [-4, 0, 0, 4]
y = [0, 0, 1, 1]
plt.plot(x, y)
plt.axis([x[0], x[-1], -0.1, 1.1])
plt.savefig('tmp_Hsmart.eps')
#import pprint
#pprint.pprint(lines[0].get())

import numpy as np
x = np.array([1, 2, 3.5])
a = x
a[-1] = 3  # this changes x[-1] too!
x
a = x.copy()
a[-1] = 9
a
x
a = (3*x**4 + 2*x + 4)/(x + 1)
a = np.linspace(1, 8, 8)
a
a[[1,6,7]] = 10
a
a[range(2,8,3)] = -2   # same as a[2:8:3] = -2
a
a[a < 0]            # pick out the negative elements of a
a[a < 0] = a.max()
a
# Replace elements where a is 10 by the first
# elements from another array/list
a[a == 10] = [10, 20, 30, 40, 50, 60, 70]
a
a = np.linspace(-1, 1, 3)
a
type(a)
isinstance(a, np.ndarray)
type(a) is np.ndarray
isinstance(a, (float,int))  # float or int?
a = np.r_[-5:5:11j]  # same as linspace(-5, 5, 11)
print a
a = np.r_[-5:5:1.0]
print a
a = np.linspace(-1, 1, 6)
a.shape
a.size
a.shape = (2, 3)
a.shape
a.size                 # total no of elements
a.shape = (a.size,)    # reset shape
a = a.reshape(3, 2)    # alternative
len(a)                 # no of rows

def f(x):
    if isinstance(x, (float, int)):
        return 2
    elif isinstance(x, np.ndarray):  # or elif type(x) is ndarray:
        return np.zeros(x.shape, x.dtype) + 2
    else:
        raise TypeError\
        ('x must be int, float or ndarray, not %s' % type(x))


x1 = np.linspace(0, 1, 2)
x2 = 3
f(x1)
f(x2)

t = np.linspace(1, 30, 30).reshape(5, 6)
t
t[1:-1:2, 2:]
t[:-2, :-1:2]
t[np.ix_([0,3], [1,2])]
t[np.ix_([0,3], [1,2])] = 0
t


