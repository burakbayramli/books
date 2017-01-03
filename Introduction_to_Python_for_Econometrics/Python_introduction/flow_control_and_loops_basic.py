from __future__ import division
from __future__ import print_function
import sys
from pylab import *
from numpy import *
# End Imports


x = 5
if x<5:
    x += 1
else:
    x -= 1
x

x = 5;
if x<5:
   x = x + 1
elif x>5:
   x = x - 1
else:
   x = x * 2
x

count = 0
for i in xrange(100):
    count += i
count = 0
x = linspace(0,500,50)
for i in x:
    count += i
count = 0
x = list(arange(-20,21))
for i in x:
    count += i

count = 0
for i in xrange(10):
    for j in xrange(10):
        count += j

returns = randn(100)
count = 0
for ret in returns:
    if ret<0:
        count += 1

returns = randn(100)
count = 0
for i in xrange(len(returns)):
    if returns[i]<0:
        count += 1

x = zeros((10,10))
for i in xrange(size(x,0)):
    for j in xrange(size(x,1)):
        if i<j:
            x[i,j]=i+j;
        else:
            x[i,j]=i-j

x = zeros((10,10))
for i in xrange(size(x,0)):
    if (i % 2) == 1:
        for j in xrange(size(x,1)):
            x[i,j] = i+j
    else:
        for j in xrange(int(i/2)):
            x[i,j] = i-j

x = range(10)
for i in x:
    print(i)
    print('Length of x:', len(x))
    x = range(5)

L = [1, 2]
for i in L:
     print(i)
     L.append(i+2)
     if i>5:
         break

x = linspace(0,100,11)
for i,y in enumerate(x):
    print('i is :', i)
    print('y is :', y)

x = randn(1000)
for i in x:
    print(i)
    if i > 2:
        break

x = randn(10)
for i in x:
    if i < 0:
        print(i)
for i in x:
    if i >= 0:
        continue
    print(i)

count = 0
i = 1
while i<10:
    count += i
    i += 1

count=0;
for i in xrange(0,10):
    count += i

# randn generates a standard normal random number
mu = abs(100*randn(1))
index = 1
while abs(mu) > .0001:
    mu = (mu+randn(1))/index
    index=index+1

condition = True
i = 0
x = randn(1000000)
while condition:
    if x[i] > 3.0:
        break # No printing if x[i] > 3 or reached end of array
    print(x[i])
    i += 1

i = 0
while x[i] <= 3:
    print(x[i])
    i += 1

text = ('a','1','54.1','43.a')
for t in text:
    try:
        temp = float(t)
        print(temp)
    except ValueError:
        print('Not convertable to a float')

x = arange(5.0)
y = []
for i in xrange(len(x)):
   y.append(exp(x[i]))
y
z = [exp(x[i]) for i in xrange(len(x))]
z

x = arange(5.0)
y = []
for i in xrange(len(x)):
   if floor(i/2)==i/2:
       y.append(x[i]**2)
y
z = [x[i]**2 for i in xrange(len(x)) if floor(i/2)==i/2]
z

x1 = arange(5.0)
x2 = arange(3.0)
y = []
for i in xrange(len(x1)):
    for j in xrange(len(x2)):
        y.append(x1[i]*x2[j])
y
z = [x1[i]*x2[j] for i in xrange(len(x1)) for j in xrange(len(x2))]
# Only when i==j
z = [x1[i]*x2[j] for i in xrange(len(x1)) for j in xrange(len(x2)) if i==j]

x = arange(-5.0,5.0)
z_set = {x[i]**2.0 for i in xrange(len(x))}
z_set
z_dict = {i:exp(i) for i in x}
z_tuple = tuple(i**3 for i in x)

