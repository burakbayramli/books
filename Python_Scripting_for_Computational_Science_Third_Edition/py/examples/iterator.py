#!/usr/bin/env python

class MySeq:
    def __init__(self, *data):
        self.data = data

    def __iter__(self):
        return MySeqIterator(self.data)

class MySeqIterator:
    def __init__(self, data):
        self.index = 0
        self.data = data

    def next(self):
        if self.index < len(self.data):
            item = self.data[self.index]
            self.index += 1  # ready for next call
            return item
        else:  # out of bounds
            raise StopIteration

class MySeq2:
    def __init__(self, *data):
        self.data = data

    def __iter__(self):
        self.index = 0
        return self

    def next(self):
        if self.index < len(self.data):
            item = self.data[self.index]
            self.index += 1  # ready for next call
            return item
        else:  # out of bounds
            raise StopIteration

print 'manually coded iteration:'
obj = MySeq(1, 9, 3, 4)
iterator = iter(obj)   # iter(obj) means obj.__iter__()
while True:
    try:
        item = iterator.next()
    except StopIteration:
        break
    # process item:
    print item

print 'the equivalent for loop:'
for item in obj:
    print item,
print


print 'with MySeq2:'
obj = MySeq2(1, 9, 3, 4)
for item in obj:
    print item


# generator version:

def items(obj):
    for item in obj.data:
        yield item

print 'using a generator function items:'
for item in items(obj):
    print item

print 'generator for computing log(x) approximately:'
from math import log
def log_generator(a):
    u_old = 0.0; x = 1.0  # starting values
    while True:
        u_new = u_old + a/x
        x += a
        u_exact = log(x)
        u_old = u_new
        yield x, u_new, u_exact

a = 0.05
for x, log_x, log_x_exact in log_generator(a):
    print 'x=%g, log=%g, error=%e' % (x, log_x, log_x_exact-log_x)
    if x > 1.5:
        break

        
class MySeq3:
    def __init__(self, *data):
        self.data = data

    def __iter__(self):
        for item in obj.data:
            yield item

print 'generator version of __iter__:'
obj = MySeq3(1,2,3,4,6,1)
for item in obj:
    print item


# rewrite generators in terms of ordinary functions with lists:
from math import sin, cos, pi
def circle1(np):
    """Return np points (x,y) equally spaced on the unit circle."""
    da = 2*pi/np
    for i in range(np+1):
        yield (cos(i*da), sin(i*da))

def circle2(np):
    da = 2*pi/np
    return [(cos(i*da), sin(i*da)) for i in range(np+1)]

print '\npoints on a circle:'
for x,y in circle1(4):
    print x,y

for x,y in circle2(4):
    print x,y

