#!/usr/bin/env python
import sys

class MyFunc:
    def __init__(self):
        self.msg = 'Hello, from Python'
    def __call__(self, x):
        print self.msg
        return x + 1

from tmp import f1
f = MyFunc()

# call with function object:
try:
    f1(f)
except:
    print "f2py didn't work with function object: f1(f)"
    type, msg, traceback = sys.exc_info()
    print 'Exception:', type, msg

# call with instance method:
try:
    f1(f.__call__)
except:
    print "f2py didn't work with instance method: f1(f.__call__)"
    type, msg, traceback = sys.exc_info()
    print 'Exception:', type, msg

# call with lambda wrapper:
try:
    f1(lambda x: f(x))
    print "f2py _did_ work with a lambda wrapper"
except:
    print "This shouldn't happen...."
    type, msg, traceback = sys.exc_info()
    print 'Exception:', type, msg


    
