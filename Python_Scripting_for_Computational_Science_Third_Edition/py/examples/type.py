#!/usr/bin/env python
import os, shutil, glob

def remove(files):
    """
    Remove one or more files and/or directories.
    """
    if isinstance(files, str): # is files a string?
        files = [files]  # convert files from a string to a list
    if not isinstance(files, list):  # is files not a list?
        raise TypeError, \
              'remove() argument 1 must be a string '\
              'or a list, not a %s' % type(files)
    for file in files:
        print 'removing',
        if os.path.isdir(file):
            shutil.rmtree(file)
            print 'directory',
        elif os.path.isfile(file):
            print 'file',
            os.remove(file)
        print file

# test the remove function:
for i in range(10):
    os.mkdir('tmp_'+str(i))
    f = open('tmp__'+str(i), 'w'); f.close()
remove('tmp_1')
remove(glob.glob('tmp_[0-9]') + glob.glob('tmp__[0-9]'))

# -----------------------------------------------------------------
class MyClass:
    def __init__(self, s):
        self.s = s
#    def __str__(self):
#        return "MyClass instance '%s'" % self.s
        
from types import *

myinst = MyClass('some input')  # instance of user-defined class
somelist = ['text', 1.28736, ['sub', 'list'],
            {'sub' : 'dictionary', 'heterogeneous' : 1},
            ('some', 'sub', 'tuple'), 888, myinst]

types_types = (IntType, ListType, TupleType, DictType, StringType,
               UnicodeType, FloatType, InstanceType)

def typecheck_types(i):
    for t in types_types:
        if type(i) == t:
            print t,

class_types = ((int, long), list, tuple, dict, str, basestring,
               float, MyClass)

def typecheck(i):
    for c in class_types:
        if isinstance(i, c):
            print c,

type_types = (type(1), type([]), type(()), type({}), type(''),
              type(u''), type(1.0), type(MyClass('')))

def typecheck_type(i):
    for t in type_types:
        if type(i) == t:
            print t,

for func in typecheck, typecheck_types, typecheck_type:
    for i in somelist:
        print i, 'is',
        func(i)
        print
        
print 'The next call should raise an exception:'
try:
    remove(os)
except TypeError, msg:
    print 'Yes, got an exception:\n', msg
    



