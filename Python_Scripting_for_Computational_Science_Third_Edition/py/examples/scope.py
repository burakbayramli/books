#!/usr/bin/env python
import sys

a = 1                 # global variable

def f(x):
    a = 2             # local variable
    print 'locals:', locals(), 'local a:', a
    print 'global a:', globals()['a']
    

class B:
    def __init__(self):
        self.a = 3    # class attribute

    def scopes(self):
        a = 4         # local (method) variable
	print 'locals:', locals()
        print 'vars(self):', vars(self)
        print 'self.a:', self.a
	print 'local a:', a, 'global a:', globals()['a']

class C(B):
    def write(self):
        local_var = -1
        try:
            s = '%(local_var)d %(global_var)d %(a)s' % vars()
        except:
            s = '%d %d %d' % (local_var, global_var, self.a)
            print s
            all = {}
            for dict in locals(), globals(), vars(self):
                all.update(dict)
            s = '%(local_var)d %(global_var)d %(a)s' % all
            print s
            # class attributes:
            self.b = 5
            s = '%(a)d %(b)d' % vars(self)
            print s


# nested blocks of code:

def f1(a=1):
    b = 2      # visible in f1 and f2
    def f2():
        print 'Hello, in f2 inside f1'
        if b:
            b = 3 + a  # binding b makes b local to f2
            a = 0      # binding a makes a local to f2

    f2()
    print b  # 4
    f2()
    print b  # 3

def f3(a=1):
    b = [2]      # visible in f1 and f2
    def f2():
        print 'Hello, in f2 inside f3'
        if b:
            b[0] = 3 + a

    f2()
    print b[0]  # 4


def f4():
    b = 2      # visible in f1 and f2
    def f2():
        print 'Hello, in f2 inside f1'
        b = -9
        if b:
            b = 3 # binding b makes b local to f2
            print 'b in if b block:', b
        print 'b in f2:', b

    f2()
    print 'b in f4', b



if __name__ == '__main__':
    print 'main: globals=',globals()
    print 'main: locals=',locals()
    print 'f: ', f(10)
    c = C()
    c.scopes()
    global_var = 1        # global variable
    c.write()

    try:
        f1()
    except UnboundLocalError:
        print 'Could not assign b in f2; it made b local to f2'
        type, value, traceback = sys.exc_info()
        print type, value
        print 'Error occured at line', traceback.tb_next.tb_next.tb_lineno
    f3()  # print 4
    f4()  # print 2, b was not altered by f2

