class A(object):
    _counter = 0
    def __init__(self):
        A._counter += 1
        self.__counter = A._counter

    @staticmethod
    def _set_counter(value):
        print 'In _set_counter'
        A.__counter = value
    
    p = property(fget=lambda o: o.__counter)
    q = property(fget=lambda o: o.__counter,
                 fset=lambda o, v: o._set_counter(v))

for i in range(10):
    a = A()
print a._counter
print 'A.p is', A.p, type(A.p)
print 'a.p:', a.p, type(a.p)
print 'a.q:', a.q, type(a.q)
print 'A.q is', A.q, type(A.q)
A.q = 101
a.q = -9
print 'Ooops (int!): A.q:', A.q, type(A.q)
print 'a.q:', a.q, type(a.q)
try:
    a.p = 12
except AttributeError:
    print 'a.p = 10 is illegal :-)'
print 'a.p:', a.p, type(a.p)
try:
    A.p = 99  # rebind A.p from property to int :-/
    print 'Ooops! A.p = 99 works :-/'
except:
    print 'A.p = 99 is illegal :-)'
print 'A.p:', A.p, type(A.p)

# conclusion:
"""
Properties related to static variables work with instances, but
when invoked as A.p, one can do anything with assignment (which
then throws the property away and binds the stativ variable to
something else :-/
"""

