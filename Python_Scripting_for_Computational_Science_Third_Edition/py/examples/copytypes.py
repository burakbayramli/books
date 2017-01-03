#!/usr/bin/env python
import copy

# number objects:
a = 3
b = a    # b becomes a copy of a
a = 4
print b  # b is still 3
print 'a is b:', a is b

# strings:
a = '3'
b = a    # b becomes a copy of a
a = '4'
print b  # b is still '3'

# user-defined objects:
class A:
    def __init__(self, value):
        self.x = value
    def __repr__(self):
        return 'x=%s' % self.x

a = A(-99)
b_assign  = a
b_shallow = copy.copy(a)
b_deep    = copy.deepcopy(a)
a.x = 9  # alter attribute in a
print 'a.x=%s, b_assign.x=%s, b_shallow.x=%s, b_deep.x=%s' %\
      (a.x, b_assign.x, b_shallow.x, b_deep.x)

a = A([-2,3])
b_assign  = a
b_shallow = copy.copy(a)
b_deep    = copy.deepcopy(a)
a.x[0] = 8
print 'a.x=%s, b_assign.x=%s, b_shallow.x=%s, b_deep.x=%s' %\
      (a.x, b_assign.x, b_shallow.x, b_deep.x)

# nested lists:
a = [4,3,5,['some string',2], A(-9)]
b_assign  = a
b_shallow = copy.copy(a)
b_deep    = copy.deepcopy(a)
b_slice   = a[0:5]
a[3] = 999; a[4].x = -6
print 'b_assign=%s\nb_shallow=%s\nb_deep=%s\nb_slice=%s' % \
      (b_assign, b_shallow, b_deep, b_slice)

# dictionaries behave similarly:
a = {'key1' : -99, 'key2' : ('str', 8, A(-9))}
b_assign  = a
b_shallow = copy.copy(a)
b_deep    = copy.deepcopy(a)
a['key1'] = 2;  a['key2'][2].x = 3
print '\nb_assign=%s\nb_shallow=%s\nb_deep=%s' % \
      (b_assign, b_shallow, b_deep)


# (tuple example does not make sense here; you cannot
# change the items in a tuple)
