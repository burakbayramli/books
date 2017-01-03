#!/usr/bin/env python

__author__ = "bt3"


foo = 'foo'
bar = 'bar'

print '%s%s' % (foo, bar) # It is OK
print '{0}{1}'.format(foo, bar) # It is better
print '{foo}{bar}'.format(foo=foo, bar=bar) # It is best