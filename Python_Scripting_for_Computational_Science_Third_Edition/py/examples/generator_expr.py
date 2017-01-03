#!/usr/bin/env python
"""Demonstration of generator expressions."""
# compute a sequence i**(-0.3)

from math import fabs
import sys, time

N = 10**int(sys.argv[1])  # no of terms in sequene


def smalldiff(term1, term2, eps=1.0E-8):
    return fabs(term1 - term2) < eps

print 'for k in xrange(N):'
term_prev = 0
t0 = time.clock()
for k in xrange(1,N):
    term = k**(-0.3)
    if smalldiff(term, term_prev):
        t1 = time.clock()
        print 'time:', t1-t0, 'final k:', k
        break
    term_prev = term


print 'Generator:'

def g1(n):
    for k in xrange(1,n):
        yield k, k**(-0.3)

term_prev = 0
t0 = time.clock()
for k, term in g1(N):
    if smalldiff(term, term_prev):
        t1 = time.clock()
        print 'time:', t1-t0, 'final k:', k
        break
    term_prev = term


print 'Generation of generator expression:',
t0 = time.clock()

g3 = ((k, k**(-0.3)) for k in xrange(1,N))

t1 = time.clock()
print t1-t0

term_prev = 0
t0 = time.clock()
for k, term in g3:
    if smalldiff(term, term_prev):
        t1 = time.clock()
        print 'time:', t1-t0, 'final k:', k
        break
    term_prev = term

# Alternative:
g3 = ((k, k**(-0.3)) for k in xrange(1,N))
term_prev = 0
t0 = time.clock()
k, term = g3.next()
while not smalldiff(term, term_prev):
    term_prev = term
    try:
        k, term = g3.next()
    except StopIteration:
        print 'No convergence, use a larger N!...'
        sys.exit(1)
t1 = time.clock()
print 'time:', t1-t0, 'final k:', k


print 'Generation of list:',
t0 = time.clock()

g2 = [(k, k**(-0.3)) for k in xrange(1,N)]

t1 = time.clock()
print t1-t0

term_prev = 0
t0 = time.clock()
for k, term in g2:
    if smalldiff(term, term_prev):
        t1 = time.clock()
        print 'time:', t1-t0, 'final k:', k
        break
    term_prev = term

# Alternative:
term_prev = 0
t0 = time.clock()
k, term = g2.pop(0)
while not smalldiff(term, term_prev):
    term_prev = term
    k, term = g2.pop(0)
t1 = time.clock()
print 'time:', t1-t0, 'final k:', k
