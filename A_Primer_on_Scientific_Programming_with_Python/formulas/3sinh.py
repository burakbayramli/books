from math import sinh, exp, e, pi
x = 2*pi
r1 = sinh(x)
r2 = 0.5*(exp(x) - exp(-x))
r3 = 0.5*(e**x - e**(-x))
print r1, r2, r3
print repr(r1), repr(r2), repr(r3)      # complete representation
print '%.16f %.16f %.16f' % (r1,r2,r3)  # complete representation

diff1 = r1 - r2
diff2 = r2 - r3
print diff1, diff2
