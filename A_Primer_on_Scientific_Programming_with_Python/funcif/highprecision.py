import decimal                  # floats with arbitrarily many digits
decimal.getcontext().prec = 25  # use 25 digits
D = decimal.Decimal             # short form for new float type

def diff2(f, x, h=1E-9):
    x = D(str(x));  h = D(str(h))  # convert to high precision
    r = (f(x-h) - 2*f(x) + f(x+h))/(h*h)
    return r

def g(t):
    return t**(-6)

for k in range(1,15):
    h = 10**(-k)
    print 'h=%.0e: %.5f' % (h, diff2(g, 1, h))
