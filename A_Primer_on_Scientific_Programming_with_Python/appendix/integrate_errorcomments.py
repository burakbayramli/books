"""
Midpoint rule for integration.
"""

def integrate(f, a, b, n):
    s = 0
    for i in range(1, n): # wrong loop limit
        s += f(a + i*h)   # wrong argument formula, NameError h
    return s

def f(x):
return asin(x)  # IndentationError, ValueError

def g(x):
return 1

# Test/application part
n = sys.argv[1]   # NameError, IndexError
I = integrate(g, 0, 10,  n)      # pi: NameError, n: TypeError
print "Integral of g equals %g" % I 
I = integrate(f, 0, pi,  n)      
I_exact = pi*asin(pi) - sqrt(1 - pi**2) - 1
print "Integral of f equals %g (exact value is %g)' % \
  (I, I_exact)   # SyntaxError

# Extension: read f, a, b, n from the command line,
# if f is a number (eval(f) yields int or float), we can verify the program

