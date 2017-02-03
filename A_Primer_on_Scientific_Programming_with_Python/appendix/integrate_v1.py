def integrate(f, a, b, n):
    s = 0
    for i in range(1, n): 
        s += f(a + i*h)
    return s

def f(x):
return asin(x)

def g(x):
return 1

# Test/application part
n = sys.argv[1]
I = integrate(g, 0, 10,  n) 
print "Integral of g equals %g" % I 
I = integrate(f, 0, pi,  n)      
I_exact = pi*asin(pi) - sqrt(1 - pi**2) - 1
print "Integral of f equals %g (exact value is %g)' % \
  (I, I_exact)
