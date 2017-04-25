from math import exp

v = lambda t: 3*(t**2)*exp(t**3)  # Function to be integrated
a = 0.0;  b = 1.0
n = input('n: ')
dt = float(b - a)/n

# Integral by the trapezoidal method
numerical = 0.5*v(a) + 0.5*v(b)
for i in range(1, n):
    numerical += v(a + i*dt)
numerical *= dt

F = lambda t: exp(t**3)
exact_value = F(b) - F(a)
error = abs(exact_value - numerical)
rel_error = (error/exact_value)*100
print 'n=%d: %.16f, error: %g' % (n, numerical, error)
