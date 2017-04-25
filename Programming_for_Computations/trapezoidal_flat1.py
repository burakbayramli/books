from math import exp

a = 0.0;  b = 1.0
n = input('n: ')
dt = float(b - a)/n

# Integral by the trapezoidal method
numerical = 0.5*3*(a**2)*exp(a**3) + 0.5*3*(b**2)*exp(b**3)
for i in range(1, n):
    numerical += 3*((a + i*dt)**2)*exp((a + i*dt)**3)
numerical *= dt

exact_value = exp(1**3) - exp(0**3)
error = abs(exact_value - numerical)
rel_error = (error/exact_value)*100
print 'n=%d: %.16f, error: %g' % (n, numerical, error)
