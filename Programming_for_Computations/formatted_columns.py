from math import sin

n = 6
t0 = 2
dt = 0.55

# Unformatted print
for i in range(n):
    t = t0 + i*dt
    g = t*sin(t)
    print t, g

print 'Formatting via printf syntax'
for i in range(n):
    t = t0 + i*dt
    g = t*sin(t)
    print '%6.2f  %8.3f' % (t, g)


