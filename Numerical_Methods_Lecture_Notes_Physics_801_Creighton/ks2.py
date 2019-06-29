import pylab, random, math

N1 = input('number of data points in set 1 -> ')
N2 = input('number of data points in set 2 -> ')
med1 = input('median of set 1 distribution -> ')
med2 = input('median of set 2 distribution -> ')
gam1 = input('scale parameter of set 1 distribution -> ')
gam2 = input('scale parameter of set 2 distribution -> ')
seed = input('random number seed -> ')
random.seed(seed)

# generate data
x1 = [med1+gam1*math.tan(math.pi*(random.random()-0.5)) for i in range(N1)]
x2 = [med2+gam2*math.tan(math.pi*(random.random()-0.5)) for i in range(N2)]

# compute K-S test statistic
x1.sort()
x2.sort()
x1 += [float('inf')]  # add a final point at infinity
x2 += [float('inf')]  # add a final point at infinity
F1 = F2 = 0.0
D = 0.0  # biggest difference
i = j = 0
while i <= N1 and j <= N2:
    F1 = i/float(N1)
    F2 = j/float(N2)
    d = abs(F1-F2)
    if x1[i] < x2[j]:
        x = x1[i]
        i += 1
    elif x1[i] > x2[j]:
        x = x2[j]
        j += 1
    else:
        x = x1[i]
        i += 1
        j += 1
    if d > D:
        (D, X) = (d, x)
        segment = [F1, F2]

# compute p-value for this statistic
N = N1*N2/float(N1+N2)  # effective number of data points
K = D*(N**0.5+0.12+0.11/N**0.5)
a = 2.0
b = -2.0*K**2
p = 0.0
eps = 1e-6
for i in range(1, 20):
    term = a*math.exp(b*i**2)
    if abs(term) < eps*p:
        break
    p += term
    a *= -1.0

# plot the two distributions
pylab.plot(x1, [i/float(N1) for i in range(N1+1)], 'o-', drawstyle='steps',
           label='x1')
pylab.plot(x2, [i/float(N2) for i in range(N2+1)], 'o-', drawstyle='steps',
           label='x2')
pylab.plot([X, X], segment, 'D--', label='D = %g'%D)
pylab.ylabel('cumulative probability')
pylab.xlabel('x')
pylab.title('p-value = %3g%%'%(100*p))
pylab.legend(loc=2)
pylab.grid()
pylab.show()
