import numpy
import scipy.stats
import matplotlib.pyplot as plt

def weighted_sample(ws, vs, n):
    total = float(sum(w for w in ws))
    i = 0
    w = ws[0]
    v = vs[0]
    while n:
        x = total * (1 - numpy.random.random() ** (1.0 / n))
        total -= x
        while x > w:
            x -= w
            i += 1
            w = ws[i]
            v = vs[i]
        w -= x
        yield v
        n -= 1

# main 
y = [-2, -1, 0, 1.5, 2.5]
step = 0.01
theta = numpy.arange(0, 1, 0.005)
densunnorm = []
for th in theta:
    prod = 1    
    for yy in y:
        prod = prod * scipy.stats.cauchy.pdf(yy, th, 1)
    densunnorm.append(prod)
       
print (step*sum(densunnorm))
densnorm = densunnorm/(step*sum(densunnorm))*2
print densunnorm
print densnorm
print (step*sum(densunnorm))

res = [x for x in weighted_sample(step*densnorm, theta, 1000)]
plt.hist(res, 50)
plt.show()
