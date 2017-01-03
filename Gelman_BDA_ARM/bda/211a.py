import numpy
import scipy.stats
import matplotlib.pyplot as plt

step = 0.01
y = [-2, -1, 0, 1.5, 2.5]
theta = numpy.arange(0, 1, 0.005)
densunnorm = []
for th in theta:
    prod = 1    
    for yy in y:
        prod = prod * scipy.stats.cauchy.pdf(yy, th, 1)
    densunnorm.append(prod)

print (step*sum(densunnorm))
densnorm = densunnorm/(step*sum(densunnorm))*2 # HACK: i included this 2 manually
print densunnorm
print densnorm
print (step*sum(densunnorm))
plt.plot(theta, densnorm)
plt.show()
