import scipy.stats as sp
import numpy as np
import matplotlib.pyplot as plt
from scipy import sqrt,exp,log,pi
x = np.linspace(0,3,200)
mu=0
sigma0=[0.25,0.5,1]
color=['blue', 'red', 'green']
target=[(1.2,1.3),(1.7,0.4),(0.18,0.7)]
start=[(1.8,1.4),(1.9,0.6),(0.18,1.6)]
for i in range(len(sigma0)):
    sigma=sigma0[i]
    y=1/(x*sigma*sqrt(2*pi))*exp(-(log(x)-mu)**2/(2*sigma*sigma))
    plt.annotate('mu='+str(mu)+', sigma='+str(sigma), xy=target[i], xytext=start[i], arrowprops=dict(facecolor=color[i],shrink=0.01),)
    plt.plot(x,y,color[i])
plt.title('Lognormal distribution')
plt.xlabel('x')
plt.ylabel('lognormal density distribution')
plt.show()
    
