import matplotlib as plt
import numpy as np
sT = np.arange(0,40,5)
k=15;s0=10;c=2
y0=np.zeros(len(sT))
y1=sT-s0   #stock only
y2=(abs(sT-k)+sT-k)/2-c #long a call
y3=y1-y2 #covered call
plt.ylim(-10,30)
plt.plot(sT,y1)
plt.plot(sT,y2)
plt.plot(sT,y3,'red')
plt.plot(sY,y0,'b-.')
plt.plot([k,k],[-10,10],'black')
title('Covered call ( long one share and short one call)')
xlabel('Stock price')
ylabel('Profit (loss)')
plt.annotate('Stock only (long one share)', xy=(24,15),xytext=(15,20),
             arrowprops=dict(facecolor='blue',shrink=0.01),)
plt.annotate('Long one share, short a call',xy=(10,4),xytext=(9,25),
             arrowprops=dict(facecolor='red',shrink=0.01),)
plt.annotate('Exercise price= '+str(k), xy=(k+0.2,-10+0.5))

show()

