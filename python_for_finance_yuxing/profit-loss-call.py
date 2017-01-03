import numpy
import matlibplot

s =  np.arange(30,70,5)
x=45;call=2.5
profit=(abs(s-x)+s-x)/2-call
y2=zeros(len(s))
ylim(-30,50)

plot(s,profit)
plot(s,y2,'-.')
plot(s,-profit)
title("Profit/Loss function")
xlabel('Stock price')
ylabel('Profit (loss)')
plt.annotate('Call option buyer', xy=(55,15),xytext=(35,20),
             arrowprops=dict(facecolor='blue',shrink=0.01),)
plt.annotate('Call option seller', xy=(55,-10), xytext=(40,-20),
             arrowprops=dict(facecolor='red',shrink=0.01),)
show() 
