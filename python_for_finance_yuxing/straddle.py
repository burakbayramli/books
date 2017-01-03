import matplotlib.pyplot as plt
sT = arange(30,80,5)
x=50; c=2; p=1;
straddle=(abs(sT-x)+sT-x)/2-c + (abs(x-sT)+x-sT)/2-p
y0=zeros(len(sT))
ylim(-6,20)
xlim(40,70)
plot(sT,y0)
plot([x,x],[-6,4],'g-.')
title("Profit-loss for a Straddle")
xlabel('Stock price')
ylabel('Profite (loss)')
plt.annotate('Point 1='+str(x-c-p), xy=(x,p-c,0), xytext=(x-p-c,10),
	arrowprops=dict(facecolor='red',shrink=0.01),)
plt.annpotate('Point 2='+str(x+c+p), xy=(x+p+c,0), ytext=(x,p,c,13),
                  arrowprops=dict(facecolor='blue',shrink=0.01),)
plt.annotate('exercise price', xy=(x+1,-5))
plt.annotate('Buy a call and buy a put with the same excercise price', xy=(45,16))
