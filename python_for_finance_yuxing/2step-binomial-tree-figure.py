from math import sqrt,exp
import matplotlib.pyplot as plt
import p4f as p4f
s=10;r=0.02;sigma=0.2;T=3./12;x=10
n=2;deltaT=T/n;q=0
u=exp(sigma*sqrt(deltaT));d=1/u
a=exp((r-q)*deltaT)
p=(a-d)/(u-d)
su=round(s*u,2);suu=round(s*u*u,2)
sd=round(s*d,2);sdd=round(s*d*d,2)
sud=2
plt.figtext(0.08,0.6,'Stock '+str(s))
plt.figtext(0.33,0.76,"Stock price=$"+str(su))
plt.figtext(0.33,0.27,'Stock price='+str(sd))
plt.figtext(0.75,0.91,'Stock price=$'+str(suu))
plt.figtext(0.75,0.6,'Stock price=$'+str(sud))
plt.figtext(0.75,0.28,"Stock price="+str(sdd))
p4f.binomial_grid(n)
plt.show()

