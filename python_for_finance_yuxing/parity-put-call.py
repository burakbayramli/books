import pylab as pl
import numpy as np
x=10
sT=np.arange(0,30,5)
payoff_call=(abs(sT-x)+sT-x)/2
payoff_put=(abs(x-sT)+x-sT)/2
cash=np.zeros(len(sT))+x
#def graph(text,text2=''):
   # pl.xticks(())
   # pl.yticks(())
  #  pl.xlim(0,30)
 #   pl.ylim(0,20)
#    pl.plot([x,x],[0,3])
    #pl.text(x,-2,"X");
   # pl.text(0,x,"X")
  #  pl.text(x,x*1.7, text, ha='center', va='canter',size-10,
 #   pl.text(-5,10,text2,size=25)
#pl.figure(figsize=(6, 4))
