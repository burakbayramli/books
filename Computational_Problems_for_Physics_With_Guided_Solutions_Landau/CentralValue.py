""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# CentralValue.py: Gaussian distribution from sum of randoms 
 
import random
import numpy as np, matplotlib.pyplot as plt
import matplotlib.mlab as mlab

N = 1000; NR = 10000   #  Sum N variables, distribution of sums 
SumList = [] # empty list

def SumRandoms():  # Sum N randoms in [0,1]
    sum = 0.0
    for i in range(0,N):  sum = sum + random.random() 
    return sum                    
    
def normal_dist_param():
    add = sum2 =0 
    for i in range(0,NR):  add = add + SumList[i]
    mu = add/NR               # Average distribution
    for i in range(0,NR): sum2 = sum2 + (SumList[i]-mu)**2
    sigma = np.sqrt(sum2/NR)
    return mu,sigma    

for i in range(0,NR):
    dist =SumRandoms()      
    SumList.append(dist)           # Fill list with NR sums
plt.hist(SumList, bins=100, normed=True) # True: normalize 
mu, sigma = normal_dist_param()            
x = np.arange(450,550)
rho = np.exp(-(x-mu)**2/(2*sigma**2))/(np.sqrt(2*np.pi*sigma**2)) 
plt.plot( x,rho, 'g-', linewidth=3.0)         # Normal distrib
plt.xlabel('Random Number x 1000')
plt.ylabel('Average of Random Numbers')
plt.title('Generated vs Analytic Normal Distribution')
plt.show()
