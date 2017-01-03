"""
  Name     : 4375OS_09_30_pdf.py
  Book     : Python for Finance
  Publisher: Packt Publishing Ltd. 
  Author   : Yuxing Yan
  Date     : 12/26/2013
  email    : yany@canisius.edu
             paulyxy@hotmail.com
"""




def bs_call(S,X,T,rf,sigma):
    """
       Objective: Black-Schole-Merton option model
       Format   : bs_call(S,X,T,r,sigma)
               S: current stock price
               X: exercise price
               T: maturity date in years
              rf: risk-free rate (continusouly compounded)
           sigma: volatiity of underlying security 
       Example 1:  
         >>>bs_call(40,40,1,0.1,0.2)
         5.3078706338643578
    """    
    from scipy import log,exp,sqrt,stats
    d1=(log(S/X)+(rf+sigma*sigma/2.)*T)/(sigma*sqrt(T))
    d2 = d1-sigma*sqrt(T)
    return S*stats.norm.cdf(d1)-X*exp(-rf*T)*stats.norm.cdf(d2)


def bs_put(S,X,T,rf,sigma):
    """
       Objective: Black-Schole-Merton option model
       Format   : bs_call(S,X,T,r,sigma)
               S: current stock price
               X: exercise price
               T: maturity date in years
              rf: risk-free rate (continusouly compounded)
           sigma: volatiity of underlying security 
       Example 1: 
       >>> put=bs_put(40,40,0.5,0.05,0.2)
       >>> round(put,2)
       1.77
    """    
    from scipy import log,exp,sqrt,stats
    d1=(log(S/X)+(rf+sigma*sigma/2.)*T)/(sigma*sqrt(T))
    d2 = d1-sigma*sqrt(T)
    return X*exp(-rf*T)*stats.norm.cdf(-d2)-S*stats.norm.cdf(-d1)

def binomial_grid(n):
    import networkx as nx 
    import matplotlib.pyplot as plt 
    G=nx.Graph() 
    for i in range(0,n+1):     
        for j in range(1,i+2):         
            if i<n:             
                G.add_edge((i,j),(i+1,j))
                G.add_edge((i,j),(i+1,j+1)) 
    posG={}    #dictionary with nodes position 
    for node in G.nodes():     
        posG[node]=(node[0],n+2+node[0]-2*node[1]) 
    nx.draw(G,pos=posG)      

#from math import sqrt, log, pi,exp
#import re
#--------------------------------------------------------#
#--- Cumulative normal distribution        --------------#
#--------------------------------------------------------#
def CND(X):
    """ Cumulative standard normal distribution
            CND(x): x is a scale
            e.g., 
            >>> CND(0)
            0.5000000005248086
    """
    (a1,a2,a3,a4,a5)=(0.31938153,-0.356563782,1.781477937,-1.821255978,1.330274429)
    L = abs(X)
    K = 1.0 / (1.0 + 0.2316419 * L)
    w = 1.0 - 1.0 / sqrt(2*pi)*exp(-L*L/2.) * (a1*K + a2*K*K + a3*pow(K,3) +
    a4*pow(K,4) + a5*pow(K,5))
    if X<0:
        w = 1.0-w
    return w
