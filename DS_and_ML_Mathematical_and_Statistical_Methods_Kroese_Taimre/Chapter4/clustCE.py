""" clustCE.py """
import numpy as np
np.set_printoptions(precision=4)

Xmat = np.genfromtxt('clusterdata.csv', delimiter=',')
K = 3
n, D = Xmat.shape

def Scluster(c):
    n, D = Xmat.shape
    dist2 = np.zeros((K,n))
    cc = c.reshape(D,K)
    for i in range(0,K):
        dist2[i,:] = np.sum((Xmat - cc[:,i].T)**2, 1)
    minvals = np.amin(dist2,0)
    return minvals.mean()

numvar = K*D
mu = np.zeros(numvar)  #initialize centers
sigma = np.ones(numvar)*2
rho = 0.1
N = 500; Nel = int(N*rho); eps = 0.001

func = Scluster
best_trj = np.array(numvar)
best_perf = np.Inf
trj = np.zeros(shape=(N,numvar))

while(np.max(sigma)>eps): 
        for i in range(0,numvar):
            trj[:,i] = (np.random.randn(N,1)*sigma[i]+ mu[i]).reshape(N,)
        S = np.zeros(N)
        for i in range(0,N):
            S[i] = func(trj[i])
            
        sortedids = np.argsort(S) # from smallest to largest   
        S_sorted = S[sortedids]  
        best_trj = np.array(n)
        best_perf = np.Inf
        eliteids = sortedids[range(0,Nel)]           
        eliteTrj = trj[eliteids,:]
        mu = np.mean(eliteTrj,axis=0)
        sigma = np.std(eliteTrj,axis=0)
        
        if(best_perf>S_sorted[0]):
            best_perf = S_sorted[0]
            best_trj = trj[sortedids[0]]
            
print(best_perf)
print(best_trj.reshape(2,3))