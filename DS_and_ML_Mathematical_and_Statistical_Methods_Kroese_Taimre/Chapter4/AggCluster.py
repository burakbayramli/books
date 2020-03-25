""" AggCluster.py """
import numpy as np 
from scipy.spatial.distance import cdist

def update_distances(D,i,j, sizes): # calculate distances for merged cluster
    n = D.shape[0]     
    d = np.inf * np.ones(n+1)
    for k in range(n): # Update distances     
       d[k] = ((sizes[i]+sizes[k])*D[i,k] + 
       (sizes[j]+sizes[k])*D[j,k] - 
       sizes[k]*D[i,j])/(sizes[i] + sizes[j] + sizes[k])
                   
    infs =  np.inf * np.ones(n) # array of infinity
    D[i,:],D[:,i],D[j,:],D[:,j] =  infs,infs,infs,infs # deactivate 
    new_D = np.inf * np.ones((n+1,n+1))
    new_D[0:n,0:n] = D # copy old matrix into new_D
    new_D[-1,:], new_D[:,-1] = d,d # add new row and column
    return new_D
            
def agg_cluster(X):  
    n = X.shape[0]
    sizes = np.ones(n)
    D = cdist(X, X,metric = 'sqeuclidean')   # initialize distance matr.
    np.fill_diagonal(D, np.inf * np.ones(D.shape[0]))    
    Z = np.zeros((n-1,4))  #linkage matrix encodes hierachy tree
    for t in range(n-1):
        i,j = np.unravel_index(D.argmin(), D.shape) # minimizer pair
        sizes = np.append(sizes, sizes[i] + sizes[j])
        Z[t,:]=np.array([i, j, np.sqrt(D[i,j]), sizes[-1]]) 
        D = update_distances(D, i,j, sizes)  # update distance matr.
    return Z   
    
# MAIN ROUTINE
import scipy.cluster.hierarchy as h 

X = np.genfromtxt('clusterdata.csv',delimiter=',') # read the data
Z = agg_cluster(X)  # form the linkage matrix

h.dendrogram(Z) # SciPy can produce a dendogram from Z 
# fcluster function assigns cluster ids to all points based on Z 
cl = h.fcluster(Z, criterion = 'maxclust', t=3) 

import matplotlib.pyplot as plt
plt.figure(2), plt.clf()
cols = ['red','green','blue']
colors = [cols[i-1] for i in cl]
plt.scatter(X[:,0], X[:,1],c=colors)
plt.show()
