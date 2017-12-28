import scipy as sp
import numpy as np
#From https://www.stat.auckland.ac.nz/~fewster/325/notes/ch9.pdf
#Python soln = https://stackoverflow.com/questions/33385763/find-markov-steady-state-with-left-eigenvalues-using-numpy-or-scipy
#Starting values for t=0; any state equally likely.
start = np.array([[1/4,1/4,1/4,1/4]])
print(start)
#Specify the matrix, P
P = np.array([[0,.9,.1,0],[.8,.1,0,.1],[0,.5,.3,.2],[.1,0,0,.9]])
#We want to find the left matrix pi which produces pi.P = pi.
#By definition, this is the stationary matrix.
#Calculate the left eigenvector, which is the solution to the problem pi(P - I) = 0.
eigenvalue, eigenvector = sp.sparse.linalg.eigs(P.T, k=1,which ='LM')
print(eigenvalue)
print('Un-normalised eigenvector: ',eigenvector)
evect_norm = (eigenvector/eigenvector.sum()).real
print('Normalised eigenvector: ',evect_norm)
print('Check that pi*P = pi: ',np.dot(evect_norm.T,P).T.real)
print('Estimate using P^n: ',np.linalg.matrix_power(P,100)[0,:])
