
# Code from Chapter 10 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# The Locally Linear Embedding algorithm, and the swissroll example
from pylab import *
from numpy import *

def swissroll():
	# Make the swiss roll dataset
	N = 1000
	noise = 0.05

	t = 3*math.pi/2 * (1 + 2*random.rand(1,N))
	h = 21 * random.rand(1,N)
	data = concatenate((t*cos(t),h,t*sin(t))) + noise*random.randn(3,N)	
	return transpose(data), squeeze(t)

def lle(data,nRedDim=2,K=12):

	ndata = shape(data)[0]
	ndim = shape(data)[1]
	d = zeros((ndata,ndata),dtype=float)
	
	# Inefficient -- not matrices
	for i in range(ndata):
		for j in range(i+1,ndata):
			for k in range(ndim):
				d[i,j] += (data[i,k] - data[j,k])**2
			d[i,j] = sqrt(d[i,j])
			d[j,i] = d[i,j]

	indices = d.argsort(axis=1)
	neighbours = indices[:,1:K+1]

	W = zeros((K,ndata),dtype=float)

	for i in range(ndata):
		Z  = data[neighbours[i,:],:] - kron(ones((K,1)),data[i,:])
		C = dot(Z,transpose(Z))
		C = C+identity(K)*1e-3*trace(C)
		W[:,i] = transpose(linalg.solve(C,ones((K,1))))
		W[:,i] = W[:,i]/sum(W[:,i])

	M = eye(ndata,dtype=float)
	for i in range(ndata):
		w = transpose(ones((1,shape(W)[0]))*transpose(W[:,i]))
		j = neighbours[i,:]
		#print shape(w), shape(dot(w,transpose(w))), shape(M[i,j])
		ww = dot(w,transpose(w))
		for k in range(K):
			M[i,j[k]] -= w[k]
			M[j[k],i] -= w[k]
			for l in range(K):
			     M[j[k],j[l]] += ww[k,l]
	
	evals,evecs = linalg.eig(M)
	ind = argsort(evals)
	y = evecs[:,ind[1:nRedDim+1]]*sqrt(ndata)
	return evals,evecs,y

data,t = swissroll()
evals,evecs,y = lle(data)

t -= t.min()
t /= t.max()
scatter(y[:,0],y[:,1],s=50,c=t,cmap=cm.gray)
axis('off')
show()
