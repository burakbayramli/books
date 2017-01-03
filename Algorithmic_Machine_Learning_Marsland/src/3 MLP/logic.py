
# Code from Chapter 3 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

from numpy import *
import mlp

anddata = array([[0,0,0],[0,1,0],[1,0,0],[1,1,1]])
xordata = array([[0,0,0],[0,1,1],[1,0,1],[1,1,0]])

p = mlp.mlp(anddata[:,0:2],anddata[:,2:3],2)
p.mlptrain(anddata[:,0:2],anddata[:,2:3],0.25,1001)
p.confmat(anddata[:,0:2],anddata[:,2:3])

q = mlp.mlp(xordata[:,0:2],xordata[:,2:3],2,outtype='logistic')
q.mlptrain(xordata[:,0:2],xordata[:,2:3],0.25,5001)
q.confmat(xordata[:,0:2],xordata[:,2:3])

#anddata = array([[0,0,1,0],[0,1,1,0],[1,0,1,0],[1,1,0,1]])
#xordata = array([[0,0,1,0],[0,1,0,1],[1,0,0,1],[1,1,1,0]])
#
#p = mlp.mlp(anddata[:,0:2],anddata[:,2:4],2,outtype='linear')
#p.mlptrain(anddata[:,0:2],anddata[:,2:4],0.25,1001)
#p.confmat(anddata[:,0:2],anddata[:,2:4])
#
#q = mlp.mlp(xordata[:,0:2],xordata[:,2:4],2,outtype='linear')
#q.mlptrain(xordata[:,0:2],xordata[:,2:4],0.15,5001)
#q.confmat(xordata[:,0:2],xordata[:,2:4])
