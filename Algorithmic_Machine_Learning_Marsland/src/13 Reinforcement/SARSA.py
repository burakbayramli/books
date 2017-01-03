
# Code from Chapter 13 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# The basic SARSA algorithm with the Europe example

from numpy import *

def SARSA():

    R = array([[-5,0,-inf,-inf,-inf,-inf],[0,-5,0,0,-inf,-inf],[-inf,0,-5,0,-inf,100],[-inf,0,0,-5,0,-inf],[-inf,-inf,-inf,0,-5,100],[-inf,-inf,0,-inf,-inf,0]])
    t = array([[1,1,0,0,0,0],[1,1,1,1,0,0],[0,1,1,1,0,1],[0,1,1,1,1,0],[0,0,0,1,1,1],[0,0,1,0,1,1]])

   
    
    nStates = shape(R)[0]
    nActions = shape(R)[1]
    Q = random.rand(nStates,nActions)*0.1-0.05
    mu = 0.7
    gamma = 0.4
    epsilon = 0.1
    nits = 0

    while nits < 1000:
        # Pick initial state
        s = random.randint(nStates)
        # epsilon-greedy
        if (random.rand()<epsilon):
            indices = where(t[s,:]!=0)
            pick = random.randint(shape(indices)[1])
            a = indices[0][pick]
        else:
            a = argmax(Q[s,:])
                
        # Stop when the accepting state is reached
        while s!=5:
            r = R[s,a]
            # For this example, new state is the chosen action
            sprime = a
            
            # epsilon-greedy
            if (random.rand()<epsilon):
                indices = where(t[sprime,:]!=0)
                pick = random.randint(shape(indices)[1])
                aprime = indices[0][pick]
                #print s,a
            else:
                aprime = argmax(Q[sprime,:])
            #print "here", Q[sprime,aprime], Q[s,a], s, a
            
            Q[s,a] += mu * (r + gamma*Q[sprime,aprime] - Q[s,a])

            s = sprime
            a = aprime
            
        nits = nits+1

    print Q

SARSA()
