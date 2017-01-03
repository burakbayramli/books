
# Code from Chapter 13 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# The basic TD(0) algorithm with the Cliff example

from numpy import *

def TDZero_cliff():

    R = -ones((4,7,4))
    R[0,:,0] = -inf
    R[:,0,3] = -inf
    R[3,:,2] = -inf
    R[:,6,1] = -inf
    R[1,1:6,0] = -100
    R[0,0,1] = -100
    R[0,6,3] = -100
    
    t = zeros((4,7,4,2))
    for i in range(4):
        for j in range(7):
            for k in range(4):
                if k==2:
                    if i<3:
                        t[i,j,k,0] = i+1
                        t[i,j,k,1] = j
                    else:
                        t[i,j,k,0] = i
                        t[i,j,k,1] = j                  
                elif k==1:
                    if j<6:
                        t[i,j,k,0] = i
                        t[i,j,k,1] = j+1
                    else:
                        t[i,j,k,0] = i
                        t[i,j,k,1] = j 
                    if i==0 and j==0:
                        t[i,j,k,0] = 0
                        t[i,j,k,1] = 0   
                elif k==0:
                    if i==0 and j==6:
                        # Finished
                        t[i,j,k,0] = 0
                        t[i,j,k,1] = 0
                    if i>0:
                        t[i,j,k,0] = i-1
                        t[i,j,k,1] = j
                    else:
                        t[i,j,k,0] = i
                        t[i,j,k,1] = j
                    
                    if i==1 and 1<=j<=5:
                        t[i,j,k,0] = 0
                        t[i,j,k,1] = 0 
                else:
                    if j>0:
                        t[i,j,k,0] = i
                        t[i,j,k,1] = j-1
                    else:
                        t[i,j,k,0] = i
                        t[i,j,k,1] = j
                    if i==0 and j==6:
                        t[i,j,k,0] = 0
                        t[i,j,k,1] = 0
    
    #print t[:,:,3,0] ,t[:,:,3,1]
    
    #Q = random.random_sample(shape(R))*0.1-0.05
    Q = zeros(shape(R))
    mu = 0.7
    gamma = 0.4
    epsilon = 0.05
    nits = 0

    while nits < 1000:
        # Pick initial state
        s = array([0,0]) #array([random.randint(4),random.randint(7)])

        #print s, shape(s)
        #print shape(Q), shape(Q[s[0],s[1],:])
        inEpisode = 1
        # Stop when the accepting state is reached
        while inEpisode:
            r=-inf
            while r==-inf:
                # epsilon-greedy
                if (random.rand()<epsilon):
                    a = random.randint(4)
                else:
                    a = argmax(Q[s[0],s[1],:])
            
                r = R[s[0],s[1],a]
            #print "r = ", r
            sprime = t[s[0],s[1],a,:]
            #print "sprime",sprime
            
            Q[s[0],s[1],a] += mu * (r + gamma*max(Q[sprime[0],sprime[1],:]) - Q[s[0],s[1],a])
            #print "there"
            s = sprime

            if s[0]==0 and s[1]==6 and a==0:
                # Have reached endpoint
                inEpisode = 0
        nits = nits+1
        print nits
    print Q
    return Q, R, t

def TDgo(Q,R,t):
    s = array([0,0])
    rtotal = 0
    finished = 0
    epsilon = 0.05
    while not(finished):
        r=-inf
        while r==-inf:
            # epsilon-greedy
            if (random.rand()<epsilon):
                a = random.randint(4)
            else:
                a = argmax(Q[s[0],s[1],:])
            r = R[s[0],s[1],a]
        s = t[s[0],s[1],a,:]
        #print s
        rtotal += r
        if s[0]==0 and s[1]==6 and a==0:
            finished = 1
    print "Total cost = ",rtotal
    return rtotal
    
Q,R,t = TDZero_cliff()
cost = TDgo(Q,R,t)
cost = TDgo(Q,R,t)
cost = TDgo(Q,R,t)
cost = TDgo(Q,R,t)
cost = TDgo(Q,R,t)
