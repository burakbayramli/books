
# Code from Chapter 7 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

from pylab import *
from numpy import *

# The boosting example of a simple 2D dataset with 2 classes

def train(data,classes,weights,whichdim):
        
    error = zeros(10)
    for value in range(0,10,1):
        val = float(value)/10
        classn = where(data[whichdim,:]<val,-1,1)
        ind = where(classes!=classn)
        error[value] = sum(weights[ind])
    #print error, argmin(error)    
    return whichdim,float(argmin(error))/10,1-whichdim  
    
def classify(data,dim,value):
    classn = where(data[dim,:]<value,-1,1)
    ind = where(classes!=classn,1,0)
    return classn, ind
        
def boost(data,classes,testdata):
    T = 20
    N = shape(data)[1]
    ndim = shape(data)[0]
    classifiers = zeros((2,T))
    whichdim = 0

    w = ones((N,T+1),dtype=float)/N
    index = ones((N,T+1))
    e = zeros(T)
    alpha = zeros(T+1)

    err = zeros((2,T+1))

    poutput = zeros((T+1,N))
    ptoutput = zeros((T+1,N))
    po = zeros(T+1)
    pto = zeros(T+1)
    
    for t in range(T):
        classifiers[0,t],classifiers[1,t],whichdim = train(data,classes,w[:,t],whichdim)
        #print "Out", classifiers[:,t]
        outputs,errors = classify(data,classifiers[0,t],classifiers[1,t])
        toutputs,terrors = classify(testdata,classifiers[0,t],classifiers[1,t])

        which = where(outputs<=0)
        which2 = where(outputs>0)
        figure()
        plot(data[0,which],data[1,which],'ko',ms=15)
        plot(data[0,which2],data[1,which2],'k^',ms=15)
        index[:,t] = errors
        #print "index: ", index[:,t]
        #print "e: ", w[:,t] * index[:,t]
        e[t] = sum(w[:,t]*index[:,t])/sum(w[:,t])
        #print "e: ",e[t]
            
        if t>0 and (e[t]==0 or e[t]>=0.5):
            T=t
            alpha = alpha[:t]
            index = index[:,:t]
            w = w[:,:t]
            break

        alpha[t] = log((1-e[t])/e[t])
        #print "alpha: ", alpha[t]
        w[:,t+1] = w[:,t]* exp(alpha[t]*index[:,t])
        w[:,t+1] = w[:,t+1]/sum(w[:,t+1])
        #print "w: ", w[:,t+1], sum(w[:,t+1])
        
        
        outputs = zeros((N,t))
        toutputs = zeros((N,t))
        for i in range(t):
            outputs[:,i],errors  = classify(data,classifiers[0,i],classifiers[1,i])
            toutputs[:,i],terrors  = classify(testdata,classifiers[0,i],classifiers[1,i])
    

        for n in range(N):
            poutput[t,n] = sum(alpha[:t]*outputs[n,:])/sum(alpha)
            ptoutput[t,n] = sum(alpha[:t]*toutputs[n,:])/sum(alpha)
        poutput[t,:] = where(poutput[t,:]>0,1,-1)
        ptoutput[t,:] = where(ptoutput[t,:]>0,1,-1)
        po[t] = shape(where(poutput[t,:]!=classes))[1]
        pto[t] = shape(where(ptoutput[t,:]!=testclasses))[1]
    #print "output: "
    #print alpha
    outputs = zeros((N,shape(w)[1]))
    for t in range(T):
        outputs[:,t],errors  = classify(data,classifiers[0,t],classifiers[1,t])
    
    output = zeros(N)
    for n in range(N):
        output[n] = sum(alpha*outputs[n,:])/sum(alpha)
        
    #print output
    #print classes 
    which = where(output<=0)
    which2 = where(output>0)
    figure()
    plot(data[0,which],data[1,which],'ko',ms=15)
    plot(data[0,which2],data[1,which2],'k^',ms=15)
    title('Output on training data')
    #axis('off')
    
    outputs = zeros((N,shape(w)[1]))
    for t in range(T):
        outputs[:,t],errors  = classify(testdata,classifiers[0,t],classifiers[1,t])
    
    output = zeros(N)
    for n in range(N):
        output[n] = sum(alpha*outputs[n,:])/sum(alpha)    
    which = where(output<=0)
    which2 = where(output>0)
    figure()
    title('Output on test data')
    plot(testdata[0,which],testdata[1,which],'ko',ms=15)
    plot(testdata[0,which2],testdata[1,which2],'k^',ms=15)
        
    figure()
    plot(arange(T),po[:T]/N,'k-',arange(T),pto[:T]/N,'k--')
    legend(('Training error','Test error'))
    xlabel('Iterations')
    ylabel('Error')
    return output

ndata = 50
data = random.rand(2,ndata)
#which = where(data[0,:]>0.4)
#which2 = where(data[0,:]<=0.4)
classes = where(((data[0,:]>0.4) & (data[1,:]>0.4)),1,-1)


#classes = where(((data[0,:]>0.7) & (data[1,:]>0.7)) | ((data[0,:]<0.3) & (data[1,:]<0.3)),1,-1)

#false = where(data[0,:]<0.3)
#new = random.randint(len(false))
#classes[false[0][new]] = 1

which = where(classes==-1)
which2 = where(classes==1)
plot(data[0,which],data[1,which],'ko',ms=15)
plot(data[0,which2],data[1,which2],'k^',ms=15)
title('Training Data')
testdata = random.rand(2,ndata)
testclasses = where(((testdata[0,:]>0.4) & (testdata[1,:]>0.4)),1,-1)
boost(data,classes,testdata)

figure()
title('Test Data')
which = where(testclasses==-1)
which2 = where(testclasses==1)
plot(testdata[0,which],testdata[1,which],'ko',ms=15)
plot(testdata[0,which2],testdata[1,which2],'k^',ms=15)


show()
