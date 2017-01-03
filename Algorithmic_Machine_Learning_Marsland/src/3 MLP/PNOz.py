
# Code from Chapter 3 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# The Palmerston North Ozone time series example

from pylab import *
from numpy import *

PNoz = loadtxt('PNOz.dat')
ion()
plot(arange(shape(PNoz)[0]),PNoz[:,2],'.')
xlabel('Time (Days)')
ylabel('Ozone (Dobson units)')

# Normalise data
PNoz[:,2] = PNoz[:,2]-PNoz[:,2].mean()
PNoz[:,2] = PNoz[:,2]/PNoz[:,2].max()

# Assemble input vectors
t = 2
k = 3

lastPoint = shape(PNoz)[0]-t*(k+1)-1
inputs = zeros((lastPoint,k))
targets = zeros((lastPoint,1))
for i in range(lastPoint):
    inputs[i,:] = PNoz[i:i+t*k:t,2]
    targets[i] = PNoz[i+t*(k+1),2]
    
test = inputs[-400:,:]
testtargets = targets[-400:]

# Randomly order the data
inputs = inputs[:-400,:]
targets = targets[:-400]
change = range(shape(inputs)[0])
random.shuffle(change)
inputs = inputs[change,:]
targets = targets[change,:]

train = inputs[::2,:]
traintargets = targets[::2]
valid = inputs[1::2,:]
validtargets = targets[1::2]

# Train the network
import mlp
net = mlp.mlp(train,traintargets,3,outtype='linear')
net.earlystopping(train,traintargets,valid,validtargets,0.25)

test = concatenate((test,-ones((shape(test)[0],1))),axis=1)
testout = net.mlpfwd(test)

figure()
plot(arange(shape(test)[0]),testout,'.')
plot(arange(shape(test)[0]),testtargets,'x')
legend(('Predictions','Targets'))
print 0.5*sum((testtargets-testout)**2)
show()
