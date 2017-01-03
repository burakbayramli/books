
# Code from Chapter 15 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# Demonstration of the Markov Random Field method of image denoising
from pylab import *
from numpy import *

def MRF(I,J,eta=2.0,zeta=1.5):
    ind =arange(shape(I)[0])
    random.shuffle(ind)
    orderx = ind.copy()
    random.shuffle(ind)

    for i in orderx:
        for j in ind:
            oldJ = J[i,j]
            J[i,j]=1
            patch = 0
            for k in range(-1,1):
                for l in range(-1,1):
                    patch += J[i,j] * J[i+k,j+l]
            energya = -eta*sum(I*J) - zeta*patch
            J[i,j]=-1
            patch = 0
            for k in range(-1,1):
                for l in range(-1,1):
                    patch += J[i,j] * J[i+k,j+l]
            energyb = -eta*sum(I*J) - zeta*patch
            if energya<energyb:
                J[i,j] = 1
            else:
                J[i,j] = -1
    return J
            
I = imread('world.png')
N = shape(I)[0]
I = I[:,:,0]
I = where(I<0.1,-1,1)
imshow(I)
title('Original Image')

noise = random.rand(N,N)
J = I.copy()
ind = where(noise<0.1)
J[ind] = -J[ind]
figure()
imshow(J)
title('Noisy image')
newJ = J.copy()
newJ = MRF(I,newJ)
figure()
imshow(newJ)
title('Denoised version')
print sum(I-J), sum(I-newJ)
show()
