import pandas as pd
import numpy as np
import cv2
import matplotlib.pyplot as plt
import scipy.linalg as lin

def compute_fundamental(x1, x2):
  '''Computes the fundamental matrix from corresponding points x1, x2 using
  the 8 point algorithm.'''
  n = x1.shape[1]
  if x2.shape[1] != n:
    raise ValueError('Number of points do not match.')

  # Normalization is done in compute_fundamental_normalized().
  A = np.zeros((n, 9))
  for i in range(n):
    A[i] = [x1[0, i] * x2[0, i],  x1[0, i] * x2[1, i],  x1[0, i] * x2[2, i],
            x1[1, i] * x2[0, i],  x1[1, i] * x2[1, i],  x1[1, i] * x2[2, i],
            x1[2, i] * x2[0, i],  x1[2, i] * x2[1, i],  x1[2, i] * x2[2, i],
           ]


  # Solve A*f = 0 using least squares.
  U, S, V = np.linalg.svd(A)
  F = V[-1].reshape(3, 3)

  # Constrain F to rank 2 by zeroing out last singular value.
  U, S, V = np.linalg.svd(F)
  S[2] = 0
  F = np.dot(U, np.dot(np.diag(S), V))
  return F / F[2, 2]

tpts1 = [[10, 232], [92, 230], [8, 334], [92, 333], [289, 230], \
        [354, 278], [289, 340], [353, 332], [69, 90], [294, 149], \
        [44, 475], [336, 433]]

tpts2 = [[123, 239], [203, 237], [123, 338], [202, 338], [397, 236],\
        [472, 286], [398, 348], [472, 341], [182, 99], [401, 153], \
        [148, 471], [447, 445]]

tpts1 = np.array(tpts1)
tpts2 = np.array(tpts2)

#img1 = cv2.imread("/home/burak/Documents/books/Multiple_View_Geometry_Lecture_Cremers/ex6/batinria0.tif")
#img2 = cv2.imread("/home/burak/Documents/books/Multiple_View_Geometry_Lecture_Cremers/ex6/batinria1.tif")
#for pt in tpts1: cv2.circle(img1, tuple(pt), 4, (0,0,255), -1)
#cv2.imwrite('vision_20recons_05.jpg',img1)
#for pt in tpts2: cv2.circle(img2, tuple(pt), 4, (0,0,255), -1)
#cv2.imwrite('vision_20recons_06.jpg',img2)

tpts1[:,1] = 512-tpts1[:,1]
tpts2[:,1] = 512-tpts2[:,1]

import fund
def make_hom(points):
    return np.vstack((points,np.ones((1,points.shape[1])))) 

print tpts1
F = compute_fundamental(tpts1,tpts2)
#F, mask = cv2.findFundamentalMat(tpts1,tpts2,method=cv2.RANSAC, param1=3., param2=0.99)
#F = fund.eight_point_algorithm(make_hom(tpts1.T),make_hom(tpts2.T))
print F

K = np.array([844.310547, 0, 243.413315, 0, 1202.508301, 281.529236, 0, 0, 1])
K = K.reshape((3,3))
def triangulate_point(u1, u2, P1, P2):
  A = [[u1[0]*P1[2,0]-P1[0,0],u1[0]*P1[2,1]-P1[0,1],u1[0]*P1[2,2]-P1[0,2]],
       [u1[1]*P1[2,0]-P1[1,0],u1[1]*P1[2,1]-P1[1,1],u1[1]*P1[2,2]-P1[1,2]],
       [u2[0]*P2[2,0]-P2[0,0],u2[0]*P2[2,1]-P2[0,1],u2[0]*P2[2,2]-P2[0,2]],
       [u2[1]*P2[2,0]-P2[1,0],u2[1]*P2[2,1]-P2[1,1],u2[1]*P2[2,2]-P2[1,2]]]
  B = [[-(u1[0]*P1[2,3]-P1[0,3])],
       [-(u1[1]*P1[2,3]-P1[1,3])],
       [-(u2[0]*P2[2,3]-P2[0,3])],
       [-(u2[1]*P2[2,3]-P2[1,3])]]
  A = np.array(A)
  B = np.array(B)
  X = lin.lstsq(A,B)[0].T[0]
  res = np.array([X[0],X[1],X[2],1])
  return res

def triangulate(x1, x2, P1, P2):
  X = [triangulate_point(x1[i, :], x2[i, :], P1, P2) for i in range(len(x1))]
  return np.array(X).T

P1 = np.eye(4)
P2 = np.array([[ 0.878, -0.01 ,  0.479, -1.995],
              [ 0.01 ,  1.   ,  0.002, -0.226],
              [-0.479,  0.002,  0.878,  0.615],
              [ 0.   ,  0.   ,  0.   ,  1.   ]])
# Homogeneous arrays
a3xN = np.array([[ 0.091,  0.167,  0.231,  0.083,  0.154],
                 [ 0.364,  0.333,  0.308,  0.333,  0.308],
                 [ 1.   ,  1.   ,  1.   ,  1.   ,  1.   ]])
b3xN = np.array([[ 0.42 ,  0.537,  0.645,  0.431,  0.538],
                 [ 0.389,  0.375,  0.362,  0.357,  0.345],
                 [ 1.   ,  1.   ,  1.   ,  1.   ,  1.   ]])
X = triangulate_point( a3xN.T[0], b3xN.T[0], P1, P2 )
X /= X[3]
x1 = np.dot(P1[:3],X)
x2 = np.dot(P2[:3],X)
x1 /= x1[2]
x2 /= x2[2]
 
print 'X', X
print 'x', x1
print 'x2', x2

from mpl_toolkits.mplot3d import axes3d
import cv2 

E = K.T.dot(F).dot(K)
R1,R2,t = cv2.decomposeEssentialMat(E)
for i,P in enumerate(((R1,t),(R1,-t),(R2,t),(R2,-t))):
    P1 = K.dot(np.hstack(P))       # Projection matrix of second cam is ready
    P00 = np.float64([ [1,0,0,0],
                       [0,1,0,0],
                       [0,0,1,0]   ]) # Projection matrix of first cam at origin
    P0 = K.dot(P00) 
    X = triangulate(tpts1, tpts2, P0, P1)
    fig = plt.figure()
    ax = fig.gca(projection='3d')    
    ax.plot(X[0], X[2], X[1], 'r.')
    #plt.savefig('vision_20recons_04_%d.png' % i)
    plt.show()

