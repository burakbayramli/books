## module triangleQuad
''' integral = triangleQuad(f,xc,yc).
    Integration of f(x,y) over a triangle using
    the cubic formula.
    {xc},{yc} are the corner coordinates of the triangle.
'''
from numarray import array,matrixmultiply

def triangleQuad(f,xc,yc):
    alpha = array([[1.0/3, 1.0/3.0, 1.0/3.0],  \
                   [0.2, 0.2, 0.6],            \
                   [0.6, 0.2, 0.2],            \
                   [0.2, 0.6, 0.2]])
    W = array([-27.0/48.0 ,25.0/48.0, 25.0/48.0, 25.0/48.0])
    x = matrixmultiply(alpha,xc)
    y = matrixmultiply(alpha,yc)
    A = (xc[1]*yc[2] - xc[2]*yc[1]      \
       - xc[0]*yc[2] + xc[2]*yc[0]      \
       + xc[0]*yc[1] - xc[1]*yc[0])/2.0
    sum = 0.0
    for i in range(4):
        sum = sum + W[i] * f(x[i],y[i])
    return A*sum
