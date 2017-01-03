## module gaussQuad2
''' I = gaussQuad2(f,xc,yc,m).
    Gauss-Legendre integration of f(x,y) over a
    quadrilateral using integration order m.
    {xc},{yc} are the corner coordinates of the quadrilateral.
'''
from gaussNodes import *
from numarray import zeros,Float64,dot

def gaussQuad2(f,x,y,m):

    def jac(x,y,s,t):
        J = zeros((2,2),type=Float64)
        J[0,0] = -(1.0 - t)*x[0] + (1.0 - t)*x[1]  \
                + (1.0 + t)*x[2] - (1.0 + t)*x[3]
        J[0,1] = -(1.0 - t)*y[0] + (1.0 - t)*y[1]  \
                + (1.0 + t)*y[2] - (1.0 + t)*y[3]
        J[1,0] = -(1.0 - s)*x[0] - (1.0 + s)*x[1]  \
                + (1.0 + s)*x[2] + (1.0 - s)*x[3]
        J[1,1] = -(1.0 - s)*y[0] - (1.0 + s)*y[1]  \
                + (1.0 + s)*y[2] + (1.0 - s)*y[3]
        return (J[0,0]*J[1,1] - J[0,1]*J[1,0])/16.0

    def map(x,y,s,t):
        N = zeros((4),type=Float64)
        N[0] = (1.0 - s)*(1.0 - t)/4.0
        N[1] = (1.0 + s)*(1.0 - t)/4.0
        N[2] = (1.0 + s)*(1.0 + t)/4.0
        N[3] = (1.0 - s)*(1.0 + t)/4.0
        xCoord = dot(N,x)
        yCoord = dot(N,y)
        return xCoord,yCoord

    s,A = gaussNodes(m)
    sum = 0.0
    for i in range(m):
        for j in range(m):
            xCoord,yCoord = map(x,y,s[i],s[j])
            sum = sum + A[i]*A[j]*jac(x,y,s[i],s[j])  \
                       *f(xCoord,yCoord)
    return sum


                               
    
