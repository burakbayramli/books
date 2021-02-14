""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# NewtonNDanimate.py:              MultiDimension Newton Search

from visual import *
from numpy.linalg import solve
from visual.graph import *

scene = display(x=0,y=0,width=500,height=500,
                title='String and masses configuration')
tempe = curve(x=range(0,500),color=color.black)

n = 9
eps = 1e-3
deriv = zeros( (n, n), float)
f = zeros( (n), float)
x = array([0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1., 1., 1.])

def plotconfig():
    for obj in scene.objects:
        obj.visible=0                  # Erase previous configuration
    L1 = 3.0
    L2 = 4.0
    L3 = 4.0
    xa = L1*x[3]                # L1*cos(th1)
    ya = L1*x[0]                # L1 sin(th1)
    xb = xa+L2*x[4]             # L1*cos(th1)+L2*cos(th2)
    yb = ya+L2*x[1]             # L1*sin(th1)+L2*sen(th2)
    xc = xb+L3*x[5]             # L1*cos(th1)+L2*cos(th2)+L3*cos(th3)
    yc = yb-L3*x[2]             # L1*sin(th1)+L2*sen(th2)-L3*sin(th3)
    mx = 100.0                  # for linear coordinate transformation
    bx = -500.0                 # from 0=< x =<10
    my = -100.0                 # to    -500 =<x_window=>500
    by = 400.0                  # same transformation for y
    xap = mx*xa+bx              # to keep aspect ratio
    yap = my*ya+by
    ball1 = sphere(pos=(xap,yap), color=color.cyan,radius=15) 
    xbp = mx*xb+bx
    ybp = my*yb+by
    ball2 = sphere(pos=(xbp,ybp), color=color.cyan,radius=25) 
    xcp = mx*xc+bx
    ycp = my*yc+by
    x0 = mx*0+bx
    y0 = my*0+by
    line1 = curve(pos=[(x0,y0),(xap,yap)], color=color.yellow,radius=4)
    line2 = curve(pos=[(xap,yap),(xbp,ybp)], color=color.yellow,radius=4)
    line3 = curve(pos=[(xbp,ybp),(xcp,ycp)], color=color.yellow,radius=4)
    topline = curve(pos=[(x0,y0),(xcp,ycp)], color=color.red,radius=4)

def F(x, f):                                       # F function
    f[0] = 3*x[3]  +  4*x[4]  +  4*x[5]  -  8.0
    f[1] = 3*x[0]  +  4*x[1]  -  4*x[2]
    f[2] = x[6]*x[0]  -  x[7]*x[1]  -  10.0
    f[3] = x[6]*x[3]  -  x[7]*x[4]
    f[4] = x[7]*x[1]  +  x[8]*x[2]  -  20.0
    f[5] = x[7]*x[4]  -  x[8]*x[5]
    f[6] = pow(x[0], 2)  +  pow(x[3], 2)  -  1.0
    f[7] = pow(x[1], 2)  +  pow(x[4], 2)  -  1.0
    f[8] = pow(x[2], 2)  +  pow(x[5], 2)  -  1.0
    
def dFi_dXj(x, deriv, n):                           # Derivatives
    h = 1e-4                                             
    for j in range(0, n):
         temp = x[j]
         x[j] = x[j] +  h/2.
         F(x, f)                                                 
         for i in range(0, n):  deriv[i, j] = f[i] 
         x[j] = temp
    for j in range(0, n):
         temp = x[j]
         x[j] = x[j] - h/2.
         F(x, f)
         for i in range(0, n): deriv[i, j] = (deriv[i, j] - f[i])/h
         x[j] = temp
         
for it in range(1, 100):
      rate(1)                            # 1 second between graphs
      F(x, f)                              
      dFi_dXj(x, deriv, n)   
      B = array([[-f[0]], [-f[1]], [-f[2]], [-f[3]], [-f[4]], [-f[5]],\
			[-f[6]], [-f[7]], [-f[8]]])      
      sol = solve(deriv, B)
      dx = take(sol, (0, ), 1)               # First column of sol
      for i in range(0, n):
        x[i]  = x[i]  +  dx[i]
      plotconfig()
      errX = errF = errXi = 0.0
      for i in range(0, n):
        if ( x[i] !=  0.): errXi = abs(dx[i]/x[i])
        else:  errXi = abs(dx[i])
        if ( errXi > errX): errX = errXi                            
        if ( abs(f[i]) > errF ):  errF = abs(f[i])        
        if ( (errX <=  eps) and (errF <=  eps) ): break
        
print('Number of iterations = ', it, "\n Final Solution:")
for i in range(0, n):
        print('x[', i, '] =  ', x[i])
