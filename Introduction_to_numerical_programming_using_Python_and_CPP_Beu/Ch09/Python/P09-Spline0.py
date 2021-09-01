# Interpolation with cubic splines
from modfunc import *

def sinc(x): return (sin(x)/x if x else 1e0)

# main

xmin = 0e0; xmax = 5*asin(1e0)              # tabulation interval: [0,5*pi/2]
n  = 6                                                # number of data points
ni = 100                                     # number of interpolation points

a = [0]*(n+1); b = [0]*(n+1)                            # spline coefficients
c = [0]*(n+1); d = [0]*(n+1)
x = [0]*(n+1); y = [0]*(n+1)                                    # data points

h = (xmax-xmin)/(n-1)                                  # generate data points
for i in range(1,n+1):
   xi = xmin + (i-1)*h
   x[i] = xi; y[i] = sinc(xi)

Spline(x,y,n,0e0,0e0,0,a,b,c,d,x,y,0)                       # natural splines

out = open("interpol.txt","w")                             # open output file
out.write("      x           y           xi          yi     " +
          "     y1          y2          f\n")
h = (xmax-xmin)/(ni-1)
for i in range(1,ni+1):
   xi = xmin + (i-1)*h                               # interpolation argument
   ip = 1
   while (ip < n-1 and xi > x[ip+1]): ip += 1               # index of spline

   yi = ((a[ip]*xi + b[ip])*xi + c[ip])*xi + d[ip]                   # spline
   y1 = (3*a[ip]*xi + 2*b[ip])*xi + c[ip]                    # 1st derivative
   y2 = (6*a[ip]*xi + 2*b[ip])                               # 2nd derivative

   if (i <= n):
      out.write(
       "{0:12.3e}{1:12.3e}{2:12.3e}{3:12.3e}{4:12.3e}{5:12.3e}{6:12.3e}\n".
                format(x[i],y[i],xi,yi,y1,y2,sinc(xi)))
   else:
      out.write("{0:36.3e}{1:12.3e}{2:12.3e}{3:12.3e}{4:12.3e}\n".
                format(xi,yi,y1,y2,sinc(xi)))
out.close()
