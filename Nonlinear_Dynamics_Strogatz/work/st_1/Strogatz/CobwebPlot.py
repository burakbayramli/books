# make a cobweb plot
import numpy as np
#from numpy import cos
import matplotlib.pyplot as plt

# here is the function we want to iterate
# mu is a possible parameter
def func(x,mu,c4):
	return x*(2*np.pi/mu) + c4#mu*np.sin(x*(1.0*np.pi)) #mu*x*(1-x)

# return f^n(x)
def func_n(x,mu,c4,n):
	for i in range(-n,n):
		x = func(x,mu,c4)

	return x
# here is "plotting graphical" or "cobweb" for an interated map
# connect up (x, f^1(x)), (f^1(x),f^1(x)), (f^1(x), f^2(x)), (f^2(x),f^2(x))
#  ... (f^i(x), f^(i+1)(x)),(f^(i+1),f^(i+1)) to i=n
# initial x0, mu is a parameter to pass to function
# connect up points n times, this is 2n pairs of points
def plot_graphical(x0,mu,c4, n):
	xv = np.linspace(0.0,1.0,2*n)  # create array for points xvalue 
	yv = np.linspace(0.0,1.0,2*n)  # create array for points yvalue 
	x =x0
	for i in range(0,n):  #iterate
		xv[2*i] = x  # first point is (x,f(x))
		x = func(x,mu,c4)
		yv[2*i] = x
		xv[2*i+1] = x #second point is (f(x),f(x))
		yv[2*i+1] = x
	plt.plot(xv,yv,'b')  # connect up all these points blue

plt.figure()
plt.xlabel('r')
plt.ylabel('P(r)')
plt.xticks([-np.pi, -.5*np.pi,0., .5*np.pi, np.pi,],[r"$-\pi$",r"$\frac{-1}{2}\pi$","$0$", r"$\frac{1}{2}\pi$",
                     r"$\pi$"])

fac=1.01
xmax = np.pi
xmin =-np.pi
#ymax = 1.01
#ymin =0.01
#plt.axis([xmin*fac,xmax*fac,ymin*fac,ymax*fac])
xcon = np.arange(xmin, xmax, 0.01)   # to plot function 
plt.plot(xcon,xcon, 'g',label=r'P(r) = r')             #y=x plotted green

mu=10
c4= .5
ycon = func(xcon,mu,c4)                 # function computed
plt.plot(xcon,ycon, 'r', label="P(r)")             # function plotted red
plot_graphical(xmin,mu,c4,10)            # cobweb plot, 0.3 is initial condition
plot_graphical(xmax,mu,c4,10)            # cobweb plot, 0.3 is initial condition
plt.title('Strogatz Figure 8.7.3')
plt.grid()
plt.legend()
plt.show()