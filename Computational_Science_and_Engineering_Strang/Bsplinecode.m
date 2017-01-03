%3.2  Bsplinecode.m

x = 0:4; y = [0 1 4 1 0]/6; yB = spline(x,[0 y 0]);
xx = linspace(0,4,101); plot(x,y,'o',xx,ppval(yB,xx),'-');


