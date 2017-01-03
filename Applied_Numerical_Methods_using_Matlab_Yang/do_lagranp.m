%do_lagranp.m
x=[-2 -1 1 2]; y=[-6 0 0 6]; % given data points
l=lagranp(x,y) % find the Lagrange polynomial
xx=[-2: 0.02 : 2]; %interpolate for [-2,2]
yy=polyval(l,xx);
clf, plot(xx,yy,'b', x,y,'*') %plot the graph