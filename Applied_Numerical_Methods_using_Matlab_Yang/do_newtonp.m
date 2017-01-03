%do_newtonp.m
x=[-2 -1 1 2 4]; y=[-6 0 0 6 60];
[n,DD]=newtonp(x,y)
n0=lagranp(x,y) %for comparison
x=[1 2 4 -1 -2]; y=[0 6 60 0 -6];
[n1,DD]=newtonp(x,y)
xx=[-2: 0.02 : 2]; yy=polyval(n,xx);
clf, plot(xx,yy,'b-',x,y,'*')