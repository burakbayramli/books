%do_lagnewch.m
clear
N=4; k=[0: N];
x=cos((2*N+1-2*k)*pi/2/(N+1))
y=f1(x)
n=newtonp(x,y) %n=lagranp(x,y)
xx=[-1: 0.02 : 1];
yy1=polyval(n,xx);
clf
subplot(121)
yy=f1(xx);
plot(xx,yy,'k'), axis([-1 1 -0.5 1.5])
hold on
plot(xx,yy1,'r', x,y,'ro')
subplot(122)
plot(xx,yy1-yy,'r'), axis([-1 1 -0.4 0.4])
hold on
N=8; k=[0: N];
x=cos((2*N+1-2*k)*pi/2/(N+1))
y=f1(x)
n=newtonp(x,y) %n=lagranp(x,y)
yy1=polyval(n,xx);
subplot(121)
plot(xx,yy1,'b', x,y,'b^')
subplot(122), plot(xx,yy1-yy,'b')
N=10; k=[0: N];
x=cos((2*N+1-2*k)*pi/2/(N+1))
y=f1(x)
n=newtonp(x,y) %n=lagranp(x,y)
yy1=polyval(n,xx);
subplot(121)
plot(xx,yy1,'m', x,y,'m+')
subplot(122), plot(xx,yy1-yy,'m')