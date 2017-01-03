%do_newtonp1.m
clear, clf
x=[-1 -0.5  0  0.5  1];
y=f1(x)
n=newtonp(x,y)
xx=[-1: 0.02 : 1];
yy1=polyval(n,xx);
subplot(121)
yy=f1(xx);
plot(xx,yy,'k'), axis([-1 1 -0.4 1.2])
hold on
plot(xx,yy1,'r', x,y,'ro')
subplot(122) 
plot(xx,yy1-yy,'r'), axis([-1 1 -0.8 0.8])  
hold on
x=[-1 :0.25: 1];
y=f1(x)
n=newtonp(x,y)
yy1=polyval(n,xx);
subplot(121)
plot(xx,yy1,'b', x,y,'b^')
subplot(122)
plot(xx,yy1-yy,'b')
x=[-1: 0.2: 1];
y=f1(x)
n=newtonp(x,y)
yy1=polyval(n,xx);
subplot(121)
plot(xx,yy1,'m', x,y,'m+')
subplot(122)
plot(xx,yy1-yy,'m')