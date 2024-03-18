% test prlng & rstrct
close all
clear all
format compact
n  = 10
n2 = 2*(n-3)+3
dx = 1/(n-3);
x = dx*(-1:n-2)';
c = 3.14156
f = max(sin(c*pi*x),0);
f = (x > 0.29);
dx2 = 1/(n2-3);
x2 = dx2*(-1:n2-2)';
f2 = max(sin(c*pi*x2),0);
f2 = (x2 > 0.29);
y  = rstrct(f2);
y2 = prlng(f);
subplot(121)
plot(x,f,'.-k');
hold on
plot(x,y,'.-r')
subplot(122)
plot(x2,f2,'.-k')
hold on
plot(x2,y2,'.-r')
