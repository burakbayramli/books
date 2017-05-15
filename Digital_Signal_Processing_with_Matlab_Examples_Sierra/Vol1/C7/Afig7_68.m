t=0:0.001:5;
Ny=length(t);
z1=sin(pi*t);
z2=sin(2*pi*t);
z3=sin(6*pi*t);
ns=0.01*randn(1,Ny);
y=0.5*t+z1+z2+z3+ns;

r1=y-z3;

figure(1)
plot(t,z3,'k'); 
axis([0 5 -5 5]);
title('first IMF')


