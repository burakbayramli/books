t=0:0.001:5;
Ny=length(t);
z1=sin(pi*t);
z2=sin(2*pi*t);
z3=sin(6*pi*t);
ns=0.01*randn(1,Ny);
y=0.5*t+z1+z2+z3+ns;


figure(1)
subplot(3,1,1)
plot(t,y-z3,'k')
title('1st residue');
subplot(3,1,2)
plot(t,z2,'k')
axis([0 5 -5 5]);
title('2nd IMF');
subplot(3,1,3)
plot(t,y-z2-z3,'k');
title('2nd residue')

