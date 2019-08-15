% TEST fuer Pendelgleichung
% Euler-Lagrange-Gleichungen fuer Pendel
% ergeben KEINE Loesung TODO--TODO

t0  = 0; t_end = 10;
x0  = [0;-2;1;0];
g = 9.81; m = 1; L = 2;
Parmeter = [g,m,L];           % Parameter fuer DGl
options = odeset('Reltol',1E-7,'Maxstep',0.001);
[T,Y]   = ode23(@bsp05,[t0, t_end],x0,options,Parmeter);
clf
plot(Y(:,1),'k'), hold on
plot(Y(:,2),'r'), hold on
