function ddot = TrussODE(t, d)
% ddot = TrussODE(t, d)
% function to set up equations for a transient truss problem
global Mf Kf Rf
ft = 100*cos(7*pi*t);
n=length(d);
u = d(1:n/2);
v = d(n/2+1:n); 
vdot = inv(Mf)*(Rf*ft - Kf*u);
udot = v;
% format short g
% soln=[t, ft, u(1), udot(1), vdot(1)]
ddot = [udot; vdot];