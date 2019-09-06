% Calling program for solving the 2PBVP for
% flight with modulated fwd and normal
% acceleration inputs by collocation method
% using MATLAB's intrinsic code 'bvp4c.m'.
% Requires 'normballisticode.m' and
% 'normballisticbc.m'.
% (c) 2010 Ashish Tewari
% dy/dx=f(y,x); a<=x<=b
% y(x=a), y(x=b): Boundary conditions
% y(1,1)=delta (rad)
% y(2,1)=r (km)
% y(3,1)=v (km/s)
% y(4,1)=phi (rad)
% y(5:8,1)=Lagrange multipliers
global R1; R1=1;
global R2; R2=7;
global tf; tf=500;
dtr=pi/180;
init=[30*dtr 6580 8 5*dtr 0 0 0 0];
solinit = bvpinit(linspace(0,tf,5),init);
sol = bvp4c(@normballisticode,@normballisticbc,solinit);
x = linspace(0,tf);
y = deval(sol,x);
u1=-0.5*y(7,:)/R1;
u2=-0.5*y(8,:)./(R2*y(3,:));
