% Calling program for solving the 2PBVP for optimal spin maneuver
% by collocation method by MATLAB's intrinsic code 'bvp4c.m'.
% Requires 'spinode.m' and 'spinbc.m' .
% (c) 2009 Ashish Tewari
% dy/dx=f(y,x); a<=x<=b
% y(x=a), y(x=b): Boundary conditions
% y(1:3,1)=omega (rad/s) (Angular velocity vector)
% y(4:6,1)=lambda (Lagrange multipliers vector)

global tf; tf=50; % Terminal time (s)
global J; J=[1000 1500 2000]; % Moms. of inertia (kg-m^2)
global wi; wi=[0 0 0]; % Initial ang. velocity (rad/s)
global wf; wf=[0.1 -0.1 0.3]; % Final ang. velocity (rad/s)
global R; R=[1/J(1) 1/J(2) 1/J(3)];% Control cost coefficients
% Collocation points & initial guess follow:
solinit = bvpinit(linspace(0,tf,50),[wf 0 0 0]);
% 2PBVP Solution by collocation method:
sol = bvp4c(@spinode,@spinbc,solinit);
x = linspace(0,tf); % Time vector (s)
y = deval(sol,x); % Solution state vector
u=-inv(diag(R))*inv(diag(J))*y(4:6,:)/2; % Control vector (N-m)
% Program for specifying governing ODEs expressed as
% state equations for the 2PBVP (to be called by 'bvp4c.m')
function dydx=spinode(x,y)
global tf;
global J;
global R;
if x<tf
u1=-y(4)/(2*R(1)*J(1));
u2=-y(5)/(2*R(2)*J(2));
u3=-y(6)/(2*R(3)*J(3));
else
u1=0; u2=0; u3=0;
end
jx=(J(2)-J(3))/J(1);
jy=(J(3)-J(1))/J(2);
jz=(J(1)-J(2))/J(3);
dydx=[jx*y(2)*y(3)+u1/J(1)
jy*y(1)*y(3)+u2/J(2)
jz*y(1)*y(2)+u3/J(3)
-jy*y(3)*y(5)-jz*y(2)*y(6)
-jx*y(3)*y(4)-jz*y(1)*y(6)
-jx*y(2)*y(4)-jy*y(1)*y(5)];
% Program for specifying boundary conditions for the 2PBVP.
% (To be called by 'bvp4c.m')
function res=spinbc(ya,yb)
global wi;
global wf;
res=[ya(1)-wi(1)
ya(2)-wi(2)
ya(3)-wi(3)
yb(1)-wf(1)
yb(2)-wf(2)
yb(3)-wf(3)];
