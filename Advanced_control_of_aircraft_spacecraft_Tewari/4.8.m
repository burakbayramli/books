% Calling program for solving the gravity turn 2PBVP
% by collocation method by MATLAB's intrinsic code 'bvp4c.m'.
% Requires 'fiveode.m' and 'fivebc.m'.
% (c) 2009 Ashish Tewari
% dy/dx=f(y,x); a<=x<=b
% y(x=a), y(x=b): Boundary conditions
% y(1,1)=r (km)
% y(2,1)=v (km/s)
% y(3,1)= phi (rad)
% y(4,1)=lambda_2 (Lagrange multiplier of r)
% y(5,1)=lambda_3 (Lagrange multiplier of v)
global mu; mu=398600.4; % Grav. const. (km^3/s^2)
global r0; r0=6378.14; % Planetary radius (km)
global R;
R=1; % Control cost coefficient
tf=600; % Terminal time (s)
% Collocation points & initial guess follow:
solinit = bvpinit(linspace(0,tf,5),[6680 8 0.1 0 0]);
% 2PBVP Solution by collocation method:
sol = bvp4c(@fiveode,@fivebc,solinit);
x = linspace(0,tf); % Time vector (s)
y = deval(sol,x); % Solution state vector

% Program for specifying governing ODEs expressed as
% state equations for the 2PBVP (to be called by 'bvp4c.m')
function dydx=fiveode(x,y)
global mu;
global r0;
global R;
% State equations follow:
dydx=[y(2)*sin(y(3))
-0.5*y(5)/R-mu*sin(y(3))/y(1)^2
(y(2)-mu/(y(1)*y(2)))*cos(y(3))/y(1)
-2*mu*y(5)*sin(y(3))/y(1)^3
-y(4)*sin(y(3))];
% Program for specifying boundary conditions for the 2PBVP.
% (To be called by 'bvp4c.m')
function res=fivebc(ya,yb)
global mu;
global r0;
% Boundary conditions follow:
res=[ya(1)-r0
ya(2)-0.001
yb(1)-r0-300
yb(2)-sqrt(mu/(r0+300))
yb(3)];
