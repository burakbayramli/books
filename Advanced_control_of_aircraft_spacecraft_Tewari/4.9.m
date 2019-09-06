% Calling program for solving the gravity turn,
% constant fwd acceleration 2PBVP by collocation
% method using MATLAB's intrinsic code 'bvp4c.m'.
% Requires 'constuode.m' and 'constubc.m'.
% (c) 2009 Ashish Tewari
% dy/dx=f(y,x); a<=x<=b
% y(x=a), y(x=b): Boundary conditions
% y(1,1)=r (km)
% y(2,1)=v (km/s)
% y(3,1)=phi (rad)
global mu; mu=398600.4; % Grav. const. (km^3/s^2)
global r0; r0=6378.14; % Planetary radius (km)
global R; R=3.6633e6; % Control cost coefficient
global tf; tf=562; % Terminal time (s)
% Collocation points & initial guess follow:
solinit = bvpinit(linspace(0,tf,5),[6680 8 0.1]);
% 2PBVP Solution by collocation method:
sol = bvp4c(@constuode,@constubc,solinit);
x = linspace(0,tf); % Time vector (s)
y = deval(sol,x); % Solution state vector

% Program for specifying governing state equations
% for the 2PBVP (to be called by 'bvp4c.m')
function dydx=constuode(x,y)
global tf;
global mu;
global r0;
global R;
if y(1)>=r0
if x<tf
u=sqrt(2*tf/R);
else
u=0;
end
% State equations follow:
dydx=[y(2)*sin(y(3))
u-mu*sin(y(3))/y(1)^2
(y(2)-mu/(y(1)*y(2)))*cos(y(3))/y(1)];
else
dydx=[0 0 0]';
end

% Program for specifying boundary conditions for the 2PBVP.
% (To be called by 'bvp4c.m')
function res=constubc(ya,yb)
global mu;
global r0;
% Boundary conditions follow:
res=[ya(1)-r0
     ya(2)-0.001
     yb(3)];
