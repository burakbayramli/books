% Program for solving 2PBVP for a second-order system
% y''=f(t,y,y'),
% with end conditions on dependent variable,
% y(t0)=a; y(tf)=b,
% by a simple shooting method.
% Requires 'ode2ord.m' for specifying the differential equation
% for integration by MATLAB's RK-4(5) IVP code 'ode45.m'.
% (c) 2009 Ashish Tewari
% Solution vector: Y=[y, dy/dt]'; Time vector: T.
function [T,Y]=bvp2ordshoot(t0,tf,a,b)
c=-4; % Constant for shooting algorithm
tol=1e-6; % Terminal error tolerance
yp=(b-a)/(tf-t0) % Initial guess of dy/dt(0)
% Initial value problem solution by 'ode45.m'
[T,Y]=ode45('ode2ord',[t0 tf],[a yp]');
n=size(T,1);
y1=Y(n,1)
y1dot=Y(n,2)
res=abs(y1-b)
i=1;
% Iteration for terminal error follows:
while res>tol
i=i+1
yp=c*(y1-b)+yp
T=[];Y=[];
[T,Y]=ode45('ode2ord',[t0 tf],[a yp]');
n=size(T,1);
y1=Y(n,1)
y1dot=Y(n,2)
res=abs(y1-b)
end
% Program for specifying the differential equation
% for integration by MATLAB's RK-4(5) IVP code 'ode45.m'.
function dydx=ode2ord(x,y)
dydx=[y(2)
      -4*y(1)^2];
