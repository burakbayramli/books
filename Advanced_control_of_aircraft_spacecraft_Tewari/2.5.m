% Calling program for solving the 2PBVP of a second-order
% system d^2y/dt^2=f(t,y,dy/dt),
% by collocation method of MATLAB's intrinsic code 'bvp4c.m'.
% Requires 'ode2ord.m' and 'bc2ord.m'.
% (c) 2009 Ashish Tewari
% Collocation solution with 5 points with t0=0, tf=1:
solinit = bvpinit(linspace(0,1,5),[1 0]); % Initial guess
%(y=1, dy/dt=0)
sol = bvp4c(@ode2ord,@bc2ord,solinit);% Solution by bvp4c.m
t = linspace(0,1); % Time vector
x = deval(sol,t); % Solution state vector, [y, dy/dt]'
% Program for specifying boundary conditions for the 2PBVP.
% (To be called by 'bvp4c.m')
function res=bc2ord(ya,yb)
  % Boundary conditions follow:
  res=[ya(1)-1
  % y(t0)=1
       yb(1)-2]; % y(tf)=2
