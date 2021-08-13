function [u,t]=euler(f,t0,tf,u0,n,varargin)

%[u,t]=euler(f,t0,tf,u0,n,p1,p2,...)
%
%   This function implements Euler's method for solving the IVP
%
%         du/dt=f(t,u), u(t0)=u0
%
%   on the interval [t0,tf].  n steps of Euler's method are taken;
%   the step size is dt=(tf-t0)/n.
%
%   If the optional arguments p1,p2,... are given, they are passed
%   to the function f.

% Figure out the dimension of the system by examining the initial value:

k=length(u0);

% Compute the grid and allocate space for the solution

t=linspace(t0,tf,n+1);
u=zeros(k,n+1);

% Assign the initial value and compute the time step

u(:,1)=u0;
dt=(tf-t0)/n;

% Now do the computations in a loop

for ii=1:n
   u(:,ii+1)=u(:,ii)+dt*f(t(ii),u(:,ii),varargin{:});
end
