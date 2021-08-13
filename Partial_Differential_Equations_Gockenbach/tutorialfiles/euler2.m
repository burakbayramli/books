function [u,t]=euler2(f,t0,tf,u0,n)

%[u,t]=euler2(f,t0,tf,u0,n)
%
%   This function implements Euler's method for solving the IVP
%
%         du/dt=f(t,u), u(t0)=u0
%
%   on the interval [t0,tf].  n steps of Euler's method are taken;
%   the step size is dt=(tf-t0)/n.
%
%   The solution u can be a scalar or a vector.  In the vector case,
%   the initial value must be a kx1 vector, and the function f must
%   return a kx1 vector.

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
   u(:,ii+1)=u(:,ii)+dt*f(t(ii),u(:,ii));
end
