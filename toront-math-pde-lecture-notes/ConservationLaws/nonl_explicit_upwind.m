% [u,err,xi,x,t] = nonl_explicit_upwind(t_0,t_f,M,N)
%
% solves burger's equation u_t + u u_x = 0 [0,2*pi] with step
% function initial data.  xi(t) is the location of the shock at
% time t.
% 
% This scheme doesn't respect the conservation law and does not converge
% to a weak solution.
function [u,u_exact,xi,x,t] = nonl_explicit_upwind(t_0,t_f,M,N)

% define the mesh in space
dx = 2*pi/N;
x = 0:dx:2*pi;
x = x';

% define the mesh in time
dt = (t_f-t_0)/M;
t = t_0:dt:t_f;

% choose the wave number of the initial data and give its decay rate
u = zeros(N+1,M+1);
u(:,1) = 6/5*(x<=pi/2) + 2/5*(x>pi/2);
xi(1) = find_thresh(x,u(:,1),(2/5+6/5)/2);
u_exact(:,1) = u(:,1);

% I want to do the stable forward-time, forward space scheme:
%
% u_new(j) = u_old(j) - dt/dx*u_old(j)*(u_old(j)-u_old(j-1))

for j=1:M
    for k=2:N+1
        u(k,j+1) = u(k,j)-dt/dx*u(k,j)*(u(k,j)-u(k-1,j));
    end
    % I code in the exact values at the left endpoint.
    u(1,j+1)=6/5;
    xi(j+1) = find_thresh(x,u(:,j+1),(2/5+6/5)/2);
    X = x-5/4*t(j+1);
    u_exact(:,j+1) = 6/5*(X<=pi/2) + 2/5*(X>pi/2);
end
