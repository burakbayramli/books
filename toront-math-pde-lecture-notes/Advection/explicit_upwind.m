% [u,err,x,t] = explicit_upwind(@f,t_0,t_f,M,N)
%
% solves the heat equation u_t + u_x = 0 [0,2*pi] 
%
% If I'm going to use the FFT to analyse the solution then I'd like to  take
% N of the form 2^k.  This will also have the advantage of making sure there's
% always a meshpoint at x = pi
%
% f is the function that contains the initial data

function [u,u_exact,x,t] = explicit_upwind(f,t_0,t_f,M,N)

% define the mesh in space
dx = 2*pi/N;
x = 0:dx:2*pi;
x = x';

% define the mesh in time
dt = (t_f-t_0)/M;
t = t_0:dt:t_f;

c = 1/2;
% display('this number should be less than 1 for stability:')
mu = c*dt/dx;

% choose the wave number of the initial data and give its decay rate
u = zeros(N+1,M+1);
u(:,1) = f(x);
u_exact(:,1) = u(:,1);

% I want to do the stable forward-time, forward space scheme:
%
% u_new(j) = u_old(j) - r*(u_old(j)-u_old(j-1))

% for j=1:M
%    u(:,j)'
%    u(1:N,j+1) = u(1:N,j)-r*diff(u(:,j));
%    u(N+1,j+1) = 0;
% end

for j=1:M
    for k=2:N+1
        u(k,j+1) = u(k,j)-mu*(u(k,j)-u(k-1,j));
    end
    % I code in the exact values at the left endpoint.
    u(1,j+1)=1;
    X = x-c*t(j+1);
    u_exact(:,j+1) = f(X);
end
