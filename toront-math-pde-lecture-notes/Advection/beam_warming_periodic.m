% [u,err,x,t] = beam_warming_periodic(@f,t_0,t_f,M,N)
%
% solves the heat equation u_t + u_x = 0 [0,2*pi] 
%
% If I'm going to use the FFT to analyse the solution then I'd like to  take
% N of the form 2^k.  This will also have the advantage of making sure there's
% always a meshpoint at x = pi
%
% f is the function that contains the initial data

function [u,u_exact,x,t] = beam_warming_periodic(f,t_0,t_f,M,N)

% define the mesh in space
dx = 2*pi/N;
x = 0:dx:2*pi;
x = x';

% define the mesh in time
dt = (t_f-t_0)/M;
t = t_0:dt:t_f;

c = 1/2;
% display('this number should be between 0 and 2 for stability:')
mu = c*dt/dx;

% choose the wave number of the initial data and give its decay rate
u = zeros(N+1,M+1);
u(:,1) = f(x);
u_exact(:,1) = u(:,1);

% I want to do the beam warming scheme:
%
% u_new(j) = u_old(j) + (mu/2)*(-u_old(j-2)+4*u_old(j-1)-3*u_old(j)) 
%                  + (mu^2/2)*(u_old(j-2)-2*u_old(j-1)+u_old(j))

for j=1:M
    for k=3:N+1
        u(k,j+1) = u(k,j) + (mu/2)*(-u(k-2,j)+4*u(k-1,j)-3*u(k,j)) + (mu^2/2)*(u(k-2,j)-2*u(k-1,j)+u(k,j));
    end
    % I code in the exact values at the endpoints.
    k = 1;
    % u(k-2) = u(-1) = u(N-1)
    % u(k-1) = u(0) = u(N)
    u(k,j+1) = u(k,j) + (mu/2)*(-u(N-1,j)+4*u(N,j)-3*u(k,j)) + (mu^2/2)*(u(N-1,j)-2*u(N,j)+u(k,j));
    k = 2;
    % u(k-2) = u(0) = u(N)
    u(k,j+1) = u(k,j) + (mu/2)*(-u(N,j)+4*u(k-1,j)-3*u(k,j)) + (mu^2/2)*(u(N,j)-2*u(k-1,j)+u(k,j));
    X = x-c*t(j+1);
    u_exact(:,j+1) = f(X);
end
