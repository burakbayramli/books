% [u,err,x,t] = heat_cn(t_0,t_f,M,N)
%
% this solves the heat equation u_t = D u_xx with initial data u_0 =
% sin(k x) with periodic boundary conditions using finite-differences in
% space and Crank-Nicolson time-stepping.  t_0 is the initial time, t_f is the
% final time, N is the number of intervals in space, and M is the number of
% intervals in time.  err is the error. 
%
% If I'm going to use the FFT to analyse the solution then I'd like to  take
% N of the form 2^k.  This will also have the advantage of making sure there's
% always a meshpoint at x = pi

function [u,err,x,t] = heat_cn(t_0,t_f,M,N)

% define the mesh in space
dx = 2*pi/N;
x = 0:dx:2*pi-dx;
x = x';

% define the mesh in time
dt = (t_f-t_0)/M;
t = t_0:dt:t_f;

% define the diffusivity
D = 1/2;

% define the ratio r
r = D*dt/dx^2 

% choose the wave number of the initial data and give its decay rate
k = 3;
sigma = D*k^2;
% define the initial data:
u(:,1) = sin(k*x);
err(:,1) = u(:,1) - exp(-sigma*(t(1)-t_0))*sin(k*x);

% define the matrix A which has to be inverted at every time-step
% because I know that the boundary conditions are periodic u(N+1) = u(1)
% and so I won't actually solve for u(N+1) below, I'll just assign it 
% a value once I know u(1)
%
% for internal points, have
%    u_new(j) = u_old(j) + r/2*(u_old(j+1)-2*u_old(j)+u_old(j-1))
% for the two end-points, have
%    u_new(1) = u_old(1) + r/2*(u_old(2)-2*u_old(1)+u_old(0))     
%             = u_old(1) + r/2*(u_old(2)-2*u_old(1)+u_old(N))     
%    u_new(N) = u_old(N) + r/2*(u_old(N+1)-2*u_old(N)+u_old(N-1))     
%             = u_old(N) + r/2*(u_old(1)-2*u_old(N)+u_old(N-1))     
%
% define the matrix A which has to be inverted at every time-step.
%   u_new(1) - u_old(1) = dt/h^2 (u_new(2)-2*u_new(1)+u_new(N)
%   u_new(i) - u_old(i) = dt/h^2 (u_new(i+1)-2*u_new(i)+u_new(i-1)
%   u_new(N) - u_old(N) = dt/h^2 (u_new(2)-2*u_new(N)+u_new(N-1)

A(1,N) = -r/2;
A(1,1) = 1+r;
A(1,2) = -r/2;
for j=2:N-1
  A(j,j-1) = -r/2;
  A(j,j) = 1+r;
  A(j,j+1) = -r/2;
end
A(N,N-1) = -r/2;
A(N,N) = 1+r;
A(N,1) = -r/2;

% Now that I have the matrix A defined I'm ready to do the timestepping.
% This is because the matrix doesn't depend on time --- I can simply define
% it at the beginning.
for j=1:M
  u_now = u(:,j);
  RHS(1) = u_now(1) + r/2*(u_now(2)-2*u_now(1)+u_now(N));
  for i=2:N-1
    RHS(i) = u_now(i) + r/2*(u_now(i+1)-2*u_now(i)+u_now(i-1));
  end
  RHS(N) = u_now(N) + r/2*(u_now(1)-2*u_now(N)+u_now(N-1));
  u(1:N,j+1) = periodic_tridiag(A,RHS);  

  err(:,j+1) = u(:,j+1) - exp(-sigma*(t(j+1)-t_0))*sin(k*x);
end
% impose periodicity
u(N+1,:)=u(1,:);
err(N+1,:)=err(1,:);
x(N+1)=2*pi;
