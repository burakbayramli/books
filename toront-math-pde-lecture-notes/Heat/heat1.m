% [u,err,x,t] = heat1(t_0,t_f,M,N)
%
% solves the heat equation u_t = D u_xx on [0,2*pi] with initial data u_0 =
% sin(k x) with periodic boundary conditions using finite-differences in
% space and explicit time-stepping.  t_0 is the initial time, t_f is the
% final time, N is the number of intervals in space, and M is the number of
% intervals in time.  err is the error. 
%
% If I'm going to use the FFT to analyse the solution then I'd like to  take
% N of the form 2^k.  This will also have the advantage of making sure there's
% always a meshpoint at x = pi

function [u,err,x,t] = heat1(t_0,t_f,M,N)

% define the mesh in space
dx = 2*pi/N;
x = 0:dx:2*pi;
x = x';

% define the mesh in time
dt = (t_f-t_0)/M;
t = t_0:dt:t_f;

% define the diffusivity
D = 1/2;

% define the ratio r.  This is lambda in the class notes.  We need
% r < 1/2 for stability
r = D*dt/dx^2;
lambda = r

% choose the wave number of the initial data and give its decay rate
k = 3;
sigma = D*k^2;
% define the initial data:
u(:,1) = sin(k*x);
err(:,1) = u(:,1) - exp(-sigma*(t(1)-t_0))*sin(k*x);

% Unlike in class, the indices go from j=1 to N+1 rather than
% from j=0 to N.  Matlab doesn't allow 0 as an index.  So my
% codes are all slightly different from the class notes in that
% way.
%
% for internal points, have
%    u_new(j) = u_old(j) + r*(u_old(j+1)-2*u_old(j)+u_old(j-1))
% for the two endpoints, have
%    u_new(1) = u_old(1) + r*(u_old(2)-2*u_old(1)+u_old(0))     
%             = u_old(1) + r*(u_old(2)-2*u_old(1)+u_old(N))     
%    u_new(N+1) = u_old(N+1) + r*(u_old(N+2)-2*u_old(N+1)+u_old(N))     
%               = u_old(N+1) + r*(u_old(2)-2*u_old(N+1)+u_old(N))     
%
% clearly one of the endpoints is redundant: u(1)= u(N+1) at all times.  I
% just kept both around for plotting convenience.

for j=1:M
  % left-hand endpoint
  u(1,j+1) = u(1,j) + r*(u(2,j)-2*u(1,j)+u(N,j));
  for i=2:N
    u(i,j+1) = u(i,j) + r*(u(i+1,j)-2*u(i,j)+u(i-1,j));
  end
  u(N+1,j+1) = u(N+1,j) + r*(u(2,j)-2*u(N+1,j)+u(N,j));
  err(:,j+1) = u(:,j+1) - exp(-sigma*(t(j+1)-t_0))*sin(k*x);
end

