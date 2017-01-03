% [u,err,x,t] = heat_eul_neu(t_0,t_f,D,L,M,N)
%
% solves the heat equation u_t = D u_xx on [0,L] with initial data u_0 =
% cos(k x) with neumann boundary conditions using finite-differences in
% space and explicit time-stepping.  t_0 is the initial time, t_f is the
% final time, N is the number of intervals in space, and M is the number of
% intervals in time.  err is the error. 

function [u,err,x,t] = heat_eul_neu(t_0,t_f,D,L,M,N)

% define the mesh in space
dx = L/N;
x = 0:dx:L;
x = x';

% define the mesh in time
dt = (t_f-t_0)/M;
t = t_0:dt:t_f;

% define the ratio r.  This is lambda in the class notes.  We need
% r < 1/2 for stability
r = D*dt/dx^2

% choose the wave number of the initial data and give its decay rate
k = 3;
sigma = D*(k*pi/L)^2;
% define the initial data:
u(:,1) = cos(k*pi*x/L);
% u(:,1) = zeros(size(x));
% u(N/2+1,1) = 1/dx;
err(:,1) = u(:,1) - exp(-sigma*(t(1)-t_0))*cos(k*pi*x/L);

% for internal points, have
%    u_new(j) = u_old(j) + r*(u_old(j+1)-2*u_old(j)+u_old(j-1))
% for the two endpoints, have
%    u_new(1) = u_old(1) + r*(u_old(2)-2*u_old(1)+u_old(0))     
%    u_new(N) = u_old(N) + r*(u_old(N+1)-2*u_old(N)+u_old(N-1))     
% I need to use the Neumann boundary conditions to specify u_old(0) which
% is to the left of my lefthand endpoint and to specify u_old(N+1) which is
% to the right of my righthand endpoint.
%
% I approximate u_x(0,t) = 0 by (u_old(0)-u_old(2))/(2*dx) = 0 hence 
% I'll take u_old(0) = u_old(2).  Similarly, I take u_old(N+1) = u_old(N-1)
%
%    u_new(1) = u_old(1) + r*(u_old(2)-2*u_old(1)+u_old(2))     
%    u_new(N) = u_old(N) + r*(u_old(N-1)-2*u_old(N)+u_old(N-1))     


for j=1:M
  u(1,j+1) = u(1,j) + r*(u(2,j)-2*u(1,j)+u(2,j));
  for i=2:N
    u(i,j+1) = u(i,j) + r*(u(i+1,j)-2*u(i,j)+u(i-1,j));
  end
  u(N+1,j+1) = u(N+1,j) + r*(u(N,j)-2*u(N+1,j)+u(N,j));
  err(:,j+1) = u(:,j+1) - exp(-sigma*(t(j+1)-t_0))*cos(k*pi*x/L);
end

