% [u,x,t] = conservative_explicit_upwind(@ID,@F,t_0,t_f,M,N,BCs)
%
% equation u_t + (f(u))_x = 0 [-pi,pi] with initial data given
% by ID.m
%
% The initial data is in ID.m  If BCs == 1 then I assume the 
% initial data equals 1 at the left and 0 at the right.  Otherwise
% I assume the initial data equal -1 at the left and 0 at the right.
%
function [u,x,t] = conservative_explicit_upwind(ID,F,t_0,t_f,M,N,BCs)

% define the mesh in space
dx = 2*pi/N;
x = -pi:dx:pi;
x = x';

% define the mesh in time
dt = (t_f-t_0)/M;
t = t_0:dt:t_f;

% choose the wave number of the initial data and give its decay rate
% u = zeros(N+1,M+1);
% u(:,1) = ID(x);
u = ID(x);

for j=1:M
    %     for k=2:N
    %         u(k,j+1) = u(k,j)-dt/dx*(F(u(k,j),u(k+1,j),dt,dx)-F(u(k-1,j),u(k,j),dt,dx));
    %     end
%     FF = F(u(1:N,j),u(2:N+1,j),dt,dx);
%     u(2:N,j+1) = u(2:n,j) - dt/dx*(FF(2:N)-FF(1:N-1));
    FF = F(u(1:N),u(2:N+1),dt,dx);
    u(2:N) = u(2:N) - dt/dx*(FF(2:N)-FF(1:N-1));

    % I code in the exact values at the left endpoint.
    if BCs == 1
      u(1)=1;
      u(N+1)=0;
    else
      u(1)=-1;
      u(N+1)=1;
    end
end
