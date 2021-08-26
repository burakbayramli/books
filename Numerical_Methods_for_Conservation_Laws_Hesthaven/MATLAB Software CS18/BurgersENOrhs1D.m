function [du] = BurgersENOrhs1D(x,u,h,k,m,Crec,maxvel)
% function [du] = BurgersENOrhs1D(x,u,h,k,m,Crec,maxvel)
% Purpose  : Evaluate the RHS of Burgers equations using 
%            an ENO reconstruction
N = length(x); du = zeros(N,1);

% Extend data and assign boundary conditions
[xe,ue] = extend(x,u,h,m,'P',0,'P',0);

% define cell left and right interface values
um = zeros(N+2,1); up = zeros(N+2,1);

for i=1:N+2
  [um(i),up(i)] = ENO(xe(i:(i+2*(m-1))),ue(i:(i+2*(m-1))),m,Crec);
end;

% Compute residual
du = - (BurgersLF(up(2:N+1),um(3:N+2),0,maxvel) ...
          - BurgersLF(up(1:N),um(2:N+1),0,maxvel))/h;
return