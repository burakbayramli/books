function [du] = BurgersWENOrhs1D(x,u,h,k,m,Crec,dw,beta,maxvel)
% function [du] = BurgersWENOrhs1D(x,u,h,k,m,Crec,dw,beta,maxvel)
% Purpose  : Evaluate the RHS of Burgers equations using a WENO reconstruction

N = length(x); du = zeros(N,1);

% Extend data and assign boundary conditions
[xe,ue] = extend(x,u,h,m,'P',0,'P',0);

% define cell left and right interface values
ul = zeros(N+2,1); ur = zeros(N+2,1);

for i=1:N+2
  [ul(i),ur(i)] = WENO(xe(i:(i+2*(m-1))),ue(i:(i+2*(m-1))),m,Crec,dw,beta);
end;

% Compute residual
du = - (BurgersLF(ur(2:N+1),ul(3:N+2),0,maxvel) - ...
           BurgersLF(ur(1:N),ul(2:N+1),0,maxvel))/h;
return