function [du] = LinwaveMrhs1D(x,u,h,k,maxvel)
% function [du] = LinwaveMrhs1D(x,u,h,k,maxvel);
% Purpose: Evaluate right hand side for linear wave equation 
% using a monotone method

N = length(x);
% Periodic boundary conditions
[xe,ue] = extend(x,u,h,1,'P',0,'P',0);

% Compute RHS - Change numerical flux here
du = -(LinwaveLF(ue(2:N+1),ue(3:N+2),k/h,maxvel) - ...
           LinwaveLF(ue(1:N),ue(2:N+1),k/h,maxvel))/h;
return