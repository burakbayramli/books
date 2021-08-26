function [du] = BurgersMrhs1D(x,u,h,k,maxvel)
% function [du] = BurgersMrhs1D(x,u,h,k,maxvel);
% Purpose: Evaluate right hand side for Burgers equation 
% using monotone method
N = length(x);

[xe,ue] = extend(x,u,h,1,'P',0,'P',0); % Periodic boundary conditions
%[xe,ue] = extend(x,u,h,1,'D',2,'N',0); % Constant boundary conditions

% Change numerical flux here
du = - (BurgersLF(ue(2:N+1),ue(3:N+2),0,maxvel) - ...
           BurgersLF(ue(1:N),ue(2:N+1),0,maxvel))/h;
return