function [du] = LinwaveFLrhs1D(x,u,h,k,maxvel)
% function [du] = LinwaveFLrhs1D(x,u,h,k,maxvel);
% Purpose: Evaluate right hand side for linear wave equation using a
% flux limited scheme
N = length(x);

% Chose flux limiter - 0:LF; 1:CO; 2:Koren; 3:Sweby; 4:OSPRE; 5:van Leer 
type = 5; beta=1.5; 

% Periodic boundary conditions
[xe,ue] = extend(x,u,h,1,'P',0,'P',0);

% Compute indicator function and define flux limiter
r = (ue(2:N+1) - ue(1:N))./(ue(3:N+2)-ue(2:N+1));
[xe,re] = extend(x,r,h,1,'N',0,'N',0);
phiL = FluxLimit(re(1:N),type,beta); 
phiR = FluxLimit(re(2:N+1),type,beta);

% Compute left flux in cell - Change numerical flux here
Fluxlow = LinwaveLF(ue(1:N),ue(2:N+1),k/h,maxvel); 
Fluxhigh = LinwaveLW(ue(1:N),ue(2:N+1),k/h,maxvel); 
FluxL = Fluxlow + phiL.*(Fluxhigh - Fluxlow);

% Compute right flux in cell - Change numerical flux here
Fluxlow = LinwaveLF(ue(2:N+1),ue(3:N+2),k/h,maxvel); 
Fluxhigh = LinwaveLW(ue(2:N+1),ue(3:N+2),k/h,maxvel); 
FluxR = Fluxlow + phiR.*(Fluxhigh - Fluxlow);

% Compute RHS 
du = -(FluxR - FluxL)/h;
return