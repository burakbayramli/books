function [du] = BurgersCrhs1D(x,u,h,k,maxvel)
% function [du] = BurgersCrhs1D(x,u,h,k,maxvel);
% Purpose: Evaluate right hand side for Burgers equation using second order
% central scheme - NOTE two steps of k/2 is taken
N = length(x); Ns = N-1; kl=k/2; us = zeros(Ns,1); uns = zeros(N,1);

% Chose slope limiter - 0:LF; 1:minmod; 2:MUSCL; 3:Superbee; 
% 4:van Albada; 5:van Leer, 6: TVB 
type = 2; c=0; M=10;

% First step from non-staggered to staggered grid
[xe,ue] = extend(x,u,h,2,'P',0,'P',0); % Periodic BC
%[xe,ue] = extend(x,u,h,2,'D',2,'N',0); % Constant BC

% Compute element slope and limit 
dup = ue(3:N+4)-ue(2:N+3); dum = ue(2:N+3) - ue(1:N+2);
duL = SlopeLimit(dup,dum,type,c,M,h);

% Compute limited intermediate solution - f'(u)=2*u
uh = ue(2:N+3) - BurgersJac(ue(2:N+3)).*kl/(2*h).*duL; 

% Advance solition k/2
us = (u(1:N-1)+u(2:N))/2 + (duL(2:N)-duL(3:N+1))/8 ...
        - kl/h*(BurgersFlux(uh(3:N+1))-BurgersFlux(uh(2:N)));

% Second step from staggered to non-staggered grid
[xe,ue] = extendstag(x(1:Ns)+h/2,us,h,2,'P',0,'P',0); % Periodic BC
%[xe,ue] = extendstag(x(1:Ns)+h/2,us,h,2,'D',2,'N',0); % Constant BC

% Compute element slope and limit 
dup = ue(3:Ns+4)-ue(2:Ns+3); dum = ue(2:Ns+3) - ue(1:Ns+2);
duL = SlopeLimit(dup,dum,type,c,M,h);

% Compute limited intermediate solution - f'(u)=2*u
uh = ue(2:Ns+3) - BurgersJac(ue(2:Ns+3)).*kl/(2*h).*duL; 
 
% Advance solition k/2
uns = (ue(2:Ns+2)+ue(3:Ns+3))/2 + (duL(1:Ns+1)-duL(2:Ns+2))/8 ...
        - kl/h*(BurgersFlux(uh(2:Ns+2))-BurgersFlux(uh(1:Ns+1)));

% Restore residual
du = (uns-u)/k;
return