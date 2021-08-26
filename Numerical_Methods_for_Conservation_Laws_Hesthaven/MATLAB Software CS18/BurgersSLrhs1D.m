function [du] = BurgersSLrhs1D(x,u,h,k,maxvel)
% function [du] = BurgersSLrhs1D(x,u,h,k,maxvel);
% Purpose: Evaluate right hand side for Burgers equation using slope 
% limited scheme
N = length(x);

% Chose slope limiter - 0:LF; 1:minmod; 2:MUSCL; 3:Superbee; 
% 4:van Albada; 5:van Leer, 6: TVB 
type = 2; c=0; M=10;

% Boundary conditions
[xe,ue] = extend(x,u,h,2,'P',0,'P',0); % Periodic boundary conditions
%[xe,ue] = extend(x,u,h,2,'D',2,'N',0); % Constant boundary conditions

% Compute element slope and limit 
dup = ue(3:N+4)-ue(2:N+3); dum = ue(2:N+3) - ue(1:N+2);
duL = SlopeLimit(dup,dum,type,c,M,h);

% Compute cell interface values - for f'(u) = 2u;
uloc = ue(2:N+3);
uh = uloc - 2*uloc.*k/(2*h).*duL; uL = uh - duL/2; uR = uh + duL/2;

% Compute RHS 
du = -(BurgersLF(uR(2:N+1),uL(3:N+2),0,maxvel) ...
           - BurgersLF(uR(1:N),uL(2:N+1),0,maxvel))/h;
return