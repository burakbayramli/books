function [rhsu] = HeatDGrhs1D(x,u,h,m,N,Ma,S,VtoE,nu)
% function [rhsu] = HeatDGrhs1D(u,x,u,h,m,N,Ma,S,VtoE,nu)
% Purpose  : Evaluate RHS flux in 1D heat equation using using a DG method
Imat = eye(m+1); ue = zeros(2,N+2); qe = zeros(2,N+2);

% Extend data and assign boundary conditions
[ue] = extendDG(u(VtoE),'D',0,'D',0);

% Compute numerical fluxes at interfaces
fluxr = HeatFlux(ue(2,2:N+1),ue(1,3:N+2),'C'); 
fluxl = HeatFlux(ue(2,1:N),ue(1,2:N+1),'C');

% Compute aux variable q
qh = S'*u - (Imat(:,m+1)*fluxr(1,:) - Imat(:,1)*fluxl(1,:));
q = nu.*((h/2*Ma)\qh); 

% Extend data and assign boundary conditions
[qe] = extendDG(q(VtoE),'N',0,'N',0);

% Compute numerical fluxes at interfaces
fluxr = HeatFlux(qe(2,2:N+1),qe(1,3:N+2),'C'); 
fluxl = HeatFlux(qe(2,1:N),qe(1,2:N+1),'C');

% Compute aux variable q
rh = S'*q - (Imat(:,m+1)*fluxr(1,:) - Imat(:,1)*fluxl(1,:));
rhsu = (h/2*Ma)\rh; 
return