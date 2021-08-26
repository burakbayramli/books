function [rhsu] = LinwaveDGrhs1D(x,u,h,k,m,N,Ma,S,VtoE,maxvel)
% function [rhsu] = LinwaveDGrhs1D(x,u,h,k,m,N,Ma,S,VtoE,maxvel)
% Purpose  : Evaluate the RHS of the linear wave equations using a DG method
Imat = eye(m+1); ue = zeros(2,N+2);

% Extend data and assign boundary conditions
[ue] = extendDG(u(VtoE),'P',0,'P',0);

% Compute numerical fluxes at interfaces
fluxr = LinwaveLF(ue(2,2:N+1),ue(1,3:N+2),0,maxvel); 
fluxl = LinwaveLF(ue(2,1:N),ue(1,2:N+1),0,maxvel);

% Compute right hand side of Maxwell's equation
ru = S'*u - (Imat(:,m+1)*fluxr(1,:) - Imat(:,1)*fluxl(1,:));
rhsu = (h/2*Ma)\ru;
return