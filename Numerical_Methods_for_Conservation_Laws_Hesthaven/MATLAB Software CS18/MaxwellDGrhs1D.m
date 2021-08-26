function [rhsq] = MaxwellDGrhs1D(x,q,ep,mu,h,k,m,N,Ma,S,VtoE,maxvel); 
% function [dq] = MaxwellDGrhs1D(x,q,ep,mu,h,k,m,Ma,Sr,VtoE,maxvel); 
% Purpose: Evaluate right hand side for Maxwells equation using DG method
Imat = eye(m+1);Ee = zeros(2,N+2); He = zeros(2,N+2);
EMl = zeros(N,2); EMr = zeros(N,2); EMm = zeros(N,2); EMp = zeros(N,2);

% Impose boundary conditions
[Ee] = extendDG(q(VtoE,1),'D',0,'D',0);
[He] = extendDG(q(VtoE,2),'N',0,'N',0);

% Compute numerical fluxes at interfaces
EMr = [Ee(2,2:N+1)' He(2,2:N+1)']; EMl = [Ee(1,2:N+1)' He(1,2:N+1)'];
EMm = [Ee(2,1:N)' He(2,1:N)']; EMp = [Ee(1,3:N+2)' He(1,3:N+2)'];
fluxr = MaxwellLF(EMr,EMp,ep(1,:)',mu(1,:)',k/h,maxvel)';
fluxl = MaxwellLF(EMm,EMl,ep(1,:)',mu(1,:)',k/h,maxvel)';

% Compute right hand side of Maxwell's equation
rE = S'*H./ep - (Imat(:,m+1)*fluxr(1,:) - Imat(:,1)*fluxl(1,:));
rH = S'*E./mu - (Imat(:,m+1)*fluxr(2,:) - Imat(:,1)*fluxl(2,:));
rhsq = (h/2*Ma)\[rE rH];
return