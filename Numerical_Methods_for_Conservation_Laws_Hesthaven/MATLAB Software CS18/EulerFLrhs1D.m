function [dq] = EulerFLrhs1D(x,q,gamma,h,k,maxvel)
% function [dq] = EulerFLrhs1D(x,q,gamma,h,k,maxvel);
% Purpose: Evaluate right hand side for Euler equations using a 
% flux limited scheme
N = length(x); 

% Chose flux limiter - 0:LF; 1:CO; 2:Koren; 3:Sweby; 4:OSPRE; 5:van Leer 
type = 5; beta=1; 

% Extend data and assign boundary conditions
[xe,re] = extend(x,q(:,1),h,1,'D',1.0,'D',0.125);
[xe,me] = extend(x,q(:,2),h,1,'D',0,'N',0);
[xe,Ee] = extend(x,q(:,3),h,1,'D',2.5,'N',0);

% [xe,re] = extend(x,q(:,1),h,1,'D',3.857143,'N',0);
% [xe,me] = extend(x,q(:,2),h,1,'D',10.141852,'D',0);
% [xe,Ee] = extend(x,q(:,3),h,1,'D',39.166661,'N',0);

% Compute indicator function and define flux limiter
rr = (re(2:N+1) - re(1:N))./(re(3:N+2)-re(2:N+1)); 
rm = (me(2:N+1) - me(1:N))./(me(3:N+2)-me(2:N+1)); 
rE = (Ee(2:N+1) - Ee(1:N))./(Ee(3:N+2)-Ee(2:N+1)); 

[xe,rre] = extend(x,rr,h,1,'N',0,'N',0); 
[xe,rme] = extend(x,rm,h,1,'N',0,'N',0); 
[xe,rEe] = extend(x,rE,h,1,'N',0,'N',0); 

phirL = FluxLimit(rre(1:N),type,beta); 
phirR = FluxLimit(rre(2:N+1),type,beta);
phimL = FluxLimit(rme(1:N),type,beta); 
phimR = FluxLimit(rme(2:N+1),type,beta);
phiEL = FluxLimit(rEe(1:N),type,beta); 
phiER = FluxLimit(rEe(2:N+1),type,beta);

% Compute fluxes
q = [re(2:N+1) me(2:N+1) Ee(2:N+1)]; 
qp = [re(3:N+2) me(3:N+2) Ee(3:N+2)]; 
qm = [re(1:N) me(1:N) Ee(1:N)];
FluxlowR  = EulerLF( q,qp,gamma,k/h,maxvel); 
FluxhighR = EulerLW( q,qp,gamma,k/h,maxvel);
FluxlowL  = EulerLF(qm, q,gamma,k/h,maxvel); 
FluxhighL = EulerLW(qm, q,gamma,k/h,maxvel);

FluxL(:,1) = FluxlowL(:,1) + phirL.*(FluxhighL(:,1) - FluxlowL(:,1));
FluxL(:,2) = FluxlowL(:,2) + phimL.*(FluxhighL(:,2) - FluxlowL(:,2));
FluxL(:,3) = FluxlowL(:,3) + phiEL.*(FluxhighL(:,3) - FluxlowL(:,3));

FluxR(:,1) = FluxlowR(:,1) + phirR.*(FluxhighR(:,1) - FluxlowR(:,1));
FluxR(:,2) = FluxlowR(:,2) + phimR.*(FluxhighR(:,2) - FluxlowR(:,2));
FluxR(:,3) = FluxlowR(:,3) + phiER.*(FluxhighR(:,3) - FluxlowR(:,3));

% Compute RHS 
dq = - (FluxR - FluxL)/h;
return