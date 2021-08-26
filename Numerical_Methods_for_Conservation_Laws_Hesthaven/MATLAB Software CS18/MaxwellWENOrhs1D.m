function [dq] = MaxwellWENOrhs1D(x,q,ep,mu,h,k,m,Crec,dw,beta,maxvel);
% function [dq] = MaxwellWENOrhs1D(x,q,ep,mu,h,k,m,Crec,dw,beta,maxvel);
% Purpose: Evaluate right hand side for Maxwells equation using WENO method

N = length(x); dq = zeros(N,2);
ql = zeros(N,2); qr = zeros(N,2); qm = zeros(N,2); qp = zeros(N,2);

% Extend data and assign boundary conditions
[xe,Ee] = extend(x,q(:,1),h,m,'D',0,'D',0);
[xe,He] = extend(x,q(:,2),h,m,'N',0,'N',0);

% define cell left and right interface values
El = zeros(N+2,1); Er = zeros(N+2,1); Hl = zeros(N+2,1); Hr = zeros(N+2,1);

for i=1:N+2
 [El(i),Er(i)] = WENO(xe(i:(i+2*(m-1))),Ee(i:(i+2*(m-1))),m,Crec,dw,beta);
 [Hl(i),Hr(i)] = WENO(xe(i:(i+2*(m-1))),He(i:(i+2*(m-1))),m,Crec,dw,beta);
end;

% Compute residual
qr = [Er(2:N+1) Hr(2:N+1)]; ql = [El(2:N+1) Hl(2:N+1)];
qm = [Er(1:N) Hr(1:N)]; qp = [El(3:N+2) Hl(3:N+2)];

% Lax Friedrich flux - or something simple
dq = - (MaxwellLF(qr,qp,ep,mu,k/h,maxvel) - ...
             MaxwellLF(qm,ql,ep,mu,k/h,maxvel))/h;
return