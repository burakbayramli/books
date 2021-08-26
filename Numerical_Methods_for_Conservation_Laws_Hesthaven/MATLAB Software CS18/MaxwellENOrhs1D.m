function [dq] = MaxwellENOrhs1D(x,q,ep,mu,h,k,m,Crec,maxvel);
% function [dE] = MaxwellENOrhs1D(x,q,ep,mu,h,k,m,Crec,maxvel);
% Purpose: Evaluate right hand side for Maxwells equation using ENO method

N = length(x); dq = zeros(N,2);
ql = zeros(N,2); qr = zeros(N,2); qm = zeros(N,2); qp = zeros(N,2);

% Extend data and assign boundary conditions
[xe,Ee] = extend(x,q(:,1),h,m,'D',0,'D',0);
[xe,He] = extend(x,q(:,2),h,m,'N',0,'N',0);

% define cell left and right interface values
Em = zeros(N+2,1); Ep = zeros(N+2,1); Hm = zeros(N+2,1); Hp = zeros(N+2,1);
for i=1:N+2
  [Em(i),Ep(i)] = ENO(xe(i:(i+2*(m-1))),Ee(i:(i+2*(m-1))),m,Crec);
  [Hm(i),Hp(i)] = ENO(xe(i:(i+2*(m-1))),He(i:(i+2*(m-1))),m,Crec);
end;

% Extract interface values
qr = [Ep(2:N+1) Hp(2:N+1)]; ql = [Em(2:N+1) Hm(2:N+1)];
qm = [Ep(1:N) Hp(1:N)]; qp = [Em(3:N+2) Hm(3:N+2)];

% Lax Friedrich flux - or something simple
% dEH = - (MaxwellLF(qr,qp,ep,mu,k/h,maxvel) - ...
%           MaxwellLF(qm,ql,ep,mu,k/h,maxvel))/h;

% Exact upwinding
[xe,epe] = extend(x,ep,h,1,'N',0,'N',0); 
[xe,mue] = extend(x,mu,h,1,'N',0,'N',0);
ep0 = epe(2:N+1); epm = epe(1:N); epp = epe(3:N+2); 
mu0 = mue(2:N+1); mum = mue(1:N); mup = mue(3:N+2);

dEH = - (MaxwellUpwind(qr,qp,ep0,epp,mu0,mup,ep0,mu0) - ...
           MaxwellUpwind(qm,ql,epm,ep0,mum,mu0,ep0,mu0))/h;
return