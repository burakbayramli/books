function [dq] = EulerWENOcharrhs1D(x,q,h,k,m,Crec,dw,beta,gamma,maxvel)
% function [dq] = EulerWENOcharrhs1D(x,q,h,k,m,Crec,dw,beta,gamma,maxvel)
% Purpose: Evaluate right hand side for Euler equations using a WENO method
%          on the characteristic variables

N = length(x); dq = zeros(N ,3); 
ql = zeros(N,3); qr = zeros(N,3); qp = zeros(N,3); qm = zeros(N,3);

% Extend data and assign boundary conditions for Sod's problem
[xe,re] = extend(x,q(:,1),h,m,'D',1.0,'D',0.125);
[xe,me] = extend(x,q(:,2),h,m,'D',0,'N',0);
[xe,Ee] = extend(x,q(:,3),h,m,'D',2.5,'N',0);

% Extend data and assign boundary conditions for shock entropy problem
% [xe,re] = extend(x,q(:,1),h,m,'D',3.857143,'N',0);
% [xe,me] = extend(x,q(:,2),h,m,'D',10.141852,'D',0);
% [xe,Ee] = extend(x,q(:,3),h,m,'D',39.166661,'N',0);

% define cell left and right interface values
qe = [re me Ee]; Rlh = zeros(1,3); Rrh= zeros(1,3);
rm = zeros(N+2,1); mm = zeros(N+2,1); Em = zeros(N+2,1); 
rp = zeros(N+2,1); mp = zeros(N+2,1); Ep = zeros(N+2,1);
for i=1:N+2
  qloc = qe(i:(i+2*(m-1)),:); q0 = qloc(m,:);
  [S, iS, Lam] = EulerChar(q0,gamma); Rloc = (iS*qloc')';
  [Rlh(1),Rrh(1)] = WENO(xe(i:(i+2*(m-1))),Rloc(:,1),m,Crec,dw,beta);
  [Rlh(2),Rrh(2)] = WENO(xe(i:(i+2*(m-1))),Rloc(:,2),m,Crec,dw,beta);
  [Rlh(3),Rrh(3)] = WENO(xe(i:(i+2*(m-1))),Rloc(:,3),m,Crec,dw,beta);
  qlh = (S*Rlh')'; qrh = (S*Rrh')';
  rm(i) = qlh(1); mm(i) = qlh(2); Em(i) = qlh(3);
  rp(i) = qrh(1); mp(i) = qrh(2); Ep(i) = qrh(3);
end;

% Compute rhs - also change numerical flux here
ql = [rm(2:N+1) mm(2:N+1) Em(2:N+1)]; qr = [rp(2:N+1) mp(2:N+1) Ep(2:N+1)]; 
qp = [rm(3:N+2) mm(3:N+2) Em(3:N+2)]; qm = [rp(1:N) mp(1:N) Ep(1:N)];
dq = - (EulerLF(qr,qp,gamma,k/h,maxvel) - ...
           EulerLF(qm,ql,gamma,k/h,maxvel))/h;
return
