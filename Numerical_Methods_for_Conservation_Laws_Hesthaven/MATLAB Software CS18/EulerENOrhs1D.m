function [dq] = EulerENOrhs1D(x,q,h,k,m,Crec,gamma,maxvel)
% function [dq] = EulerENOrhs1D(x,q,h,k,m,Crec,gamma,maxvel)
% Purpose: Evaluate right hand side for Euler equations using an ENO method
N = length(x); dq = zeros(N,3); 
ql = zeros(N,3); qr = zeros(N,3); qp = zeros(N,3); qm = zeros(N,3);

% Extend data and assign boundary conditions for Sod's problem
[xe,re] = extend(x,q(:,1),h,m,'D',1.0,'D',0.125);
[xe,me] = extend(x,q(:,2),h,m,'D',0,'N',0);
[xe,Ee] = extend(x,q(:,3),h,m,'D',2.5,'N',0);

% Extend data and assign boundary conditions for shock entropy
% [xe,re] = extend(x,q(:,1),h,m,'D',3.857143,'N',0);
% [xe,me] = extend(x,q(:,2),h,m,'D',10.141852,'D',0);
% [xe,Ee] = extend(x,q(:,3),h,m,'D',39.166661,'N',0);

% define cell left and right interface values
rm = zeros(N+2,1); mm = zeros(N+2,1); Em = zeros(N+2,1); 
rp = zeros(N+2,1); mp = zeros(N+2,1); Ep = zeros(N+2,1);

for i=1:N+2
  [rm(i),rp(i)] = ENO(xe(i:(i+2*(m-1))),re(i:(i+2*(m-1))),m,Crec);
  [mm(i),mp(i)] = ENO(xe(i:(i+2*(m-1))),me(i:(i+2*(m-1))),m,Crec);
  [Em(i),Ep(i)] = ENO(xe(i:(i+2*(m-1))),Ee(i:(i+2*(m-1))),m,Crec);
end;

% Compute rhs - also change numerical flux here
ql = [rm(2:N+1) mm(2:N+1) Em(2:N+1)]; qr = [rp(2:N+1) mp(2:N+1) Ep(2:N+1)]; 
qp = [rm(3:N+2) mm(3:N+2) Em(3:N+2)]; qm = [rp(1:N) mp(1:N) Ep(1:N)];
dq = - (EulerLF(qr,qp,gamma,k/h,maxvel) - ...
            EulerLF(qm,ql,gamma,k/h,maxvel))/h;
return