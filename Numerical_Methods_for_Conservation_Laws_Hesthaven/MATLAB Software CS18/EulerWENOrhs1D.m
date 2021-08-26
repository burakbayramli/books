function [dq] = EulerWENOrhs1D(x,q,h,k,m,Crec,dw,beta,gamma,maxvel)
% function [dq] = EulerWENOrhs1D(x,q,h,k,m,Crec,dw,beta,gamma,maxvel)
% Purpose: Evaluate right hand side for Euler equations using a WENO method
% with reconstruction on the conserved variables
N = length(x); dq = zeros(N,3); 
ql = zeros(N,3); qr = zeros(N,3); qp = zeros(N,3); qm = zeros(N,3);

% Extend data and assign boundary conditions
[xe,re] = extend(x,q(:,1),h,m,'D',1.0,'D',0.125);
[xe,me] = extend(x,q(:,2),h,m,'D',0,'N',0);
[xe,Ee] = extend(x,q(:,3),h,m,'D',2.5,'N',0);

% [xe,re] = extend(x,q(:,1),h,m,'D',3.857143,'N',0);
% [xe,me] = extend(x,q(:,2),h,m,'D',10.141852,'D',0);
% [xe,Ee] = extend(x,q(:,3),h,m,'D',39.166661,'N',0);

% define cell left and right interface values
rl = zeros(N+2,1); rr = zeros(N+2,1);
ml = zeros(N+2,1); mr = zeros(N+2,1);
El = zeros(N+2,1); Er = zeros(N+2,1);

for i=1:N+2
   [rl(i),rr(i)] = WENO(xe(i:(i+2*(m-1))),re(i:(i+2*(m-1))),m,Crec,dw,beta);
   [ml(i),mr(i)] = WENO(xe(i:(i+2*(m-1))),me(i:(i+2*(m-1))),m,Crec,dw,beta);
   [El(i),Er(i)] = WENO(xe(i:(i+2*(m-1))),Ee(i:(i+2*(m-1))),m,Crec,dw,beta);
end

% Compute rhs - also change numerical flux here
ql = [rl(2:N+1) ml(2:N+1) El(2:N+1)]; qr = [rr(2:N+1) mr(2:N+1) Er(2:N+1)]; 
qp = [rl(3:N+2) ml(3:N+2) El(3:N+2)]; qm = [rr(1:N) mr(1:N) Er(1:N)];
dq = - (EulerLF(qr,qp,gamma,k/h,maxvel) - EulerLF(qm,ql,gamma,k/h,maxvel))/h;
return