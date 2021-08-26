function [dq] = EulerMrhs1D(x,q,gamma,h,k,maxvel)
% function [dq] = EulerMrhs1D(x,q,gamma,h,k,maxvel);
% Purpose: Evaluate right hand side for the Euler equations
% using a monotone method

% Extend data and assign boundary conditions
[xe,re] = extend(x,q(:,1),h,1,'D',1.0,'D',0.125);
[xe,me] = extend(x,q(:,2),h,1,'D',0,'N',0);
[xe,Ee] = extend(x,q(:,3),h,1,'D',2.5,'N',0);

% [xe,re] = extend(x,q(:,1),h,1,'D',3.857143,'N',0);
% [xe,me] = extend(x,q(:,2),h,1,'D',10.141852,'D',0);
% [xe,Ee] = extend(x,q(:,3),h,1,'D',39.166661,'N',0);

% Compute RHS - change numerical flux here
N = length(x); q = [re(2:N+1) me(2:N+1) Ee(2:N+1)]; 
qp = [re(3:N+2) me(3:N+2) Ee(3:N+2)]; qm = [re(1:N) me(1:N) Ee(1:N)];
dq = - (EulerLF(q,qp,gamma,k/h,maxvel)-EulerLF(qm,q,gamma, k/h,maxvel))/h;
return