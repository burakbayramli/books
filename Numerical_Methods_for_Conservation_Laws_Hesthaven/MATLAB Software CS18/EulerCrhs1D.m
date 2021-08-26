function [dq] = EulerCrhs1D(x,q,gamma,h,k,maxvel)
% function [dq] = EulerCrhs1D(x,q,gamma,h,k,maxvel);
% Purpose: Evaluate right hand side for Euler equations using second order
% central scheme - NOTE two steps of k/2 is taken
N = length(x); dq = zeros(N,3); qh = zeros(N+2,3); 
Ns = N-1; kl=k/2; duL = zeros(N+2,3); duLs = zeros(Ns+2,3);

% Chose slope limiter - 0:LF; 1:minmod; 2:MUSCL; 3:Superbee; 
% 4:van Albada; 5:van Leer, 6: TVB 
type = 2; c=0; M=10;

% First step from non-staggered to staggered grid
% Extend data and assign boundary conditions
[xe,re] = extend(x,q(:,1),h,2,'D',1.0,'D',0.125);
[xe,me] = extend(x,q(:,2),h,2,'D',0,'N',0);
[xe,Ee] = extend(x,q(:,3),h,2,'D',2.5,'N',0);

% [xe,re] = extend(x,q(:,1),h,2,'D',3.857143,'N',0);
% [xe,me] = extend(x,q(:,2),h,2,'D',10.141852,'D',0);
% [xe,Ee] = extend(x,q(:,3),h,2,'D',39.166661,'N',0);

% Extract variables
qe = [re me Ee];

% Compute left and right differences, evaluate slopes
dup = qe(3:N+4,:) - qe(2:N+3,:); dum = qe(2:N+3,:) - qe(1:N+2,:);
duL(:,1) = SlopeLimit(dup(:,1), dum(:,1),type,c,M,h);
duL(:,2) = SlopeLimit(dup(:,2), dum(:,2),type,c,M,h);
duL(:,3) = SlopeLimit(dup(:,3), dum(:,3),type,c,M,h);

% Compute intermediate values. f'(u) = Au
for i=1:N+2
    A = EulerJac(qe(i+1,:),gamma);
    qh(i,:) = qe(i+1,:) - k/(2*h)*duL(i,:)*A';
end

% Advance solition k/2
qs = (q(1:N-1,:)+q(2:N,:))/2 + (duL(2:N,:)-duL(3:N+1,:))/8 ...
        - kl/h*(EulerFlux(qh(3:N+1,:),gamma)-EulerFlux(qh(2:N,:),gamma));

% Second step from staggered to non-staggered grid
% Extend data and assign boundary conditions
xs = x(1:Ns)+h/2;
[xe,re] = extendstag(xs,qs(:,1),h,2,'D',1.0,'D',0.125);
[xe,me] = extendstag(xs,qs(:,2),h,2,'D',0,'N',0);
[xe,Ee] = extendstag(xs,qs(:,3),h,2,'D',2.5,'N',0);

% [xe,re] = extendstag(xs,qs(:,1),h,2,'D',3.857143,'N',0);
% [xe,me] = extendstag(xs,qs(:,2),h,2,'D',10.141852,'D',0);
% [xe,Ee] = extendstag(xs,qs(:,3),h,2,'D',39.166661,'N',0);

% Extract variables
qe = [re me Ee];    

% Compute left and right differences, evaluate slopes
dup = qe(3:Ns+4,:) - qe(2:Ns+3,:); dum = qe(2:Ns+3,:) - qe(1:Ns+2,:);
duLs(:,1) = SlopeLimit(dup(:,1), dum(:,1),type,c,M,h);
duLs(:,2) = SlopeLimit(dup(:,2), dum(:,2),type,c,M,h);
duLs(:,3) = SlopeLimit(dup(:,3), dum(:,3),type,c,M,h);

% Compute intermediate values. f'(u) = Au
for i=1:Ns+2
    A = EulerJac(qe(i+1,:),gamma);
    qh(i,:) = qe(i+1,:) - kl/(2*h)*duLs(i,:)*A';
end

% Advance solition k/2
qns = (qe(2:Ns+2,:)+qe(3:Ns+3,:))/2 + (duLs(1:Ns+1,:)-duLs(2:Ns+2,:))/8 ...
        - kl/h*(EulerFlux(qh(2:Ns+2,:),gamma)-EulerFlux(qh(1:Ns+1,:),gamma));

% Restore residual
dq = (qns-q)/k;
return