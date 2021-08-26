function [dq] = EulerSLrhs1D(x,q,gamma,h,k,maxvel)
% function [dq] = EulerSLrhs1D(x,q,gamma,h,k,maxvel);
% Purpose: Evaluate right hand side for the Euler equations using 
% slope-limited method with limiting on characteristic variables
N = length(x); dq = zeros(N,3); qL = zeros(N+2,3); qR = zeros(N+2,3);
dup = zeros(N+2,3); dum = zeros(N+2,3); duL = zeros(N+2,3);

% Chose slope limiter - 0:LF; 1:minmod; 2:MUSCL; 3:Superbee; 
% 4:van Albada; 5:van Leer, 6: TVB 
type = 2; c=h^3; M=150;

% Extend data and assign boundary conditions
[xe,re] = extend(x,q(:,1),h,2,'D',1.0,'D',0.125);
[xe,me] = extend(x,q(:,2),h,2,'D',0,'N',0);
[xe,Ee] = extend(x,q(:,3),h,2,'D',2.5,'N',0);

% [xe,re] = extend(x,q(:,1),h,2,'D',3.857143,'N',0);
% [xe,me] = extend(x,q(:,2),h,2,'D',10.141852,'D',0);
% [xe,Ee] = extend(x,q(:,3),h,2,'D',39.166661,'N',0);

% Extract variables
qe = [re me Ee];

% Compute left and right differences, evaluate slopes and interface values
dup = qe(3:N+4,:) - qe(2:N+3,:); dum = qe(2:N+3,:) - qe(1:N+2,:);
duL(:,1) = SlopeLimit(dup(:,1), dum(:,1),type,c,M,h);
duL(:,2) = SlopeLimit(dup(:,2), dum(:,2),type,c,M,h);
duL(:,3) = SlopeLimit(dup(:,3), dum(:,3),type,c,M,h);
qL = qe(2:N+3,:) - duL/2; qR = qe(2:N+3,:) + duL/2;
 
% Evaluate right hand side using numerical flux
dq = - (EulerLF(qR(2:N+1,:),qL(3:N+2,:),gamma, k/h,maxvel) - ...
          EulerLF(qR(1:N,:),qL(2:N+1,:),gamma, k/h,maxvel))/h;
return