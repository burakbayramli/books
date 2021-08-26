function [dq] = EulerSLcharrhs1D(x,q,gamma,h,k,maxvel)
% function [dq] = EulerSLcharrhs1D(x,q,gamma,h,k,maxvel);
% Purpose: Evaluate right hand side for Euler equations using slope-limit
% method with limiting on characteristic variables
N = length(x); dq = zeros(N,3); qL = zeros(N+2,3); qR = zeros(N+2,3);
Rchar = zeros(N+4,3); Lamq = zeros(N+4,3); Rloc = zeros(3); Laml = zeros(1,3);

% Chose slope limiter - 0:LF; 1:minmod; 2:MUSCL; 3:Superbee; 
% 4:van Albada; 5:van Leer 
type = 2; c=0; 

% Extend data and assign boundary conditions
[xe,re] = extend(x,q(:,1),h,2,'D',1.0,'D',0.125);
[xe,me] = extend(x,q(:,2),h,2,'D',0,'N',0);
[xe,Ee] = extend(x,q(:,3),h,2,'D',2.5,'N',0);

% [xe,re] = extend(x,q(:,1),h,2,'D',3.857143,'N',0);
% [xe,me] = extend(x,q(:,2),h,2,'D',10.141852,'D',0);
% [xe,Ee] = extend(x,q(:,3),h,2,'D',39.166661,'N',0);

% Extract characteristic variables
qe = [re me Ee];
for i=1:N+4
    [S,iS,Lam] = EulerChar(qe(i,:),gamma);
    Rchar(i,:) = (iS*qe(i,:)')'; Lamq(i,:) = diag(Lam)';
end

% Compute limited slopes and values at cell interfaces
for i=1:N+2
    Rloc = Rchar(i:i+2,:); Laml = Lamq(i+1,:);
    
    % Determine upwind directions
    for j=1:3
      rRloc = (Rloc(2,j)-Rloc(1,j))/(Rloc(3,j)-Rloc(2,j));
      if (Laml(j)>=0) % Upwind
            dRlocL(j) = (Rloc(2,j)-Rloc(1,j))*SlopeLimit(1./rRloc,type,c);
      else % Downwind
            dRlocL(j) = (Rloc(3,j)- Rloc(2,j))*SlopeLimit(rRloc,type,c);
      end
    end
    Rloch = Rloc(2,:) - k/(2*h)*(Laml.*dRlocL);
    RlocL = Rloch - dRlocL/2; RlocR = Rloch + dRlocL/2;
    
    [S,iS,Lam] = EulerChar(qe(i+1,:),gamma);
    qL(i,:) = (S*RlocL')'; qR(i,:) = (S*RlocR')';
end

% Evaluate right hand side using numerical flux
dq = - (EulerLF(qR(2:N+1,:),qL(3:N+2,:),gamma, k/h,maxvel) - ...
          EulerLF(qR(1:N,:),qL(2:N+1,:),gamma, k/h,maxvel))/h;
return