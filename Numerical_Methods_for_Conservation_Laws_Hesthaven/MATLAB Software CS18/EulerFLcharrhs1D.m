function [dq] = EulerFLcharrhs1D(x,q,gamma,h,k,maxvel)
% function [dq] = EulerFLcharrhs1D(x,q,gamma,h,k,maxvel);
% Purpose: Evaluate right hand side for Euler equations using flux-limit
% scheme with limiting on characteristic variables
N = length(x); dq = zeros(N,3); phir = zeros(3,1);

% Chose flux limiter - 0:LF; 1:CO; 2:Koren; 3:Sweby; 4:OSPRE; 5:van Leer 
type = 5; beta=1.5; 

% Extend data and assign boundary conditions
[xe,re] = extend(x,q(:,1),h,2,'D',1.0,'D',0.125);
[xe,me] = extend(x,q(:,2),h,2,'D',0,'N',0);
[xe,Ee] = extend(x,q(:,3),h,2,'D',2.5,'N',0);

% [xe,re] = extend(x,q(:,1),h,2,'D',3.857143,'N',0);
% [xe,me] = extend(x,q(:,2),h,2,'D',10.141852,'D',0);
% [xe,Ee] = extend(x,q(:,3),h,2,'D',39.166661,'N',0);

qe = [re me Ee];
for i=1:N
    qloc = qe(i:i+4,:);
    
    % Left cell interface
    q0 = (qloc(2,:)+qloc(3,:))/2;
    [S, iS, Lam] = EulerChar(q0,gamma); Rloc = (iS*qloc')';
    for j=1:3
      if (Lam(j,j)>=0) % Upwind
        phir(j) = ...
          FluxLimit((Rloc(2,j)-Rloc(1,j))/(Rloc(3,j)-Rloc(2,j)),type,beta);
      else % Downwind
        phir(j) = ...
          FluxLimit((Rloc(4,j)-Rloc(3,j))/(Rloc(3,j)-Rloc(2,j)),type,beta);
      end  
    end
    phiL = S*diag(phir(:))*iS;
    
    % Right cell interface
    q0 = (qloc(3,:)+qloc(4,:))/2;
    [S, iS, Lam] = EulerChar(q0,gamma); Rloc = (iS*qloc')';
    for j=1:3
      if (Lam(j,j)>=0) % Upwind
        phir(j) = ...
          FluxLimit((Rloc(3,j)-Rloc(2,j))/(Rloc(4,j)-Rloc(3,j)),type,beta);
      else % Downwind
        phir(j) = ...
          FluxLimit((Rloc(5,j)-Rloc(4,j))/(Rloc(4,j)-Rloc(3,j)),type,beta);
      end  
    end
    phiR = S*diag(phir(:))*iS;

    % Compute fluxes
    q = [re(i+2) me(i+2) Ee(i+2)]; 
    qp = [re(i+3) me(i+3) Ee(i+3)]; 
    qm = [re(i+1) me(i+1) Ee(i+1)];
    FluxlowR  = EulerLF( q,qp,gamma,k/h,maxvel)'; 
    FluxhighR = EulerLW( q,qp,gamma,k/h,maxvel)';
    FluxlowL  = EulerLF(qm, q,gamma,k/h,maxvel)'; 
    FluxhighL = EulerLW(qm, q,gamma,k/h,maxvel)';
    FluxL = FluxlowL + phiL*(FluxhighL - FluxlowL);
    FluxR = FluxlowR + phiR*(FluxhighR - FluxlowR);
    
    % Compute RHS 
    dq(i,:) = - (FluxR' - FluxL')/h;
end
return