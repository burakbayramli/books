function [rhsq] = EulerDGrhs1D(x,q,h,k,m,N,gamma,S,Ma,VtoE,maxvel)
% function [rhsq] = EulerDGrhs1D(x,q,h,k,m,N,gamma,S,Ma,VtoE,maxvel)
% Purpose  : Evaluate the RHS of the Euler equations using a DG method
Imat = eye(m+1); re = zeros(2,N+2); rue = zeros(2,N+2); Ee = zeros(2,N+2);

% Extract solution
r = q(:,:,1); ru = q(:,:,2); E = q(:,:,3); 

% Extend data and assign boundary conditions
% [re]  = extendDG( r(VtoE),'D',1.0,'D',0.125);
% [rue] = extendDG(ru(VtoE),'D',0,'N',0);
% [Ee]  = extendDG( E(VtoE),'D',2.5,'N',0);

[re]  = extendDG( r(VtoE),'D',3.857143,'N',0);
[rue] = extendDG(ru(VtoE),'D',10.141852,'D',0);
[Ee]  = extendDG( E(VtoE),'D',39.166661,'N',0);

% Compute volume fluxes
p = (gamma-1)*(E - 0.5*ru.^2./r);
fr = ru; fru = ru.^2./r + p; fE = (E+p).*ru./r;

% Compute surface fluxes
fluxr = EulerLF([re(2,2:N+1)' rue(2,2:N+1)' Ee(2,2:N+1)'],...
                [re(1,3:N+2)' rue(1,3:N+2)' Ee(1,3:N+2)'],...
                gamma, 0, maxvel)';
frr = fluxr(1,:); frur = fluxr(2,:);  fEr = fluxr(3,:);                         
fluxl = EulerLF([re(2,1:N)' rue(2,1:N)' Ee(2,1:N)'],...
                [re(1,2:N+1)' rue(1,2:N+1)' Ee(1,2:N+1)'],...
                gamma, 0, maxvel)';
frl = fluxl(1,:); frul = fluxl(2,:);  fEl = fluxl(3,:);                         

% Compute residual
qh = S'*fr - (Imat(:,m+1)*frr(1,:) - Imat(:,1)*frl(1,:));
rhsq(:,:,1) = (h/2*Ma)\qh;
qh = S'*fru - (Imat(:,m+1)*frur(1,:) - Imat(:,1)*frul(1,:));
rhsq(:,:,2) = (h/2*Ma)\qh;
qh = S'*fE - (Imat(:,m+1)*fEr(1,:) - Imat(:,1)*fEl(1,:));
rhsq(:,:,3) = (h/2*Ma)\qh;
return

