%Last Updated on 03-10-2008

%This program applies basic gradient projection algorithm with backtracking 
%line search on the boundary  to solve the dual formulation of ROF image 
%restoration model

%Dual Formulation of TV model:
% min || div w - \lbd f|| subject to |w| <= 1

% \div : divergence,  \g : gradient
%-------------------------------------------------------------------------
% Input variables
%-------------------------------------------------------------------------
% w1,w2:        Dual variable, initial guess. 
% f:            noisy image
% lbd:          Constant fidelity parameter. 
% NIT:          Maximum number of iterations
% GapTol:       Convergence tolerance (stop criterion) for relative duality gap
%--------------------------------------------------------------------------
%-------------------------------------------------------------------------
% Output variables
%-------------------------------------------------------------------------
% u:                Primal variable, numerical solution - restored image 
% w = (w1,w2):      Dual variable, numerical solution
% Energy:           The value of the objective function
% DGap:             Duality Gap
% TimeCost:        CPU time cost
% itr:              number of iterations

function [u, w1, w2, Energy, Dgap, TimeCost, itr] = ...
      TV_GPLS(w1,w2,f,lbd,NIT,GapTol,verbose);

n=length(f);                    %Assume a square image        
g=lbd*f;
gx = [g(:,2:n)-g(:,1:n-1), zeros(n,1)];
gy = [g(2:n,:)-g(1:n-1,:); zeros(1,n)];
sf = 0.5*lbd*sum(sum(f.^2));

% initialize
beta = 0.5;   mu = 1.e-4;       % backtracking line search parameter; 
amin = 1.e-5; amax = 1.e5;    % choice of step length for the first iteration (must be in [amin,amax])

% Compute energy
DivW=([w1(:,1),w1(:,2:n)-w1(:,1:n-1)] + [w2(1,:);w2(2:n,:)-w2(1:n-1,:)]); 
Energy(1)=0.5*sum(sum((DivW-g).^2));

%Compute the primal u and the duality gap
u  = f - (1/lbd)*DivW;   
ux = [u(:,2:n)-u(:,1:n-1), zeros(n,1)];
uy = [u(2:n,:)-u(1:n-1,:); zeros(1,n)];
gu_norm = sqrt(ux.^2+uy.^2);
Dgap(1) = sum(sum(gu_norm + ux.*w1 + uy.*w2));   
TimeCost(1) = 0;
t0 = cputime;                    %Start CPU clock    

for itr=1:NIT
    %gradient of objective function
    dFx = [DivW(:,1:n-1)-DivW(:,2:n), zeros(n,1)] + gx;
    dFy = [DivW(1:n-1,:)-DivW(2:n,:); zeros(1,n)] + gy;
    
    % Construct the projected search direction - the projection of the
    % direction onto the tangent of the set of active indices. Initialize it
    % to the gradient
    g1 = dFx;   g2 = dFy;
    % Identify the components for which projection won't be operative when
    % the step is sufficiently short. 
    wnorm = sqrt(w1.^2+w2.^2);
    gproj = dFx.*w1 + dFy.*w2;
    % These are the components that are on the boundary and for which the
    % negative gradient direction points outside the disk. For these, the
    % projected direction needs to be modified from -(dFx,dFy)
    id = (wnorm >= 1) & (gproj < 0); 
    g1(id) = g1(id) - gproj(id).*w1(id);
    g2(id) = g2(id) - gproj(id).*w2(id);
    
    % now find the unconstrained min along this direction
    Divg=([g1(:,1),g1(:,2:n)-g1(:,1:n-1)] + [g2(1,:);g2(2:n,:)-g2(1:n-1,:)]);
    alpha0 = sum(sum(g1.*dFx + g2.*dFy))/sum(sum(Divg.^2));
    % the next version includes a fudge factor of 1/2
    % alpha0 = 0.5*sum(sum(g1.*dFx + g2.*dFy))/sum(sum(Divg.^2));
    % alpha0 = max(amin, min(alpha0, amax)); 
    
    w1_old = w1;
    w2_old = w2;
    Energy_old = Energy(itr);
    alpha= alpha0/beta;
        
    %Perform line search to ensure decrease in energy.
    %In our case, it seems not to be needed in practice
    while 1
      alpha=alpha*beta;
      w1 = w1_old - alpha * dFx;
      w2 = w2_old - alpha * dFy;
      %Project w to the feasibility set
      wnorm= max(1, sqrt(w1.^2+w2.^2));
      w1 = w1./wnorm;
      w2 = w2./wnorm;
      %Compute new energy
      DivW = ([w1(:,1),w1(:,2:n)-w1(:,1:n-1)] + [w2(1,:);w2(2:n,:)-w2(1:n-1,:)]); 
      Energy_new = 0.5*sum(sum((DivW-g).^2)); 
      dw1=w1-w1_old; dw2=w2-w2_old;
      if ( Energy_new <= (Energy_old + mu*sum(sum(dw1.*dFx + dw2.*dFy))))
	break;
      end
    end
    
    Energy(itr+1)=Energy_new;
    %Compute the primal u and the duality gap
    u=f - (1/lbd)*DivW;   
    ux = [u(:,2:n)-u(:,1:n-1), zeros(n,1)];
    uy = [u(2:n,:)-u(1:n-1,:);zeros(1,n)];
    gu_norm = sqrt(ux.^2+uy.^2);
    Dgap(itr+1)= sum(sum(gu_norm + ux.*w1 + uy.*w2)); 
    TimeCost(itr+1)=cputime-t0;

    % test for convergence:  
    % (Primal-Dual) / (|Primal|+|Dual|)< tol
    DualVal=sf-Energy_new/lbd; PriVal=DualVal+Dgap(itr+1);
    Dgap(itr+1)=Dgap(itr+1)/(abs(PriVal)+abs(DualVal));
    if verbose
      fprintf(1,' GPLS iter %4d: Obj=%11.6e, alpha=%6.2e, rel dgap=%7.3e\n', ...
	  itr, DualVal, alpha, Dgap(itr+1));
    end
    if (Dgap(itr+1) < GapTol )
      if verbose
	fprintf(1,'GPLS convergence tolerance reached: %6.2e\n',...
	    Dgap(itr+1));
      end
      break
    end

end
