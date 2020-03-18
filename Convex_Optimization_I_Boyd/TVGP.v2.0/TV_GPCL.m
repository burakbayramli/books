% Last updated 03-10-2008

%This program applies basic gradient projection method with constant 
%steplength to solve the dual formulation of ROF image restoration model
%The const steplength is passed as input parameter alpha. Global
%convergence can be proved for alpha <= 1/4.

% Dual Formulation of TV model:
% min || div w - \lbd f|| subject to |w| <= 1

% Feasibility Eqn: \div w + \lbd (u-f)=0 
% Complimentarity: |\g u|\g u + w = 0

% \div : divergence,  \g : gradient
%-------------------------------------------------------------------------
% Input variables
%-------------------------------------------------------------------------
% w1,w2:        Dual variable, initial guess. 
% f:            noisy image
% lbd:          Constant fidelity parameter. 
% alpha:        fixed steplength
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

function [u,w1,w2,Energy,Dgap,TimeCost,itr] = ...
      TV_GPCL(w1,w2,f,lbd,alpha,NIT,GapTol,verbose);

n=length(f);                %Assume a square image        
g=lbd*f;
gx = [g(:,2:n)-g(:,1:n-1), zeros(n,1)];
gy = [g(2:n,:)-g(1:n-1,:); zeros(1,n)];
sf = 0.5*lbd*sum(sum(f.^2));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Compute energy
DivW=([w1(:,1),w1(:,2:n)-w1(:,1:n-1)] + [w2(1,:);w2(2:n,:)-w2(1:n-1,:)]); 
Energy(1)=0.5*sum(sum((DivW-g).^2));

%Compute the primal u and the duality gap
u=f - (1/lbd)*DivW;   
ux = [u(:,2:n)-u(:,1:n-1), zeros(n,1)];
uy = [u(2:n,:)-u(1:n-1,:); zeros(1,n)];
gu_norm = sqrt(ux.^2+uy.^2);
Dgap(1)= sum(sum(gu_norm + ux.*w1 + uy.*w2));   
TimeCost(1) = 0;
t0 = cputime;                %Start CPU clock

for itr=1:NIT
  % gradient of the objective function
  dFx = [DivW(:,1:n-1)-DivW(:,2:n), zeros(n,1)] + gx;
  dFy = [DivW(1:n-1,:)-DivW(2:n,:); zeros(1,n)] + gy;  

  % GP with constant step length
  w1 = w1 - alpha * dFx;
  w2 = w2 - alpha * dFy;
  wnorm= max(1, sqrt(w1.^2+w2.^2));
  w1 = w1./wnorm;
  w2 = w2./wnorm;
  
  % Compute Energy  
  DivW=([w1(:,1),w1(:,2:n)-w1(:,1:n-1)] + [w2(1,:);w2(2:n,:)-w2(1:n-1,:)]); 
  Energy_new=0.5*sum(sum((DivW-g).^2));
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
  Dgap(itr+1) = Dgap(itr+1)/(abs(PriVal)+abs(DualVal));
  if verbose
    fprintf(1,' GPCL iter %4d: Obj=%11.6e, rel dgap=%7.3e\n', ...
	itr, DualVal, Dgap(itr+1));
  end
  if (Dgap(itr+1) < GapTol )
    if verbose
      fprintf(1,'GPCL convergence tolerance reached: %6.2e\n',...
	 Dgap(itr+1));
    end
    break
  end
  
end    
