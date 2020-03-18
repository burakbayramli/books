%Last Updated on 03-10-2008

%This program applies Gradient Projection with spectral (Barzilai-Borwein)
% steplength to solve the dual formulation of ROF image restoration model
% steplength alpha is switched between the two BB rules adaptively

% A limited minimization line search is performed on \gamma to ensure
% monotonocity

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

function [u, w1,w2, Energy, Dgap, TimeCost, itr] = ...
      TV_GPABB(w1,w2,f,lbd,NIT,GapTol,verbose);

n=length(f);            %Assume a square image        
g=lbd*f;
sf = 0.5*lbd*sum(sum(f.^2));
gx = [g(:,2:n)-g(:,1:n-1), zeros(n,1)];
gy = [g(2:n,:)-g(1:n-1,:); zeros(1,n)];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% upper and lower bounds on line search parameter alpha
amin = 1.e-5; amax = 1.e5;
% choice of step length for the first iteration (in [amin,amax])
alpha = 1;   
%%%%%%%%% Parameters for adaptively switching between two BB rules %%%%%%%%
na=1;   flag = 1; 
Nmin = 1;   Nmax = 10;
gl=0.1;     gu=5;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Compute energy
DivW=([w1(:,1),w1(:,2:n)-w1(:,1:n-1)] + [w2(1,:);w2(2:n,:)-w2(1:n-1,:)]); 
Energy(1)=0.5*sum(sum((DivW-g).^2));

%Compute the primal u and the duality gap
u  = f - (1/lbd)*DivW;   
ux = [u(:,2:n)-u(:,1:n-1), zeros(n,1)];
uy = [u(2:n,:)-u(1:n-1,:); zeros(1,n)];
gu_norm = sqrt(ux.^2+uy.^2);
Dgap(1) = sum(sum(gu_norm + ux.*w1 + uy.*w2)); 
TimeCost(1)=0;
t0 = cputime;            %Start CPU clock

for itr=1:NIT    
  % gradient of the objective function
  dFx = [DivW(:,1:n-1)-DivW(:,2:n), zeros(n,1)] + gx; 
  dFy = [DivW(1:n-1,:)-DivW(2:n,:); zeros(1,n)] + gy;
  
  w1_old = w1;    w2_old = w2;
  w1 = w1_old - alpha * dFx;
  w2 = w2_old - alpha * dFy;
  
  %apply gradient projection to ensure constraints
  %let w_{i,j}=w_{i,j}/|w_{i,j}| if |w_{i,j}|>1
  wnorm= max(1, sqrt(w1.^2+w2.^2));
  w1 = w1./wnorm;
  w2 = w2./wnorm;
  
  dw1 = w1-w1_old;    %search direction satisfying constraints 
  dw2 = w2-w2_old;
  
  DivdW=([dw1(:,1),dw1(:,2:n)-dw1(:,1:n-1)] + [dw2(1,:);dw2(2:n,:)-dw2(1:n-1,:)]);
  % calculate the step length gamma along the (feasible) line segment
  gamma1 = -sum(sum(dw1.*dFx + dw2.*dFy))/(sum(sum(DivdW.^2)));  %Exact line search along dw
  gamma = max(0,min(gamma1,1));    
  w1 = w1_old + gamma*dw1;
  w2 = w2_old + gamma*dw2;
  
  % compute step length alpha for next iteration
  alp(1) = sum(sum(dw1.^2 + dw2.^2))/sum(sum(DivdW.^2));
  gwx = [DivdW(:,2:n)-DivdW(:,1:n-1), zeros(n,1)];
  gwy = [DivdW(2:n,:)-DivdW(1:n-1,:); zeros(1,n)];
  alp(2) = sum(sum(DivdW.^2))/sum(sum(gwx.^2 + gwy.^2));
  
  if na > Nmin
    if( (na>Nmax) | ((alpha>alp(2)) & (alpha<alp(1))) | (gamma1<=gl & flag ==1) |(gamma1>=gu & flag==2) )
      flag = 3-flag;
      na = 0;
    end
  end
  na = na+1;  
  alpha = min(amax, max(amin, alp(flag)) );
  %fprintf('%f\t%f\t%f\t%f\t%g\n', alpha, alp(1), alp(2), gamma1, flag);   
  
  %Compute new energy
  DivW = ([w1(:,1),w1(:,2:n)-w1(:,1:n-1)] + [w2(1,:);w2(2:n,:)-w2(1:n-1,:)]); 
  Energy_new = 0.5*sum(sum((DivW-g).^2)); 
  Energy(itr) = Energy_new;
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
    fprintf(1,' GPABB itr %d: Obj=%12.6e, rel dgap=%9.3e\n', ...
	itr, DualVal, Dgap(itr+1));
  end
  if (Dgap(itr+1) < GapTol )
    if verbose
      fprintf(1,'GPABB: convergence tolerance reached: %6.2e\n',...
	  Dgap(itr+1));
    end
    break;
  end
  
end
