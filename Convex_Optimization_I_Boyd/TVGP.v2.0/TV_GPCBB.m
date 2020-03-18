%Last Updated on 04-14-2008

%This program applies Gradient Projection with cyclic Barzilai-Borwein
% steplength to solve the dual formulation of ROF image restoration model

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
% monotone:     1=monotone method using limited minimization; 0=nonmonotone
% BBformula:    1=standard BB formula, 2=alternative BB formula.
% fudge:        fudge factor for scaling of the BB alpha.
% cycleLength:  number of iterations that reuse the same step length. Set to
%               1 if omitted.
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
      TV_GPCBB(w1,w2,f,lbd,NIT,GapTol,monotone,BBformula,fudge,cycleLength,verbose);

n=length(f);                %Assume a square image        
g=lbd*f;
gx = [g(:,2:n)-g(:,1:n-1), zeros(n,1)];
gy = [g(2:n,:)-g(1:n-1,:); zeros(1,n)];
sf = 0.5*lbd*sum(sum(f.^2));

% upper and lower bounds on line search parameter alpha
amin = 1.e-5; amax = 1.e5;
% choice of step length for the first iteration (must be in [amin,amax])
alpha = 1;   

if ~(BBformula==1 | BBformula==2)
  fprintf(1,'\nGPCBB: ERORR: illegal value for BBformula: %d\n', BBformula);
  return
end
if ~(fudge > 0.0 & fudge <= 1.0)
  fprintf(1,'\nGPCBB: ERROR: illegal value for fudge: %6.2e\n', fudge);
  return
end


% Set cycle length for cyclic variants of BB; steplength is recalculated 
% only every cycleLength steps. Set cycleLength = 1 for standard BB, 
% otherwise choose a larger integer. Default = 1(standard BB)
if (exist('cycleLength')==0)
    cycleLength = 1;
end

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
t0 = cputime;                %Start CPU clock

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for itr=1:NIT
  % gradient of the objective function
  dFx = [DivW(:,1:n-1)-DivW(:,2:n), zeros(n,1)] + gx;
  dFy = [DivW(1:n-1,:)-DivW(2:n,:); zeros(1,n)] + gy;
  
  w1_old = w1;
  w2_old = w2;
  w1 = w1_old - alpha * dFx;
  w2 = w2_old - alpha * dFy;
  
  %apply gradient projection to ensure constraints
  %let w_{i,j}=w_{i,j}/|w_{i,j}| if |w_{i,j}|>1
  wnorm= max(1, sqrt(w1.^2+w2.^2));
  w1 = w1./wnorm;
  w2 = w2./wnorm;
  
  dw1 = w1-w1_old;    %search direction satisfying constraints 
  dw2 = w2-w2_old;
  
  DivdW=([dw1(:,1),dw1(:,2:n)-dw1(:,1:n-1)] + ...
      [dw2(1,:);dw2(2:n,:)-dw2(1:n-1,:)]);
  
  % in the monotone version, calculate the step gamma along the (feasible)
  % line segment by limited minimization
  if monotone==1
    gamma = -sum(sum(dw1.*dFx + dw2.*dFy))/sum(sum(DivdW.^2)); 
    gamma = max(0,min(gamma,1));    
    w1 = w1_old + gamma*dw1;
    w2 = w2_old + gamma*dw2;
  end
    
  % compute step length alpha for next iteration, if it's time to start a
  % new cycle
  if mod(itr-1,cycleLength)==0
    if BBformula==1
      alpha = sum(sum(dw1.^2 + dw2.^2))/sum(sum(DivdW.^2));
    elseif BBformula==2
      % "alternative" BB formula - some extra work needed
      gwx = [DivdW(:,2:n)-DivdW(:,1:n-1), zeros(n,1)];
      gwy = [DivdW(2:n,:)-DivdW(1:n-1,:); zeros(1,n)];
      alpha = sum(sum(DivdW.^2))/sum(sum(gwx.^2 + gwy.^2));
    end
    % scale by fudge factor
    alpha = fudge*alpha;
    % restrict to interval [amin,amax]
    alpha = min( max(amin, alpha), amax); 
  end
  
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
  Dgap(itr+1)=Dgap(itr+1) / (abs(PriVal)+abs(DualVal));
  if verbose
    fprintf(1,' GPCBB cycle %2d iter %4d: Obj=%11.6e, alpha=%6.2e, rel dgap=%7.3e\n', ...
	cycleLength, itr, DualVal, alpha, Dgap(itr+1));
  end
  
  if (Dgap(itr+1) < GapTol )
    if verbose
      fprintf(1,'GPCBB cycle %2d, convergence tolerance reached: %6.2e\n',...
	  cycleLength, Dgap(itr+1));
    end
    break
  end
end
