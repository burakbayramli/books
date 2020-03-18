% Last Updated on 05-21-2008

% This program applies Mingqiang's PDHG algorithm to solve the
% ROF image restoration model

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
% Energy:           The value of the dual objective function
% DGap:             Duality Gap
% TimeCost:        CPU time cost
% itr:              number of iterations

function [u, w1,w2, Energy, Dgap, TimeCost, itr] = ...
      TV_PDHG(w1,w2,f,lbd,NIT,GapTol,verbose);

n=length(f);                %Assume a square image        
g=lbd*f;
gx = [g(:,2:n)-g(:,1:n-1), zeros(n,1)];
gy = [g(2:n,:)-g(1:n-1,:); zeros(1,n)];
sf = 0.5*lbd*sum(sum(f.^2));

% Compute energy
DivW=([w1(:,1),w1(:,2:n)-w1(:,1:n-1)] + [w2(1,:);w2(2:n,:)-w2(1:n-1,:)]); 
Dual(1) = (lbd/2.)*sum(sum(f.^2 - (1.0/lbd^2)*(DivW-g).^2)); 
Energy(1) = Dual(1);
  
%Compute the primal u and the duality gap
u  = f;
ux = [u(:,2:n)-u(:,1:n-1), zeros(n,1)];
uy = [u(2:n,:)-u(1:n-1,:); zeros(1,n)];
gu_norm = sqrt(ux.^2+uy.^2);
Primal(1) = sum(sum(gu_norm + (lbd/2)*(u-f).^2));
Dgap(1)=(Primal(1)-Dual(1)) / (abs(Primal(1)) + abs(Dual(1)));

if verbose
  fprintf(' Initial: Pri=%8.3e, Dua=%8.3e, Gap=%5.2e\n', ...
      Primal(1), Dual(1), Dgap(1));
end

TimeCost(1)=0;
t0 = cputime;                %Start CPU clock

for itr=1:NIT
  
  % choose tau
  tau=2.0;
  tau = 0.2 + 0.08*itr;
  w1 = w1 - tau*lbd*ux; w2 = w2 - tau*lbd*uy;
  %apply gradient projection to ensure constraints
  %let w_{i,j}=w_{i,j}/|w_{i,j}| if |w_{i,j}|>1
  wnorm= max(1, sqrt(w1.^2+w2.^2));
  w1 = w1./wnorm;
  w2 = w2./wnorm;

  DivW = ([w1(:,1),w1(:,2:n)-w1(:,1:n-1)] + [w2(1,:);w2(2:n,:)-w2(1:n-1,:)]); 
  Dual(itr+1) = (lbd/2.)*(sum(sum(f.^2)) - (1.0/lbd^2)*sum(sum((DivW-g).^2))); 
  Energy(itr+1) = Dual(itr+1);
  
  %Compute the primal u and the duality gap
  % choose theta
  theta = 0.2;
  theta = (0.5 - 5.0/(15.0+itr)) / tau;
  u= (1.0-theta)*u + theta*(f - (1/lbd)*DivW);   
  ux = [u(:,2:n)-u(:,1:n-1), zeros(n,1)];
  uy = [u(2:n,:)-u(1:n-1,:);zeros(1,n)];
  gu_norm = sqrt(ux.^2+uy.^2);
  Primal(itr+1) = sum(sum(gu_norm + (lbd/2)*(u-f).^2));
  
  Dgap(itr+1)= (Primal(itr+1)-Dual(itr+1)) / ...
      (abs(Primal(itr+1)) + abs(Dual(itr+1)));
  
  TimeCost(itr+1)=cputime-t0;
  
  if verbose
    fprintf(' PDHG itr %4d: Pri=%8.3e, Dua=%8.3e, Gap=%5.2e\n', itr, ...
	Primal(itr+1), Dual(itr+1), Dgap(itr+1));
  end
  % test for convergence stop criterion:  (Primal-Dual)/Dual < tol
  % Dual Energy = 0.5*lbd*||f||^2 - (1/lbd)*||\div w - lbd *f||^2
  if ( Dgap(itr+1) < GapTol )
    if verbose
      fprintf(1,' PDHG convergence tolerance %7.3e satisfied\n', GapTol);
    end 
    break
  end   
end

