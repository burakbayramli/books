%Last Updated on 10-09-2008

% Gradient projection algorithm with Barzilai-Borwein steplengths,
% optional safeguarding (by insisting on sufficient decrease over 
% a span of nDecr iterations) and backtracking. 

% Dual Formulation of TV model:
% min || div w - \lbd f|| subject to |w| <= 1

% \div : divergence,  \g : gradient
%-------------------------------------------------------------------------
% Input variables
%-------------------------------------------------------------------------
% w1,w2:        Dual variable, initial guess. 
% f:            noisy image
% lbd:          Constant fidelity parameter. 
% nDecr:        span of iterations over which we insist on a
%                 decrease. (0=never insist)
% BBformula:    1=standard BB formula, 2=alternative BB formula.
% fudge:        fudge factor for scaling of the BB alpha.
% NIT:          Maximum number of iterations
% GapTol:       Convergence tol (stop criterion) for relative duality gap
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
      TV_GPBBsafe(w1,w2,f,lbd,nDecr,BBformula,fudge,NIT,GapTol,verbose);

  n=length(f);                    %Assume a square image        
  g=lbd*f;
  gx = [g(:,2:n)-g(:,1:n-1), zeros(n,1)];
  gy = [g(2:n,:)-g(1:n-1,:); zeros(1,n)];
  sf = 0.5*lbd*sum(sum(f.^2));
  
  % initialize
  beta = 0.5;       % backtracking factor
  mu = 1.e-4;       % backtracking line search parameter; 
  % limits on initial choice of step at each iteration
  amin = 1.e-5; amax = 1.e5; 
  % initial choice of alpha for first iteration
  alpha = 1.0;
  
  if ~(BBformula==1 | BBformula==2)
    fprintf(1,'\nGPCBB: ERORR: illegal value for BBformula: %d\n', BBformula);
    return
  end
  if ~(fudge > 0.0 & fudge <= 1.0)
    fprintf(1,'\nGPCBB: ERROR: illegal value for fudge: %6.2e\n', fudge);
    return
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
  TimeCost(1) = 0;
  t0 = cputime;                    %Start CPU clock    
  
  for itr=1:NIT
    %gradient of objective function
    dFx = [DivW(:,1:n-1)-DivW(:,2:n), zeros(n,1)] + gx;
    dFy = [DivW(1:n-1,:)-DivW(2:n,:); zeros(1,n)] + gy;
    
    w1_old = w1;
    w2_old = w2;
    % get reference objective value (over which we insist on a decrease):
    if itr>nDecr
      Energy_ref = max(Energy(itr-nDecr:itr));
    else
      Energy_ref = realmax;
    end
    
    %Perform line search to ensure decrease in energy.
    while 1
      w1 = w1_old - alpha * dFx;
      w2 = w2_old - alpha * dFy;
      %Project w to the feasibility set
      wnorm= max(1, sqrt(w1.^2+w2.^2));
      w1 = w1./wnorm;
      w2 = w2./wnorm;
      %Compute new energy
      DivW = ([w1(:,1),w1(:,2:n)-w1(:,1:n-1)] + ...
	  [w2(1,:);w2(2:n,:)-w2(1:n-1,:)]); 
      Energy_new = 0.5*sum(sum((DivW-g).^2)); 
      dw1=w1-w1_old; dw2=w2-w2_old;
      if ( Energy_new <= (Energy_ref + mu*sum(sum(dw1.*dFx + dw2.*dFy))))
	break;
      else
	if verbose
	  fprintf(1,' reducing alpha\n');
	end
	alpha = beta*alpha;
      end
    end
    
    DivdW=([dw1(:,1),dw1(:,2:n)-dw1(:,1:n-1)] + ...
      [dw2(1,:);dw2(2:n,:)-dw2(1:n-1,:)]);
  
    % compute step length alpha for next iteration
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
    DgapRel = Dgap(itr+1)/(abs(PriVal)+abs(DualVal));
    if verbose
      fprintf(1,' GPBBsafe iter %4d: Obj=%11.6e, alpha=%6.2e, rel dgap=%7.3e\n', ...
	  itr, DualVal, alpha, DgapRel);
    end
    
    if (DgapRel < GapTol )
      if verbose
	fprintf(1,'GPBBsafe nDecr %2d, convergence tolerance reached: %6.2e\n',...
	    nDecr, DgapRel);
      end
      break
    end
  end
