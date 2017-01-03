function stat=hessz(b,infoz,stat,varargin)
% PURPOSE: Calculate/update Inverse Hessian 
% ------------------------------------------------------
% USAGE: stat=hessz(b,infoz,stat,varargin)
% Where: b          = parameter vector fed to func
%        infoz       = structure from MINZ
%        stat       = status structure from MINZ
%        varargin   = arguments list passed to func
% ------------------------------------------------------
% RETURNS: stat = updated status structure with new 
%          inverse Hessian
% ------------------------------------------------------
% NOTES: Supports the following search direction algorithms:
%         *  Steepest Descent (SD)
%         *  Gauss-Newton (GN)
%         *  Levenberg-Marquardt (MARQ)
%         *  Davidon-Fletcher-Powell (DFP)
%         *  Broyden-Fletcher-Goldfarb-Shano (BFGS)
%
%==================================================================
%   REFERENCES:
%     Gill, Murray, Wright (1981)
%     Numerical Recipes in FORTRAN
%     Optimization notes from XX (UNC OR Dept.)
%==================================================================
% VERSION: 1.1.4  (9/23/00)

% written by;
% Mike Cliff,  Purdue Finance  mcliff@mgmt.purdue.edu
% CREATED: 12/8/98
% UPDATED: 11/19/99 (1.1.2 safeguarding in eval eig, cond)
%          7/24/00  (1.1.3 scaling of gnbase)
%          9/23/00  (1.1.4 fcnchk)
  
%==================================================================
%   INITIALIZATIONS
%==================================================================

k = rows(b);
lvar = length(varargin);

if strcmp(infoz.call,'gmm') | strcmp(infoz.call,'ls')
  if strcmp(infoz.call,'gmm')
    wdum = 1;
  else
    wdum=0; 
  end
  momt = fcnchk(infoz.momt);
  jake = fcnchk(infoz.jake);
  m = feval(momt,b,infoz,stat,varargin{1:lvar-wdum});
  M = feval(jake,b,infoz,stat,varargin{1:lvar-wdum});
  if strcmp(infoz.call,'gmm')
    W = varargin{lvar};    
  else
    W = eye(rows(M));
  end
  gnbase = 2*M'*W*M;            % Scaling by 2 added 7/24/00
else
  gnbase = 2*eye(k);            % Could replace with some other pd matrix
end

dG = stat.dG; db = stat.db; Hi0 = stat.Hi;

%==================================================================
%   UPDATE INVERSE HESSIAN BY CASE
%==================================================================

switch infoz.hess

case 'sd'                          % Steepest Descent
  Hi = eye(k);

case {'gn','marq'}                 % GN/Marq directions
  H = gnbase;
  if strcmp(infoz.hess,'marq')       % Marquardt    
    try 				% Safeguard against errors
      mineig = min(eig(gnbase));
      Hcond = sqrt(cond(gnbase));      
    catch				% If problem with eig(S)
      mineig = -Inf;
      Hcond = Inf;
    end        
    lambda = max(infoz.lambda,mineig);  % alternate criteria
%    lambda = infoz.lambda;
    while Hcond > infoz.cond
      H = gnbase + lambda*eye(k);
      try
        Hcond=sqrt(cond(H));
      catch
	Hcond = Inf;
      end
      lambda = lambda*2;           % may be a better factor for increases
      if lambda > 100
	Hcond = -Inf;
	H = eye(k);                % Just use SD
      end      
    end
  end
  Hi = H\eye(k);

case {'dfp','bfgs'}               % DFP/BFGS 
  if isempty(stat.Hi)              
    if infoz.H1 == 1, Hi = eye(k); % Initial Hessian
    else, Hi = gnbase\eye(k); end
  else
    if db'*dG > sqrt(eps*(db'*db)*(dG'*dG))

% ------- Based on update of inverse given in Num. Recipes, p. 420 -------
      a = db*db'/(db'*dG);
      b = -Hi0*dG*dG'*Hi0'/(dG'*Hi0*dG);
      if strcmp(infoz.hess,'bfgs')             % c = [0] for DFP
        c = db/(db'*dG) - Hi0*dG/(dG'*Hi0*dG);
        c = dG'*Hi0*dG*c*c';
      else 
        c=zeros(k); 
      end
      Hi = Hi0 + a + b + c;
    else
      Hi = stat.Hi;
    end    
  end

otherwise
  error('UNKNOWN HESSIAN TYPE')
end


%==================================================================
%   CHECK CONDITIONING AND RETURN RESULT
%==================================================================

stat.Hcond = sqrt(cond(Hi));
if stat.Hcond > infoz.cond,  stat.star = '*';
else,  stat.star = ' '; end
stat.Hi = Hi;
