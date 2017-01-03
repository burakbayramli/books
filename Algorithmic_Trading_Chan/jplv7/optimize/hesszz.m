function stat=hesszz(b,infoz,stat,varargin)
% PURPOSE: Calculate/update Inverse Hessian 
% ------------------------------------------------------
% USAGE: stat=hesszz(b,infoz,stat,varargin)
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

% written by;
% Mike Cliff,  UNC Finance  mcliff@unc.edu
% CREATED: 12/8/98
% UPDATED: 1/24/99


%==================================================================
%   INITIALIZATIONS
%==================================================================

k = rows(b);
lvar = length(varargin);

if strcmp(infoz.call,'gmm') | strcmp(infoz.call,'ls')
  if strcmp(infoz.call,'gmm'), wdum = 1;
  else, wdum=0; end;
  momt = fcnchk(infoz.momt,3+(lvar-1)+wdum);
  jake = fcnchk(infoz.jake,3+(lvar-1)+wdum); % was lvar + 2
  m = feval(momt,b,infoz,stat,varargin{1:lvar-wdum});
  M = feval(jake,b,infoz,stat,varargin{1:lvar-wdum});
  if strcmp(infoz.call,'gmm')
    W = varargin{lvar};    
  else
    W = eye(rows(M));
  end
  gnbase = M'*W*M;
else
  gnbase = eye(k);            % Could replace with some other pd matrix
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
  if strcmp(infoz.hess,'marq')      % Marquardt
%    lambda = max(infoz.lambda,min(eig(gnbase)));  % alternate criteria
    lambda = infoz.lambda;
    Hcond = cond(gnbase);
    while Hcond > infoz.cond
      H = gnbase + lambda*eye(k);
      Hcond=cond(H);
      lambda = lambda*2;           % may be a better factor for increases
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

stat.Hcond = cond(Hi);
if stat.Hcond > infoz.cond,  stat.star = '*';
else,  stat.star = ' '; end
stat.Hi = Hi;
