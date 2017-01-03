function alpha=step2(b,infoz,stat,varargin)
% PURPOSE: Determine step size in NUMZ package
%-----------------------------------------------------------------
% USAGE: alpha=step(b,infoz,stat,varargin)
%  Where
%  b         vector of model parameters
%  infoz      structure variable with settings for MINZ
%  infoz.stepred controls step size reduction [.9 default]
%  stat      structure variable with minimization status
%  varargin  Variable list of arguments passed to func
%
% RETURNS:   alpha     scalar step size
%-----------------------------------------------------------------
%VERSION: 1.3  (1/10/03)

% written by:
% Mike Cliff,  Purdue Finance  mcliff@mgmt.purdue.edu
% CREATED:  1/24/99
% UPDATED: 11/6/99 (stepred as input [.9], robustnes)
%           7/24/00 (1.2 Added maxiter error printing)
%           9/23/00 (1.2.1 fcnchk)
%           1/10/03 (1.3 changed calc of maxit)

%=================================================================
%  INITIALIZATIONS
%=================================================================

if ~isfield(infoz,'stepred')
  stepred = .9;				% Factor for reducing stepsize
else
  stepred = infoz.stepred;
end

%maxit = log(sqrt(eps))/log(stepred);	% Set a cap on # iterations
maxit = log(eps)/log(stepred);	% Set a cap on # iterations

lvar = length(varargin);
func = fcnchk(infoz.func);
direc = stat.direc;
alpha = 1;
go = 1;
b0 = b;
f0 = stat.f;
ct = 1;

while go == 1
  b = b0 + alpha*direc;
  f = feval(func,b,infoz,stat,varargin{:});
  if ~isfinite(f)				% In case f blows up
    alpha = alpha*stepred;
    f = f0*2;
  end
  if f > f0
    alpha = alpha*stepred;
  else
    go = 0;
  end
  ct = ct + 1;
  if ct > maxit
    alpha = 0;
    go = 0;
    fprintf(infoz.prt,...
	    'Max Line Search Iterations (%i5) Exceeded in STEP2',maxit);
  end
end


% --- Uncomment for diagnostics --------------------------------
%fprintf(1,'Step size = %10.8f   %%Red in Obj Fcn %10.8f\n',alpha,f/f0);	
