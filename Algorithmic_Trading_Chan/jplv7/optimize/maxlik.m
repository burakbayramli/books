function result=maxlik(func,b,info,varargin)
% PURPOSE: minimize a log likelihood function
% ------------------------------------------------------------------
% USAGE:     result = maxlike(func,b,info,varargin)
%        or: result = maxlike(func,b,[],varargin) for default options 
% Where: func    = function to be minimized 
%        b       = parameter vector fed to func       
%        info structure containing optimization options
%        .delta  = Increment in numerical derivs                   [.000001]
%        .hess   = Hessian method: ['dfp'], 'bfgs', 'gn', 'marq', 'sd'
%        .maxit  = Maximium iterations                             [100]
%        .lambda = Minimum eigenvalue of Hessian for Marquardt     [.01]
%        .cond   = Tolerance level for condition of Hessian        [1000]
%        .btol   = Tolerance for convergence of parm vector        [1e-4]
%        .ftol   = Tolerance for convergence of objective function [sqrt(eps)] 
%        .gtol   = Tolerance for convergence of gradient           [sqrt(eps)]
%        .prt    = Printing: 0 = None, 1 = Most, 2 = All           [0]
%       varargin = arguments list passed to func
% ------------------------------------------------------------------
% RETURNS: results = a structure variable with fields:
%           .b     = parameter value at the optimum
%           .hess  = numerical hessian at the optimum
%           .bhist = history of b at each iteration
%           .f     = objective function value at the optimum
%           .g     = gradient at the optimum
%           .dg    = change in gradient
%           .db    = change in b parameters
%           .df    = change in objective function
%           .iter  = # of iterations taken
%           .meth  = 'dfp', 'bfgs', 'gn', 'marq', 'sd' (from input)
%           .time  = time (in seconds) needed to find solution
% ------------------------------------------------------------------
% NOTES: This is really Mike Cliff's minz function converted for usage 
%        with the Econometrics Toolbox.
% calls stepz, numz, hessz functions
% ------------------------------------------------------------------

% written by:
% Mike Cliff,  UNC Finance   mcliff@unc.edu
% CREATED  12/8/98
% UPDATED  1/24/99

% changed for consistency with other
% Econometrics Toolbox optimization routines
% by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com
% last modified 4/14/2000

infoz.func = func;

% set defaults
infoz.maxit = 100;
infoz.hess = 'bfgs';
infoz.prt = 0;
infoz.cond = 1000;
infoz.btol = 1e-4;
infoz.gtol = sqrt(eps);
infoz.ftol = sqrt(eps);
infoz.lambda = 0.01;
infoz.H1 = 1;
infoz.delta = .000001;
infoz.call = 'other';
infoz.step = 'stepz';
infoz.grad='numzz'; 
hessfile = 'hesszz';


if length(info) > 0
  if ~isstruct(info)
    error('maxlik: options should be in a structure variable');
  end;
% parse options
fields = fieldnames(info);
nf = length(fields); xcheck = 0; ycheck = 0;
  for i=1:nf
    if strcmp(fields{i},'maxit')
        infoz.maxit = info.maxit; 
    elseif strcmp(fields{i},'btol')
        infoz.btol = info.btol;
    elseif strcmp(fields{i},'gtol')
        infoz.gtol = info.gtol;
    elseif strcmp(fields{i},'ftol')
        infoz.ftol = info.ftol;
    elseif strcmp(fields{i},'hess')
        infoz.hess = info.hess;
    elseif strcmp(fields{i},'cond')
        infoz.cond = info.cond;
    elseif strcmp(fields{i},'lambda')
        infoz.lambda = info.lambda;
    elseif strcmp(fields{i},'delta')
        infoz.delta = info.delta;
    elseif strcmp(fields{i},'prt')
        infoz.prt = info.prt;        
    end;
  end;
else
% rely on default options
end;

 
lvar = length(varargin);
stat.iter = 0;
k = rows(b); 
if lvar > 0
n = rows(varargin{1});
end;
convcrit = ones(4,1);
stat.Hi = []; 
stat.df = 1000; 
stat.db = ones(k,1)*1000; 
stat.dG = stat.db;
func = fcnchk(infoz.func,lvar+2);
grad = fcnchk(infoz.grad,lvar+1);
hess = fcnchk(hessfile,lvar+2);
step = fcnchk(infoz.step,lvar+2);

stat.f = feval(func,b,varargin{:});
stat.G = feval(grad,b,infoz,stat,varargin{:});

stat.star = ' ';  
stat.Hcond = 0;

%====================================================================
%   MINIMIZATION LOOP
%====================================================================

if infoz.prt > 0
% set up row-column formatting for mprint of intermediate results
in0.fmt = strvcat('%5d','%16.8f','%16.8f');
% this is for infoz.prt = 1 (brief information)
in1.cnames = strvcat('iteration','function value','dfunc');
in1.fmt = strvcat('%5d','%16.8f','%16.8f');
% this is for infoz.prt = 2
Vname = 'Parameter';
 for i=1:k
 tmp = ['Parameter ',num2str(i)];
 Vname = strvcat(Vname,tmp);
 end;
in2.cnames = strvcat('Estimates','dEstimates','Gradient','dGradient');
in2.rnames = Vname;
in2.fmt = strvcat('%16.8f','%16.8f','%16.8f','%16.8f');    
end

if infoz.prt == 1
mprint([stat.iter stat.f stat.df],in1);
end;
if infoz.prt == 2
mprint([stat.iter stat.f stat.df],in1);
mprint([b stat.db stat.G stat.dG],in2);
end;

t0 = clock;
while all(convcrit > 0)
% Calculate grad, hess, direc, step to get new b
  stat.iter = stat.iter + 1;
  stat = feval(hess,b,infoz,stat,varargin{:});
  stat.direc = -stat.Hi*stat.G;
  alpha  = feval(step,b,infoz,stat,varargin{:});
  stat.db = alpha*stat.direc; 
  b = b + stat.db;

% Re-evaluate function, display current status
  f0 = stat.f;     G0 = stat.G;
 if strcmp(infoz.call,'other'),
  stat.f = feval(func,b,varargin{:});
  stat.G = feval(grad,b,infoz,stat,varargin{:});
 else
  stat.f = feval(func,b,infoz,stat,varargin{:});
  stat.G = feval(grad,b,infoz,stat,varargin{:});
 end;

% Determine changes in func, grad, and parms
  if stat.f == 0
    stat.df = 0;
  else
    stat.df = (f0-stat.f)/abs(stat.f);
  end
  stat.dG = stat.G-G0;
  dbcrit = any(abs(stat.db)>infoz.btol*ones(k,1));
  dgcrit = any(abs(stat.dG)>infoz.gtol*ones(k,1));
  convcrit = [(infoz.maxit-stat.iter); (stat.df-infoz.ftol);...
    dgcrit];
  %if stat.df < 0, error('Objective Function Increased'); end
  X(stat.iter,:) = b';

% print intermediate results
if infoz.prt == 1
mprint([stat.iter stat.f stat.df],in1);
end;
if infoz.prt == 2
mprint([stat.iter stat.f stat.df],in1);
mprint([b stat.db stat.G stat.dG],in2);
end;

end
time = etime(clock,t0);


%====================================================================
%   FINISHING STUFF
%====================================================================

% Write a message about why we stopped

if infoz.prt > 0
  if convcrit(1) <= 0
    critmsg = 'Maximum Iterations';
  elseif convcrit(2) <= 0
    critmsg =  'Change in Objective Function';
  elseif convcrit(3) <= 0
    critmsg = 'Change in Parameter Vector';
  elseif convcrit(4) <= 0
    critmsg = 'Change in Gradient';  
  end
  disp(['  CONVERGENCE CRITERIA MET: ' critmsg])
  disp(' ')
end

% put together results structure information
result.bhist = X;
result.time = time;
result.b = b;
result.g = stat.G;
result.dg = stat.dG;
result.f = stat.f;
result.df = stat.df;
result.iter = stat.iter;
result.meth = infoz.hess;
% Calculate numerical hessian at the solution
result.hess = fdhess(func,b,varargin{:});


