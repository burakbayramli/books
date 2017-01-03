function [b,infoz,stat]=minz(b,func,infoz,varargin)

% PURPOSE: minimize general function.  Special applications
%           are least-squares/GMM and MLE
% ------------------------------------------------------------------
% USAGE: [b,infoz,stat]=minz(b,func,infoz,varargin)
% Where: b		parameter vector fed to func
%	 func		function to minimize.  'lsfunc' for GMM/LS
% infoz structure
%   infoz.call   Calling program: 'gmm', 'ls', 'mle', 'other'
%   infoz.func   What to min: 'lsfunc' for LS/GMM
%   infoz.momt   Orthog. conditions m of m'Wm for GMM
%   infoz.jake   Jacobian of momt
%   infoz.grad   Gradient: 'gradfile' for analytic, else       ['numz']
%   infoz.delta  Increment in numerical derivs                [.000001]
%   infoz.hess   Hessian: ['dfp'], 'bfgs', 'gn', 'marq', 'sd'
%   infoz.H1     Initial Hessian in DFP/BFGS.  [1] = eye, else evaluate
%   infoz.maxit  Maximium iterations                              [100]
%   infoz.step   step size routine                            ['step2']
%   infoz.lambda Minimum eigenvalue of Hessian for Marquardt     [.001]
%   infoz.cond   Tolerance level for condition of Hessian        [1000]
%   infoz.btol   Tolerance for convergence of parm vector        [1e-4]
%   infoz.ftol   Tolerance for convergence of objective function [1e-7] 
%   infoz.gtol   Tolerance for convergence of gradient           [1e-7]
%   infoz.prt    Printing: 0 = None, 1 = Screen, higher = file   [1]
%
% varargin   = arguments list passed to func
% ------------------------------------------------------------------
% RETURNS: b    = optimum parameter vector
%        infoz   = updated version of input structure
%
% stat structure
%   stat.G      Gradient
%   stat.f      Objective function value
%   stat.Hi     Inverse Hessian
%   stat.dG     Change in Gradient
%   stat.db     Change in Parm vector
%   stat.df     Change in Objective Function
%   stat.Hcond  Condition number for current Hessian
%   stat.hist   History of b at each iteration
% ------------------------------------------------------------------
%
%====================================================================
%   NOTES
%     W is included as last element of varargin for GMM
%====================================================================
%VERSION: 1.1.5 (11/10/02)

% written by:
% Mike Cliff,  Purdue Finance   mcliff@mgmt.purdue.edu
% CREATED  12/8/98
% UPDATED  11/14/99 (1.1.1 ftol; btol/gtol now relative, alpha on output)
%           5/17/00 (1.1.2 Trap for Imaginary Parm Vector)
%           9/23/00 (1.1.3 fcnck)
%           5/15/01 (1.1.4 Changed hmsg to print for fid = 1 also.) 
%          11/10/02 (1.1.5 Updated possible error around line 168)

%====================================================================
%   CHECK INFOZ STRUCTURE
%====================================================================

if ~isstruct(infoz)
  error('MINZ options should be in a structure variable');
end;

infoz.func = func;

if ~isfield(infoz,'call'), infoz.call='other'; end
if ~isfield(infoz,'prt'), infoz.prt=1; end

hmsg = [];
if ~isfield(infoz,'maxit')
  infoz.maxit = 100; 
  hmsg = strvcat(hmsg,'Maximum Iterations Set to 100');
end
if ~isfield(infoz,'hess')
  infoz.hess = 'dfp'; 
  hmsg = strvcat(hmsg,'Hessian Type Set to DFP in HESSZ.M'); 
end
if ~isfield(infoz,'step')
  infoz.step = 'step2';
  hmsg = strvcat(hmsg,'Line Minimization Using STEP2.M'); 
end
if ~isfield(infoz,'cond')
  infoz.cond = 1000; 
  hmsg = strvcat(hmsg,'Ill-Conditioning Tolerance Set to 1000'); 
end
if ~isfield(infoz,'btol')
  infoz.btol = 1e-4; 
  hmsg = strvcat(hmsg,'Parameter Convergence Tolerance Set to 1e-4'); 
end
if ~isfield(infoz,'ftol')
  infoz.ftol = 1e-7; 
  hmsg = strvcat(hmsg,'Objective Function Convergence Tolerance Set to 1e-7');
end
if ~isfield(infoz,'gtol')
  infoz.gtol = 1e-7; 
  hmsg = strvcat(hmsg,'Gradient Convergence Tolerance Set to 1e-7'); 
end  
if (~isfield(infoz,'lambda') & strcmp(infoz.hess,'marq'))
  infoz.lambda = .001; 
  hmsg = strvcat(hmsg,'LAMBDA SET TO .01'); 
end
if (strcmp(infoz.hess,'dfp') | strcmp(infoz.hess,'bfgs'))
  if ~isfield(infoz,'H1')
    infoz.H1=1; 
    hmsg = strvcat(hmsg,'INITIAL HESSIAN = I'); 
  end
end
if strcmp(infoz.step,'step2')
  if ~isfield(infoz,'stepred'), infoz.stepred=.9; end
end
if infoz.prt >= 1
  for i = 1:rows(hmsg)
    fprintf(infoz.prt,[hmsg(i,:) '\n']);
  end
end
  
lvar = length(varargin);

if strcmp(infoz.call,'gmm') | strcmp(infoz.call,'ls')
  if ~isfield(infoz,'momt'), error('MISSING MOMENT CONDITION FILE'); end  
  if ~isfield(infoz,'jake')
    error('MISSING JACOBIAN OF MOMENT CONDITIONS'); end    
  if ~isfield(infoz,'func'), infoz.func='lsfunc'; end
  if ~isfield(infoz,'grad'), infoz.grad='lsgrad'; end
  if ~strcmp(infoz.func,'lsfunc')
    error('LS or GMM Require Using LSFUNC as Objective Function')
  end
  if ~strcmp(infoz.grad,'lsgrad')
    error('LS or GMM Require Using LSGRAD as Gradient of Obj. Fcn.')
  end
  lsflag = 1;
else
  lsflag = 0;
end

if ~isfield(infoz,'func'), error('MISSING FUNCTION NAME'); end
if ~isfield(infoz,'grad')
  fprintf(infoz.prt,'Using Numerical Derivatives\n');
  infoz.grad='numz'; 
end

%====================================================================
%   INITIALIZATIONS
%====================================================================

switch infoz.hess
  case {'dfp','bfgs','gn','marq','sd'}
    hessfile = 'hessz';
  otherwise, hessfile = infoz.hess;
end

stat.iter = 0;
k = rows(b); n = rows(varargin{1});
convcrit = ones(4,1);
stat.Hi = []; 
stat.df = 1000; stat.db = ones(k,1)*1000; stat.dG = stat.db;
func = fcnchk(infoz.func);
grad = fcnchk(infoz.grad);
hess = fcnchk(hessfile);
step = fcnchk(infoz.step);
%if lsflag == 1
%  momt = fcnchk(infoz.momt);
%  jake = fcnchk(infoz.jake);
%end

if strcmp(infoz.call,'other')
  %stat.f = feval(func,b,varargin{:});
stat
  stat.f = feval(func,b,infoz,stat,varargin{:});
  stat.G = feval(grad,b,infoz,stat,varargin{:});
else
  stat.f = feval(func,b,infoz,stat,varargin{:});
  stat.G = feval(grad,b,infoz,stat,varargin{:});
end
%f0 = stat.f;  G0=stat.G;  
stat.star = ' ';  stat.Hcond = 0;

%====================================================================
%   MINIMIZATION LOOP
%====================================================================

if infoz.prt > 0
  fprintf(infoz.prt,...
    '\n ---------------------------------------------------------------\n');
  fprintf(infoz.prt,...
    ['\n        ITER      cond(H)  *    Step' blanks(17) 'Obj Fcn \n']); 
end

while all(convcrit > 0)
% --- Calculate grad, hess, direc, step to get new b -----------------
  stat.iter = stat.iter + 1;
  stat = feval(hess,b,infoz,stat,varargin{:});
  stat.direc = -stat.Hi*stat.G;
  alpha  = feval(step,b,infoz,stat,varargin{:});
  stat.db = alpha*stat.direc; 
  if ~isreal(stat.db)
    disp(b')
    error('Parm Vector Not Real (Last Good b Above)'); 
  end
  b = b + stat.db; 

% --- Re-evaluate function, display current status -------------------
  f0 = stat.f;     G0 = stat.G;
  stat.f = feval(func,b,infoz,stat,varargin{:});
  stat.G = feval(grad,b,infoz,stat,varargin{:});
  if infoz.prt > 0
    fprintf(infoz.prt,'\t%3.0f     %10.2e %s  %7.6f  \t%14.10f\n',...
      stat.iter,stat.Hcond,stat.star,alpha,stat.f);
  end
  
% --- Determine changes in func, grad, and parms -------------------
  if stat.f == 0
    stat.df = 0;
  else
    stat.df = f0/stat.f - 1;
  end
  stat.dG = stat.G-G0;
  dbcrit = any(abs(stat.db)>infoz.btol*b);	% Changed 11/14/99
  dgcrit = any(abs(stat.dG)>infoz.gtol*G0);	% was tol*ones()
  if stat.iter > 1
    convcrit = [(infoz.maxit-stat.iter); (stat.df-infoz.ftol);...
      dbcrit; dgcrit];
  end
  if stat.df < 0
    error(['Objective Function Increased by ' num2str(-stat.df,'%15.10f')]); 
  end
  X(stat.iter,:) = b';

end


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
  fprintf(infoz.prt,'  CONVERGENCE CRITERIA MET: %s\n',critmsg);
end

stat.hist = X;

%============================================================================
%  GET FINAL STANDARD ERRORS, EVALUATED AT SOLUTION
%    Use Analytic Hessian if Model is linear, else GN Hessian
%============================================================================

if strcmp(infoz.call,'ls')
  ahdum = 0;
  if (isfield(infoz,'jake') & lsflag == 1)
    if strcmp(infoz.jake,'lingmmj'), ahdum = 1; end
  end

  if ahdum == 1
    stat = lingmmh(b,infoz,stat,varargin{:});
  else
    infoz2=infoz;
    infoz2.hess='gn';
    stat = feval(hess,b,infoz2,stat,varargin{:});
  end
end
