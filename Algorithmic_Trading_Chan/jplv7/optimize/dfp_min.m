function result = dfp_min(f,pin,info,varargin)
% PURPOSE: DFP minimization routine to minimize func 
%          (Converted from Numerical Recipes book dfpmin routine) 
%---------------------------------------------------
% USAGE: result = dfp_min(func,b,info,varargin)
%     or result = dfp_min(func,b,[],varargin)
%        (for the case of default options)
% Where: func   = likelihood function to minimize 
%        b      = parameter vector fed to func
%        info       = a structure with fields:
%        info.maxit = maximum # of iterations (default = 500)
%        info.tol   =  tolerance for convergence (default = sqrt(eps))
%        info.prt   = 1 for printing iterations, 0 for no printing
%        varargin   = list of arguments passed to function
%---------------------------------------------------
% RETURNS: a result structure
%        result.b     =  (kx1) minimizing vector
%        result.f     = value of func at solution values
%        result.hess  = hessian at solution values
%        result.iter = # number of iterations
%        result.time  = time taken
%        result.flag  = 0 for convergence, 1 for non-convergence
%---------------------------------------------------
% NOTE: func must take the form func(b,varargin)
%       where:     b = parameter vector (k x 1)
%           varargin = arguments passed to the function
%---------------------------------------------------    
% SEE ALSO: maxlik, qnewton, pow_min, frpr_min functions
%     NOTE: calls linmin(), gradnt(), hessian()
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

% default options
tol = sqrt(eps);;  maxit = 500; pflag = 0;

if length(info) > 0
  if ~isstruct(info)
    error('dfp_min: options should be in a structure variable');
  end;
% parse options
fields = fieldnames(info);
nf = length(fields); xcheck = 0; ycheck = 0;
  for i=1:nf
    if strcmp(fields{i},'maxit')
        maxit = info.maxit; 
    elseif strcmp(fields{i},'tol')
        tol = info.tol;
    elseif strcmp(fields{i},'prt')
        pflag = info.prt;        
    end;
  end;
else 
% rely on defaults
end;
   
funfcn = fcnchk(f,length(varargin));

epss = sqrt(eps);

[n junk] = size(pin);
xarg = pin;

deltagrad = tol;
fp = feval(funfcn,xarg,varargin{:});
g = gradnt(funfcn,xarg,deltagrad,varargin{:});

xi = -g';

hessn = eye(n);
iter = 1;

% these are for the case of printing intermediate results during iteration
input.cnames = strvcat('iteration','function value');
input.fmt = strvcat('%5d','%16.8f');
Vname = 'Parameter';
 for i=1:n
 tmp = ['Parameter ',num2str(i)];
 Vname = strvcat(Vname,tmp);
 end;
pinf.cnames = strvcat('Estimates','Gradient');
pinf.rnames = Vname;     

t0 = clock;

while (iter <= maxit)
 
 [pin fret] = linmin(pin,xi,tol,f,varargin{:});
 
 if pflag == 1 
 % print iteration results
             matprt = [iter fp];
             mprint(matprt,input);
             matprt = [pin xi];
             mprint(matprt,pinf);  
 end;
 
 if ((2*abs(fp-fret)) <= tol*(abs(fp) + abs(fret)+epss));
     pout = pin;
     fout = fret;
        hess = fdhess(funfcn,xarg,varargin{:});
     niter = iter;  
     result.b = pin;
     result.f = -fret;
     result.hess = fdhess(funfcn,pout,varargin{:});
     result.iter = niter; 
     result.time = etime(clock,t0);
     result.flag = 0;
  return;
 end;
    xarg = pin;
    fp = feval(funfcn,xarg,varargin{:}); 
 
    dg = g';
    xarg = pin;
    fret = feval(funfcn,xarg,varargin{:});
    g = gradnt(funfcn,xarg,deltagrad,varargin{:});
    
    dg = g' - dg;
    hdg = hessn*dg;
    fac = dg'*xi;
    fae = dg'*hdg;
    
    if (fac == 0)
     pout = pin;
     fout = fret;
        hess = fdhess(funfcn,xarg,varargin{:});
     niter = iter;
     result.b = pin;
     result.f = -fret;
     result.hess = fdhess(funfcn,pout,varargin{:});
     result.iter = niter; 
     result.time = etime(clock,t0);
     result.flag = 0;
     return;
    else
     fac = 1/fac;
    end;
    
    if (fae == 0)
     pout = pin;
     fout = fret;
     hess = hessn;     
     niter = iter;    
     result.b = pin;
     result.f = -fret;
     result.hess = fdhess(funfcn,pout,varargin{:});
     result.iter = niter; 
     result.time = etime(clock,t0);
     result.flag = 0; 
     return;
    else
     fad = 1/fae;
    end;
    
    dg = fac*xi - fad*hdg;
    hessn = hessn + fac*xi*xi' - fad*hdg*hdg' + fae*dg*dg';
    xi = -hessn*g';
    iter = iter + 1;
end;

     result.b = pin;
     result.f = -fret;
     result.hess = fdhess(funfcn,pout,varargin{:});
     result.iter = niter; 
     result.time = etime(clock,t0);
     result.flag = 1;

   
  
  
