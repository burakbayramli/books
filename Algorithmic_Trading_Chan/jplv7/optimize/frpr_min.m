function result = frpr_min(f,pin,info,varargin)
% PURPOSE: Fletcher,Reeves,Polak,Ribiere minimization routine to minimize func
%          (Converted from Numerical Recipes book frprmn routine) 
%---------------------------------------------------
% USAGE: result = frpr_min(func,b,info,varargin)
% where: func   = likelihood function to minimize 
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
% SEE ALSO: dfp_min, pow_min functions
%     NOTE: calls linmin() ,gradnt(), hessian()
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

  if ~isstruct(info)
    error('frpr_min: options should be in a structure variable');
  end;
% default options
tol = sqrt(eps);;  maxit = 500; pflag = 0;
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

funfcn = fcnchk(f,length(varargin));
     

epss = sqrt(eps);

[n junk] = size(pin);
xarg = pin;
deltagrad = tol;
fp = feval(funfcn,xarg,varargin{:});
xi = gradnt(funfcn,xarg,deltagrad,varargin{:});


g = -xi';
h = g;
xi = h;

iter = 0;
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
 iter = iter+1;
 
  [pin fret] = linmin(pin,xi,tol,f,varargin{:});

  if pflag == 1 
         % print iteration results
                matprt = [iter fp ];
                mprint(matprt,input);
                matprt = [pin xi];
                mprint(matprt,pinf);  
           end;
 
  if ((2*abs(fp-fret)) <= tol*(abs(fp) + abs(fret)+epss));
             pout = pin;
             fout = fret;
                hess = hessian(funfcn,xarg,varargin{:});
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
    xi = gradnt(funfcn,xarg,deltagrad,varargin{:});
    
    
    gg = g'*g;
    dgg = (xi'+g)'*xi';
    
    if (gg == 0)
     pout = pin;
     fout = fret;
        hess = hessian(funfcn,xarg,varargin{:});
     niter = iter;  
     result.b = pin;
     result.f = -fret;
     result.hess = fdhess(funfcn,pout,varargin{:});
     result.iter = niter; 
     result.time = etime(clock,t0);
     result.flag = 0;

  return;
 end;
 
 gam = dgg/gg;
 g = -xi';
 h = g + gam*h;
 xi = h;
 
end; % end of while


   niter = iter; 

     result.b = pin;
     result.f = -fret;
     result.hess = fdhess(funfcn,pout,varargin{:});
     result.iter = niter; 
     result.time = etime(clock,t0);
     result.flag = 1;
   
