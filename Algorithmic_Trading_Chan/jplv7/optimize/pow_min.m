function result = pow_min(f,pin,info,varargin)
% PURPOSE: Powell minimization routine to minimize func
%          (Converted from Numerical Recipes book powell routine) 
%---------------------------------------------------
% USAGE: result = pow_min(func,b,info,varargin)
%     or result = pow_min(func,b,[],varargin)
%        (to rely on default options)
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
% SEE ALSO: dfp_min, frpr_min functions
%     NOTE: calls linmin(), hessian()
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
    error('pow_min: options should be in a structure variable');
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
% rely on default options
end;

funfcn = fcnchk(f,length(varargin));
   
epss = sqrt(eps);

[n junk] = size(pin);
xarg = pin;
fret = feval(funfcn,xarg,varargin{:});
% for printing iteration information
input.cnames = strvcat('iteration','function value');
input.fmt = strvcat('%5d','%16.8f');
Vname = 'Parameter';
 for i=1:n
 tmp = ['Parameter ',num2str(i)];
 Vname = strvcat(Vname,tmp);
 end;
pinf.cnames = strvcat('Estimates');
pinf.rnames = Vname;     

pt = pin;
xi = eye(n);

iter = 0;
t = 1;

t0 = clock;
while (iter <= maxit)
 iter = iter+1;
 fp = fret;
 ibig = 0;
 del = 0;
 i = 1;

 if pflag == 1 
 % print iteration results
        matprt = [iter fp ];
        mprint(matprt,input);
        matprt = [pin];
        mprint(matprt,pinf);  
   end;
 

 while (i <= n)
  xit = xi(:,i);
   
     [pin fret] = linmin(pin,xit,tol,f,varargin{:});
 
       if (abs(fp-fret) > del);
        del = abs(fp-fret);
        ibig = i;
       end;
       i = i+1;
   end; % end while
   
   if ((2*abs(fp-fret)) <= tol*(abs(fp)+abs(fret)));
    pout = pin;
    fout = fret;
    hess = fdhess(funfcn,pout,varargin{:});
    niter = iter;
     result.b = pin;
     result.f = -fret;
     result.hess = fdhess(funfcn,pout,varargin{:});
     result.iter = niter; 
     result.time = etime(clock,t0);
     result.flag = 0;
    return;
   end;
   
   ptt = 2*pin-pt;
   xit = pin - pt;
   pt = pin;
    xarg = ptt;
    fptt = feval(funfcn,xarg,varargin{:}); 
 
    if (fptt < fp)
     t = 2*(fp-2*fret +fptt)*(fp-fret-del)^2 - del*(fp-fptt)^2;
    end;
    
    if (t < 0)
     [pin fret] = linmin(pin,xit,tol,f,varargin{:});
     xi(:,ibig) = xit;
    end;
end; % end of while

     result.b = pin;
     result.f = -fret;
     result.hess = fdhess(funfcn,pout,varargin{:});
     result.iter = niter; 
     result.time = etime(clock,t0);
     result.flag = 1;
   
  

  
