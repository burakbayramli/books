function [pout,fout] = linmin(pin,xi,tol,f,varargin)
% PURPOSE: line minimization routine that performs an ad hoc n-dimensional
%          Golden Section Search for the minimum of a function. 
%          (Converted from Numerical Recipes book linmin routine) 
%---------------------------------------------------
% USAGE: [pout,fout] = linmin(pin,x,tol,func,varargin)
% where:      pin = (kx1) vector of starting values
%             x   = (kx1) direction vector
%             tol = tolerance
%            func = function name string
%        varargin = arguments passed to func
%---------------------------------------------------
% RETURNS: pout = (kx1) minimizing vector
%          fout = (kx1) value of func at minimum pout values
%---------------------------------------------------
% NOTE: func must take the form func(b,varargin)
%       where: b = parameter vector (k x 1)
%       varargin = arguments passed to the function               
%---------------------------------------------------    
% SEE ALSO: dfp_min, pow_min, frpr_min functions that use this routine
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu


funfcn = fcnchk(f,length(varargin));


p1 = pin;
d1 = 0.618;
d2 = 0.382;
p2 = pin + xi;

xarg = p1;
f1 = feval(funfcn,xarg,varargin{:});

itry = 0;

    xarg = p2;
    f2 = feval(funfcn,xarg,varargin{:});
    
while (f1 == f2 ) 
 itry = itry+1;
    p2 = p2 + xi;
    xarg = p2;
    f2 = feval(funfcn,xarg,varargin{:});
    if itry == 10
     error('problems in linmin --- itry is 10');
    end;
end;  

if f1 <= f2
 pm = p1;
 fm = f1;
 f1 = f2;
 p1 = p2;
else
 pm = p2;
 fm = f2;
end;

xd = pm - p1;
p2 = pm + xd;
xarg = p2;
f2 = feval(funfcn,xarg,varargin{:});

while (f2 <= fm)
 xd = 2*xd;
 p2 = pm + xd;
    xarg = p2;
    f2 = feval(funfcn,xarg,varargin{:});
end;

x1 = d1*p1 + d2*p2;
x2 = d2*p1 + d1*p2;
xarg = x1;
fx1 = feval(funfcn,xarg,varargin{:});
xarg = x2;
fx2 = feval(funfcn,xarg,varargin{:});

while (sqrt(sum(p1-p2).^2) >= tol)
 x1 = d1*p1 + d2*p2;
 x2 = d2*p1 + d1*p2;
 xarg = x1;
    fx1 = feval(funfcn,xarg,varargin{:});
    xarg = x2;
    fx2 = feval(funfcn,xarg,varargin{:});
    if (fx1 <= fx2)
     p2 = x2;
    else
     p1 = x1;
    end;   
end;
   
if (fx1 < fx2)
 pout = x1;
else
 pout = x2;
end;
xarg = pout;
    fout = feval(funfcn,xarg,varargin{:});
    