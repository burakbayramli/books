function resid = detrend(y,p) 
% PURPOSE: detrend a matrix y of time-series using regression
%          of y against a polynomial time trend of order p
% -----------------------------------------------------------          
% USAGE: resid = detrend(y,p)
% where:     y = input matrix (or vector) of time-series (nobs x nvar)
%            p = 0, subtracts mean
%            p = 1, constant plus trend model
%            p > 1, higher order polynomial model
%            p = -1, returns y
% -----------------------------------------------------------           
% RETURNS: resid = residuals from the detrending regression
% -----------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

% error checking on input arguments
if nargin ~= 2
 error('Wrong # of arguments to detrend');
end;

if (p == -1)
 resid = y;
 return
end;

[nobs junk] = size(y);
    u = ones(nobs,1);

   if p > 0
 timep = zeros(nobs,p);
    t = 1:nobs;
    tp = (t')/nobs;
    m = 1;
    while (m <= p)
     timep(:,m) = tp.^m;
     m = m+1;
    end;
    xmat = [u timep];
   else
    xmat = u;
   end;
   
   xpxi = inv(xmat'*xmat);
   beta = xpxi*(xmat'*y);
   resid = y - xmat*beta;

