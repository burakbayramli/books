function like =  to_like2(b,y,x);
% PURPOSE: evaluate tobit log-likelihood
%          to demonstrate optimization routines
%-----------------------------------------------------
% USAGE:    like = to_like1(b,y,x) 
% where:     b = parameter vector (k x 1)
%            y = dependent variable vector (n x 1)
%            x = explanatory variables matrix (n x m)
%-----------------------------------------------------
% NOTE: this function returns a scalar equal to the
%       the log likelihood function (NOT the usual negative)
%       or a scalar sum of the vector depending
%       on the value of the flag argument
%       k ~= m because we may have additional parameters
%           in addition to the m bhat's (e.g. sigma)
%-----------------------------------------------------
% error check
if nargin ~= 3,error('wrong # of arguments to to_like1'); end;
[m1 m2] = size(b);
if m1 == 1
 b = b';
end;

   h = .000001;             % avoid sigma = 0
   [m junk] = size(b);
   beta = b(1:m-1);         % pull out bhat
   sigma = max([b(m) h]);   % pull out sigma
   xb = x*beta;
   llf1 = -(y-xb).^2./(2*sigma) - .5*log(2*pi*sigma);  
   xbs = xb./sqrt(sigma); cdf = .5*(1+erf(xbs./sqrt(2)));
   llf2 = log(h+(1-cdf));
   llf = (y > 0).*llf1 + (y <= 0).*llf2;
   like = sum(llf); % scalar result
   