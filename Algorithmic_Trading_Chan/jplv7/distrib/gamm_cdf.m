function cdf = gamm_cdf (x, a)
% PURPOSE: returns the cdf at x of the gamma(a) distribution
%---------------------------------------------------
% USAGE: cdf = gamm_cdf(x,a)
% where: x = a vector 
%        a = a scalar gamma(a)
%---------------------------------------------------
% RETURNS:
%        a vector of cdf at each element of x of the gamma(a) distribution      
% --------------------------------------------------
% SEE ALSO: gamm_d, gamm_pdf, gamm_rnd, gamm_inv
%---------------------------------------------------

%       Anders Holtsberg, 18-11-93
%       Copyright (c) Anders Holtsberg

if nargin ~= 2
error('Wrong # of arguments to gamm_cdf');
end;

if any(any(a<=0))
   error('gamm_cdf: parameter a is wrong')
end

cdf = gammainc(x,a);
I0 = find(x<0);
cdf(I0) = zeros(size(I0));
