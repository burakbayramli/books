function x = gamm_rnd(n,a)
% PURPOSE: a vector of random draws from the gamma distribution
%---------------------------------------------------
% USAGE: r = gamm_rnd(n,A)
% where: n = the row size of the n x 1 vector drawn 
%        a = a parameter such that the mean of the gamma = a
%            and the variance of the gamma = a
%        notes: x = gamm_rnd(n,a*0.5)*2,equals chisq a random deviate
%        For different parameters, A,B use:
%	B*gamm_rnd(n,A) to produce a vector of random deviates from the gamma
%	distribution with shape parameter A and scale parameter B.  
%   The distribution then has mean A*B and variance A*B^2.
%---------------------------------------------------
% RETURNS:
%        r = an n x 1 vector of random numbers from
%        the gamma(A) distribution      
% --------------------------------------------------
% SEE ALSO: gamm_inv, gamm_pdf, gamm_cdf
%---------------------------------------------------

% modified slightly by J. LeSage
% to avoid an error in Matlab version 7.01

%RGAMMA   Random numbers from the gamma distribution
%
%         x = rgamma(n,a)

% GNU Public Licence Copyright (c) Anders Holtsberg 10-05-2000.

% This consumes about a third of the execution time compared to 
% the Mathworks function GAMRND in a third the number of
% codelines. Yihaaa! (But it does not work with different parameters)
%
% The algorithm is a rejection method. The logarithm of the gamma 
% variable is simulated by dominating it with a double exponential.
% The proof is easy since the log density is convex!
% 
% Reference: There is no reference! Send me an email if you can't
% figure it out.


if any(any(a<=0))
   error('Parameter a is wrong')
end

y0 = log(a)-1/sqrt(a);
c = a - exp(y0);
m = ceil(n*(1.7 + 0.6*(min(min(a))<2)));

y = log(rand(m,1)).*sign(rand(m,1)-0.5)/c + log(a);
f = a*y-exp(y) - (a*y0 - exp(y0));
g = c*(abs((y0-log(a))) - abs(y-log(a)));
reject = (log(rand(m,1)) + g) > f;
y(reject) = [];
x = zeros(n,1);

if length(y) >= n
   x = exp(y(1:n));
else
   tmp = rgamma(n - length(y), a);
   x = [exp(y) 
        tmp];
end
