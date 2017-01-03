function y = norm_crnd(n,epsilon,sigma)
% PURPOSE: random numbers from a contaminated normal distribution
% y = (1-epsilon)*N(0,1) + epsilon*N(0,sigma^2)
%---------------------------------------------------
% USAGE:   y = norm_crnd(n,eps,sigma)
% where:   n = size of the vector returned
%        sig = a scalar variance (sigma^2)
%        eps = scalar 0 < eps < 1
%---------------------------------------------------      
% RETURNS: y = random vector of contaminated 
%              normal random draws 
%---------------------------------------------------
% SEE ALSO: norm_d, norm_rnd, norm_inv, norm_cdf
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

u = unif_rnd(n,0,1);
for i = 1:n
    if u(i)<= epsilon
       v(i) = sigma;
    else
       v(i) = 1;
    end
end
g = randn(n,1);
y = g.*v';
