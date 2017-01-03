function [statistic, pval, H] = shapirofrancia(x,tails,probability)
% PURPOSE:
%   This function performs that Shapiro-Francia Test for normality of the data
%   This is an omnibus test, and is generally considered relatively powerful against 
%   a variety of alternatives, and better than the S-Wilks tst for Lepto-kurtotic Sample
% 
% 
% USAGE:
%     [statistic, pval, H] = shapirofrancia(x,tails,probability)
% 
% INPUTS:
%   x: an Nx1 vector of deviates from an unknown distribution
%   tails(optional): 0 for a two tailed test(Default)
%                    1 for a one sided(upper) test
%                    -1 for a one sided(lower) test
%   probability: The significance level for the test(.05 by default)
% 
% 
% OUTPUTS:
%  statistic:A N(0,1) teststatistic transformed form the W
%  pval: The significance of the statistic
%  H: 0 for fail to reject the null at the sig level, 1 otherwise
% 
% 
% COMMENTS:
%  See Royston(1993) for details in the approximation
%  Not for censored data
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001
%   



% First, calculate the a's for weights as a function of the m's
% See Royston(1993) for details in the approximation
x=unique(x);
n=size(x,1)
mtilde=norm_inv(([1:n]'-(3/8))/(n+.25));
weights=(sqrt(mtilde'*mtilde)^-1)*mtilde;

W=(sum(weights.*x))^2/((x-mean(x))'*(x-mean(x)));

nu=log(n);
u1=log(nu)-nu;
mu=-1.2725+1.0521*u1;
u2=log(nu)+2/nu;
sigma=1.0308-.026758*u2;

newstatistic=log(1-W);
z=(newstatistic-mu)/sigma;

pval=min(norm_cdf(z),1-norm_cdf(z))
H=pval<.05;
statistic=z;
