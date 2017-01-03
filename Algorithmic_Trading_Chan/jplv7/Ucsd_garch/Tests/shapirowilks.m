function [statistic, pval, H] = shapirowilks(x,tails,probability)
% PURPOSE:
%   This function performs that Shapiro-Wilks Test for normality of the data
%   This is an omnibus test, and is generally considered relatively powerful against 
%   a variety of alternatives, and better than the S-Francia tst for Platy-kurtotic Sample
% 
% 
% USAGE:
%   x: an Nx1 vector of deviates from an unknown distribution
%   tails(optional): 0 for a two tailed test(Default)
%   probability: The significance level for the test(.05 by default)
% 
% 
% INPUTS:
%  statistic:A N(0,1) teststatistic transformed form the W
%  pval: The significance of the statistic
%  H: 0 for fail to reject the null at the sig level, 1 otherwise
% 
% OUTPUTS:
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


                  

% First, calculate the a's for weights as a function of the m's
% See Royston(1993) for details in the approximation
if nargin<2
    tails=0;
    probability=.05;
end
if nargin<3
    probability=.05;
end

x=unique(x);
n=size(x,1);
mtilde=norm_inv(([1:n]'-(3/8))/(n+.25));


c=(sqrt(mtilde'*mtilde)^-1)*mtilde;

weights=ones(size(x))*-999;
u=n^(-0.5);
weights(n)=c(n)+ .221157*u - .147981 * u^2 - 2.071190 * u^3 + 4.434685 * u^4 - 2.706056 * u^5;
weights(n-1)=c(n-1)+ .042981*u - .293762 * u^2 - 1.752461 * u^3 + 5.682633 * u^4 - 3.582633 * u^5;
weights(1)=-weights(n);
weights(2)=-weights(n-1);

phi=(mtilde'*mtilde - 2 * mtilde(n)^2 - 2 * mtilde(n-1)^2)/(1- 2*weights(n)^2 - 2*weights(n-1)^2);

weights(3:n-2)=mtilde(3:n-2)*phi^(-0.5);

W=(sum(weights.*x))^2/((x-mean(x))'*(x-mean(x)));


newu=log(n);

if n>3 & n<12
    gamma=-2.273+.459*newu;
    mu=.5440-.39978*newu+.025054*newu^2-.0006714*newu^3;
    sigma=exp(1.3822-.77857*newu+.062767*newu^2-.0020322*newu^3);
    newstatistic=-log(gamma-log(1-W));
    z=(newstatistic-mu)/sigma;
    pval=min(norm_cdf(z),1-norm_cdf(z));
elseif n>11 & n <5000
    mu=-1.5861 - .31082 *newu - .083751*newu^2 + .0038915 * newu^3;
    sigma = exp(-.4803 - .082676 * newu + .0030302 * newu^2);
    newstatistic=log(1-W);
    z=(newstatistic-mu)/sigma;        
    pval=min(norm_cdf(z),1-norm_cdf(z));
end

H=pval<probability;
statistic=z;