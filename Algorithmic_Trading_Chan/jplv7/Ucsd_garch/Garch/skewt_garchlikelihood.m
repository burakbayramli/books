function [LLF, h, likelihoods] = skewt_garchlikelihood(parameters , data , p , q, m, stdEstimate)
% PURPOSE:
%     Likelihood for skewt_garch
% 
% USAGE:
%     [LLF, grad, hessian, h, scores, robustse] = garchlikelihood(parameters , data , p , q, m, stdEstimate)
% 
% INPUTS:
%     parameters:   A vector of GARCH process aprams of the form [constant, arch, garch]
%     data:         A set of zero mean residuals
%     p:            The lag order length for ARCH
%     q:            The lag order length for GARCH
%     m:            The max of p and q
%     stdEstimate:  The sample standard deviation of the data 
% 
% OUTPUTS:
%     LLF:          Minus 1 times the log likelihood
%     h:            The time series of conditional variances implied by the parameters and the data
%     likelihoods:  The time series of log likelioods
% 
% COMMENTS:
%     This is a helper function for skewt_garch
%
% Author: Andrew Patton
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001


[r,c]=size(parameters);
if c>r
   parameters=parameters';
end

parameters(find(parameters(1:1+p+q) <= 0)) = realmin;


constp=parameters(1);
archp=parameters(2:p+1);
garchp=parameters(p+2:p+q+1);
nu = parameters(p+q+2);
lambda = parameters(p+q+3);

T           =  size(data,1);                    
h=garchcore(data,parameters(1:1+p+q),stdEstimate,p,q,m,T);

t=(m+1:T)';

h=h(t);
stdresid = data(t)./sqrt(h);
LLF = +0.5*sum(log(h)) + skewtdis_LL([nu;lambda], stdresid);
if isnan(LLF)
    LLF=1e6;
end
if nargout>2
    [temp,likelihoods]=skewtdis_LL([nu;lambda], stdresid);
    likelihoods= likelihoods+0.5*log(h);
end

