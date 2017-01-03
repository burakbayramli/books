function [LLF, grad, hessian, h, scores, robustse] = garchlikelihood(parameters , data , p , q,  m, stdEstimate)
% PURPOSE:
%     Likelihood and analytic derivatives for garchpq
% 
% USAGE:
%     [LLF, grad, hessian, h, scores, robustse] = garchlikelihood(parameters , data , p , q, m, stdEstimate)
% 
% 
% INPUTS:
%     parameters:   A vector of GARCH process aprams of the form [constant, arch, garch]
%     data:         A set of zero mean residuals
%     p:            The lag order length for ARCH
%     q:            The lag order length for GARCH
%     m:            The max of p and q
%     stdEstimate:  The sample standard deviation of the data 
% 
% 
% OUTPUTS:
%     LLF:          Minus 1 times the log likelihood
%     grad:         The analytic gradient at the parameters
%     hessian:      The analytical hessian at the parameters
%     h:            The time series of conditional variances implied by the parameters and the data
%     scores:       A matrix, T x #params of the individual scores
%     robustse:     Quasi-ML Robust Standard Errors(Bollweslev Wooldridge)
% 
% 
% COMMENTS:
%     This is a helper function for garchpq
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001
    
parameters(find(parameters(1:p+1)<= 0)) = realmin;

if isempty(q)
    m=p;
else
    m  =  max(p,q);   
end
T           =  size(data,1);                    
h=garchcore(data,parameters,stdEstimate^2,p,q,m,T);

places=find(h<=0);
h(places)=stdEstimate^2*10.^(-4+h(places));
t    = (m + 1):T;
h=h(t);
LLF  =  0.5 * (sum(log(h)) + sum((data(t).^2)./h)  +  (T - m)*log(2*pi));
if isnan(LLF)
    LLF=1e6;
end
