function [LLF, h, likelihoods] = tarchlikelihood(parameters , data , p, o, q,  m, stdEstimate, type)
% PURPOSE:
%     Likelihood tarch and gjrarch
% 
% USAGE:
%     [LLF, grad, hessian, h, scores, robustse] = tarchlikelihood(parameters , data , p , o, q, m, stdEstimate)
% 
% 
% INPUTS:
%     parameters:   A vector of GARCH process aprams of the form [constant, arch, garch]
%     data:         A set of zero mean residuals
%     p:            The lag order length for ARCH
%`    0:            The lag order lenth for TARCH
%     q:            The lag order length for GARCH
%     m:            The max of p and q
%     stdEstimate:  The sample standard deviation of the data 
% 
% 
% OUTPUTS:
%     LLF:          Minus 1 times the log likelihood
%     h:            The time series of conditional variances implied by the parameters and the data
%     likelihoods:  A  by 1 vector of likelihoods
% 
% 
% COMMENTS:
%     This is a helper function for tarch
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001
    
parameters(find(parameters <= 0)) = realmin;

if isempty(q)
    m=max(p,o);
else
    m  =  max([p q o]);   
end
T   =  size(data,1);                    

dataneg=(data<0).*data;
dataneg(1:m)=data(1)/2;

if type==1
    h=tarchcore(abs(data),abs(dataneg),parameters,stdEstimate,p,o,q,m,T);
    h=h.^2;
else
    h=tarchcore(data.^2,dataneg.^2,parameters,stdEstimate^2,p,o,q,m,T);    
end

t    = (m + 1):T;
h=h(t);
likelihoods  =  0.5 * (log(h) + (data(t).^2)./h  + log(2*pi));
LLF=sum(likelihoods);

if isnan(LLF)
   LLF=1e+6;
end