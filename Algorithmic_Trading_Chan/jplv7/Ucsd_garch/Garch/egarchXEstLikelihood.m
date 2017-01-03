function [LLF, h, likelihoods]= egarchXestlikelihood(parameters,data,p,o,q,T,X,stdEstimate,errortype);
% PURPOSE:
%     EGARCHLIKELIHOOD(P,Q) likelihood function.  Helper function to EGARCH
% 
% USAGE:
%    [LLF, h, likelihoods]= egarchXestlikelihood(parameters,data,p,o,q,T,X,stdEstimate,errortype);
% 
% INPUTS:
%     parameters:A vector of parameters,1+2p+q, for the terms in the EGARCH model below
%     data: T by 1 set of residuals
%     P: Non-negative, scalar integer representing a model order of the ARCH 
%         process
%     Q: Positive, scalar integer representing a model order of the GARCH 
%         process: Q is the number of lags of the lagged conditional variances included
%         Can be empty([]) for ARCH process
%     T: Lenth of data
%     X: Exogenous variables, shoudl be aligned with data (have same time subscript).
%     stdEstimate: std of data
%     errortype:  A number,
%            1 for - Gaussian Innovations
%            2 for - T-distributed errors
%            3 for - General Error Distribution
% 
% OUTPUTS:
%     LLF = the loglikelihood evaluated at the parameters
%     h = the estimated time varying VARIANCES
%     likelihoods = A T by 1+2p+q matrix of likelihoods for m testing and robuse SE estimation
% 
% COMMENTS:
%   EGARCH(P,Q) the following(wrong) constratins are used(they are right for the (1,1) case or any Arch case
%     (1) nu>2 of Students T and nu>1 for GED
%
%   The time-conditional variance, H(t), of a EGARCH(P,Q) process is modeled 
%   as follows:
%
%     log H(t) = Omega + Alpha(1)*r_{t-1}/(sqrt(h(t-1))) + Alpha(2)*r_{t-2}^2/(sqrt(h(t-2))) +...
%                    + Alpha(P)*r_{t-p}^2/(sqrt(h(t-p)))+ Absolute Alpha(1)* abs(r_{t-1}^2/(sqrt(h(t-1)))) + ...
%                    + Absolute Alpha(P)* abs(r_{t-p}^2/(sqrt(h(t-p)))) +  Beta(1)* log(H(t-1))
%                    + Beta(2)*log(H(t-2))+...+ Beta(Q)*log(H(t-q))
%
% Has a mex file available in egarchcore.c.  You should compile it(or use the binaries available)
%
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

[r,c]=size(parameters);
if c>r
    parameters=parameters';
end

if errortype ==2;
    nu = 2.1+parameters(2*p+q+2)^2;
    parameters = parameters(1:2*p+q+1);
elseif errortype ==3;
    nu = 1.05+parameters(2*p+q+2)^2;
    parameters = parameters(1:2*p+q+1);
end


if isempty(q) | q==0
    m=max(p,o);
else
    m  =  max([p,q,o]);
end

garchparameters=parameters(1:1+o+p+q);
Xparameters=parameters(2+o+p+q:length(parameters));

h=egarchXcore(data, garchparameters, stdEstimate, p, o, q ,m , T,X*Xparameters');
h(isnan(h))=.0001*stdEstimate;
h(isinf(h))=.0001*stdEstimate;
h(h<=0)=.0001*stdEstimate;

Tau = T-m;
LLF = 0;
t = (m + 1):T;
if errortype == 1
    LLF  =  sum(log(h(t))) + sum((data(t).^2)./h(t));
    LLF  =  0.5 * (LLF  +  (T - m)*log(2*pi));
elseif errortype == 2
    LLF = Tau*gammaln(0.5*(nu+1)) - Tau*gammaln(nu/2) - Tau/2*log(pi*(nu-2));
    LLF = LLF - 0.5*sum(log(h(t))) - ((nu+1)/2)*sum(log(1 + (data(t).^2)./(h(t)*(nu-2)) ));
    LLF = -LLF;
else
    Beta = (2^(-2/nu) * gamma(1/nu)/gamma(3/nu))^(0.5);
    LLF = (Tau * log(nu)) - (Tau*log(Beta)) - (Tau*gammaln(1/nu)) - Tau*(1+1/nu)*log(2);
    LLF = LLF - 0.5 * sum(log(h(t))) - 0.5 * sum((abs(data(t)./(sqrt(h(t))*Beta))).^nu);
    LLF = -LLF;
end


if nargout > 2
    likelihoods=zeros(size(T));
    if errortype == 1
        likelihoods = 0.5 * ((log(h(t))) + ((data(t).^2)./h(t)) + log(2*pi));
        likelihoods = -likelihoods;
    elseif errortype == 2
        likelihoods = gammaln(0.5*(nu+1)) - gammaln(nu/2) - 1/2*log(pi*(nu-2))...
            - 0.5*(log(h(t))) - ((nu+1)/2)*(log(1 + (data(t).^2)./(h(t)*(nu-2)) ));
        likelihoods = -likelihoods;
    else
        Beta = (2^(-2/nu) * gamma(1/nu)/gamma(3/nu))^(0.5);
        likelihoods = (log(nu)/(Beta*(2^(1+1/nu))*gamma(1/nu))) - 0.5 * (log(h(t))) ...
            - 0.5 * ((abs(data(t)./(sqrt(h(t))*Beta))).^nu);
        likelihoods = -likelihoods;
    end
end


if isnan(LLF) | isinf(LLF)
    LLF=10e+4;
end

h=h(t);