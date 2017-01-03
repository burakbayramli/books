function [LLF, errors, likelihoods] = armaxfilter_likelihood(parameters , regressand , regressors, ma)
% PURPOSE:
%     Likelihood function for armaxfilter
% 
% USAGE:
% [LLF, errors, likelihoods] = armaxfilter_likelihood(parameters , regressand , regressors, ma)
% 
% INPUTS:
%     parameters:   A vector of GARCH process aprams of the form [constant, arch, garch]
%     regressand:   A set of zero mean residuals
%     regressors:   A matrix of exogenous(conditionally) of size (1+nlags+numX) by t with a ci
%     ma:           The MA order of the ARMA process
% 
% OUTPUTS:
%     LLF:          Minus 1 times the log likelihood
%     errors:       Time series of errors
%     likelihoods:  Time series of log likelihoods
% 
% 
% COMMENTS:
%     This is a helper function for mafilter
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

tau=length(regressand);

if nargin>2 & ~isempty(regressors)
    regressand=regressand-parameters(1:size(regressors,2))*regressors;
    [e,E]=maxcore(regressand,parameters(size(regressors,2)+1:length(parameters)),ma,tau);
else
    [e,E]=maxcore(regressand,parameters,ma,tau);
end

t    = (ma + 1):tau;
sigma2=(E)/(tau-length(parameters));

LLF  =  0.5*(tau*(log(sigma2)) + (E/sigma2) + tau*log(2*pi));

if nargout>1
    errors=e(t);
    likelihoods  =  (log(sigma2)) + ((errors.^2)./sigma2);
    likelihoods  =  -0.5 * (likelihoods  +  log(2*pi));
end
if isnan(LLF)
    LLF=1e7;
end
