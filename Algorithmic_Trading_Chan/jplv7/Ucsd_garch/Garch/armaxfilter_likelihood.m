function [LLF, errors, likelihoods] = armaxfilter_likelihood(parameters , regressand , regressors, ar , ma, tau)
% PURPOSE:
%     This is the likelihood function for armaxfilter
% 
% USAGE:
%     [LLF, errors, likelihoods] = armaxfilter_likelihood(parameters , regressand , regressors, ar , ma, tau)
% 
% 
% INPUTS:
%     parameters:   A vector of GARCH process aprams of the form [constant, arch, garch]
%     regressand:   A set of zero mean residuals
%     regressors:   A matrix of exogenous(conditionally) of size (1+nlags+numX) by t with a ci
%     ar:           The AR order
%     ma:           The MA order of the ARMA process
%     tau:          Length(regressand+max(p,q))
% 
% OUTPUTS:
%     LLF:          Minus 1 times the log likelihood
%     errors:       Time series fo model erors
%     likelihoods:  Time series fo likelihoods
% 
% 
% COMMENTS:
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

if size(parameters,1)>size(parameters,2)
    parameters=parameters';
end

if nargin>2 & ~isempty(regressors)
    regressand=regressand-regressors*parameters(1:size(regressors,2))';
    [e,E]=maxcore(regressand,parameters(size(regressors,2)+1:length(parameters)),ma,tau);
else
    [e,E]=maxcore(regressand,parameters,ma,tau);
end


sigma2=(E)/(tau-length(parameters));

LLF  =  0.5*(tau*(log(sigma2)) + (E/sigma2) + tau*log(2*pi));

if nargout>1
    t    = (ma + 1):tau;
    errors=e(t);
    likelihoods  =  (log(sigma2)) + ((errors.^2)./sigma2);
    likelihoods  =  -0.5 * (likelihoods  +  log(2*pi));
end

if isnan(LLF)
    LLF=1e7;
end
