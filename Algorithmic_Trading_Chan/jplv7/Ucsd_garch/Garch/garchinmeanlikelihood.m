function [LLF, errors, likelihoods, h] = garchinmeanlikelihood(parameters , regressand , regressors, ar , ma, p , q, m ,T, stdEstimate)
% PURPOSE:
%     Likelihood evaluation for garchinmean
% 
% USAGE:
%     [LLF, errors, likelihoods, h] = garchinmeanlikelihood(parameters , regressand , regressors, ar , ma, p , q, m ,T, stdEstimate)
% 
% INPUTS: 
%     parameters:   A vector of GARCH process aprams of the form [constant, ar, ma, garchconst, arch, garch]
%     regressand:   A set of zero mean residuals
%     regressors:   Tx K matrix of regressors(incl constand and ar)
%     ar:           AR order
%     ma:           MA order
%     p:            The lag order length for ARCH
%     q:            The lag order length for GARCH
%     m:            The max of p and q
%     stdEstimate:  The sample standard deviation of the data 
% 
% OUTPUTS:
%     LLF:          Minus 1 times the log likelihood
%     errors:       Time series of model erorrs
%     likelihoods   Time series of likelihoods
%     h:            Time series of conditional volatilities
% 
% 
% COMMENTS:
%     This is a helper function for garchinmean
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001




% first parse teh parameters into 67 categories
% [ AR(incl CONST) GIM MA]
k=size(regressors,2);
arp=parameters(1:k);
garchimp=parameters(k+1);
map=parameters(k+2:k+1+ma);


temp=k+1+ma;
garchconst=parameters(temp+1);
archp=parameters(temp+2:temp+p+1);
garchp=parameters(temp+p+2:temp+p+1+q);
if garchconst<0
    garchconst=realmin;
end

%Now we need to back cast
he=zeros(T,1);
he(1:m)=stdEstimate;
regressand=regressand-regressors*arp;

if ma==0
    map=0;
    ma=1;
end

[e,h,E]=garchinmeancore(regressand,he,stdEstimate,map,garchconst,garchimp,archp,garchp,m,ma,p,q,T);

t    = (m + 1):T;
LLF  =  0.5 * (sum(log(h(t))) + E  +  (T - m)*log(2*pi));


if nargout>1
    t    = (m + 1):T;
    errors=e(t);
    h=h(t);
    likelihoods  =  (log(h)) + ((errors.^2)./h);
    likelihoods  =  -0.5 * (likelihoods  +  log(2*pi));
end
if isnan(LLF)
    LLF=1e6;
end



