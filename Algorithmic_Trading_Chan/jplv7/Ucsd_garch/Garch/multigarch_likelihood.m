function [LLF,Ht, likelihoods] = multigarch_likelihood(parameters,data,p,o,q,garchtype, errortype, stdEstimate)
% PURPOSE:
%   Likelihood for multigarch
% 
% USAGE:
%   [LLF,Ht, likelihoods] = multigarch_likelihood(parameters,data,p,q,garchtype, errortype, stdEstimate)
% 
% INPUTS:
%   parameters - Parameters for estimation, const, arch, garch, special
%   data       - T+max(p,q) by 1 vector of data
%   p          - Order of ARCH
%   q          - Ordrr of GARCH
%   garchtype  - Number representing garch type
%   errortype  - Number representing the eror type
%   stdEstimate- Std Dev of the data
% 
% OUTPUTS:
%   LLF         - Log Likelihood
%   Ht          - Estimated Conditional Variance
%   likeihoods  - T by 1 vector of likelihoods
% 
% COMMENTS:
%   Helper Function for multigarch
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001
parameters(find(parameters(1:1+p+o+q) <= 0)) = realmin;
garchparameters=parameters(1:p+o+q+1);
remainingparams=parameters(p+q+o+2:length(parameters));
constp=garchparameters(1);
archp=garchparameters(2:p+1);
tarchp=garchparameters(p+2:p+o+1);
garchp=garchparameters(p+o+2:p+q+o+1);
if garchtype == 1
   lambda=2;
   nu=2;
   b=0;
elseif garchtype == 2
   lambda=1;
   nu=1;
   b=0;
elseif garchtype == 3
   lambda=1;
   nu=1;
   b=remainingparams(1);
elseif garchtype == 4
   lambda=remainingparams(1);
   nu=lambda;
   b=0;
elseif garchtype == 5
   lambda=2;
   nu=2;
   b=remainingparams(1);
elseif garchtype == 6
   lambda=remainingparams(1);
   nu=lambda;
   b=0;
elseif garchtype == 7
   lambda=remainingparams(1);
   nu=lambda;
   b=remainingparams(2);
elseif garchtype == 8
   lambda=2;
   nu=2;
   b=0;
end

if errortype ~=1;
   nu2 = remainingparams(length(remainingparams));
end

if isempty(q)
   m=p;
else
   m  =  max(p,q);   
end
T           =  size(data,1);
t = (m + 1):T;
datamb=data-b;
dataneg=(data<0).*datamb;
h=multigarchcore(abs(datamb),abs(dataneg),garchparameters,nu,lambda,b,p,o,q,m,T,stdEstimate);
h=h.^2;

Tau = T-m;
LLF = 0;

if errortype == 1
   LLF  =  sum(log(h(t)) + (data(t).^2)./h(t));
   LLF  =  0.5 * (LLF  +  (T - m)*log(2*pi));
elseif errortype == 2
   LLF = Tau*gammaln(0.5*(nu2+1)) - Tau*gammaln(nu2/2) - Tau/2*log(pi*(nu2-2));
   LLF = LLF - 0.5*sum(log(h(t))) - ((nu2+1)/2)*sum(log(1 + (data(t).^2)./(h(t)*(nu2-2)) ));
   LLF = -LLF;
else
   Beta = (2^(-2/nu2) * gamma(1/nu2)/gamma(3/nu2))^(0.5);
   LLF = (Tau * log(nu2)) - (Tau*log(Beta)) - (Tau*gammaln(1/nu2)) - Tau*(1+1/nu2)*log(2);
   LLF = LLF - 0.5 * sum(log(h(t))) - 0.5 * sum((abs(data(t)./(sqrt(h(t))*Beta))).^nu2);
   LLF = -LLF;
end



if nargout > 1
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
        likelihoods = log(nu)-log(Beta)-log(2)*(1+1/nu)-gammaln(1/nu) - 0.5 * log(h(t)) ...
            - 0.5 * ((abs(data(t)./(sqrt(h(t))*Beta))).^nu);
        likelihoods = -likelihoods;
    end
       Ht=h(t);
end


if isnan(LLF)
   LLF=1e+6;
end