function [c,a1,a2] = BS( S, K, r, tau, sigma)
% BS calculates the value of the call option as a function of
% stock price S.
%
%        K = strike price             sigma = volatility
%        r = interest rate          tau = time until maturity
%
% note: time scales should be consistent with the interest rate.

a1 = (repmat(log(S./K),[length(tau),1]) + ...
   repmat((r+sigma^2/2).*tau,[1,length(S)]))...
   ./repmat(sigma.*sqrt(tau),[1,length(S)]);
a2 = (repmat(log(S./K),[length(tau),1]) + ...
   repmat((r-sigma^2/2).*tau,[1,length(S)]))...
   ./repmat(sigma.*sqrt(tau),[1,length(S)]);

c = repmat(S,[length(tau),1]).*normcdf(a1)...
   - repmat(K*exp(-r.*(tau)),[1,length(S)]).*normcdf(a2);

