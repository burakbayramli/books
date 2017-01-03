function C = bsf(S,t,E,r,sigma,T)
% function C = bsf(S,t,E,r,sigma,T)
%
%  Black-Scholes formula for a European call
%

tau = T-t;
if tau > 0
   d1 = (log(S/E) + (r + 0.5*sigma^2)*tau)/(sigma*sqrt(tau));
   d2 = d1 - sigma*sqrt(tau);
   N1 = 0.5*(1+erf(d1/sqrt(2)));
   N2 = 0.5*(1+erf(d2/sqrt(2)));
   C = S*N1-E*exp(-r*tau)*N2;
else 
   C = max(S-E,0);
end
