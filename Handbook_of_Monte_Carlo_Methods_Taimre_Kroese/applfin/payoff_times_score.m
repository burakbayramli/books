function [HS,S_t]=payoff_times_score(z,dt,r,sig,S_0,K,b)
% implements H(x)*S(x)
y=(r-sig^2/2)*dt+sig*sqrt(dt)*z; 
S_t=exp(cumsum([log(S_0),y]));
%sensitivity_factor=z(1)/(S_0*sig*sqrt(dt)); % for Vega
sensitivity_factor=sum((z.^2-1)/sig-z*sqrt(dt)); % for Delta
HS=max(S_t(end)-K,0)*sensitivity_factor*any(S_t<=b);
