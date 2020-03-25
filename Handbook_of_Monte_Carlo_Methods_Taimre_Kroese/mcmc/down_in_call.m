function [H,S_t]=down_in_call(z,dt,r,sig,S_0,K,b)
% implements H(x)
y=(r-sig^2/2)*dt+sig*sqrt(dt)*z; 
S_t=exp(cumsum([log(S_0),y]));
H=max(S_t(end)-K,0)*any(S_t<=b);
