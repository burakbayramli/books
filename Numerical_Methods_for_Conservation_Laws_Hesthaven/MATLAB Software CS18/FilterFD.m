function [fc] = filter(M,p)
% function [f] = filter(M,p)
% Purpose: Compute filter coefficients 0:M for filter of order p 
% and width 2M+1.
fc = zeros(M+1,1); 

% Exponential filter (p even)
alpha = 10;
f = @(x,n) exp(-alpha*(x/pi).^(p)).*cos(n*x);

% Optimal filter of order p
% f = @(x,n) (1-betainc(x/pi,p,p)).*cos(n*x);

for m=0:M
    fc(m+1) = integral(@(x) f(x,m),0,pi)/pi;
end
return