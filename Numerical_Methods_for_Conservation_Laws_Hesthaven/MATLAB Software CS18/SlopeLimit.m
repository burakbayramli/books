function psi = SlopeLimit(a,b,type,c,M,h)
% function psi = SlopeLimit(a,b,type,c,M,h)
% Define slope limiter function based on type. c is used in one limiter
% M,h is used for TVB limiting
N = length(a); zero = zeros(N,1);

% No slope 
if (type==0) psi = zeros(N,1); end
% minmod limiter
if (type==1) psi = minmod([a b]); end
% MUSCL limiter
if (type==2) psi = minmod([(a+b)/2 2*a 2*b]); end
% Superbee limiter
if (type==3) psi = minmod([maxmod([a b]) minmod([2*a 2*b]) ]); end
% van Albada limiter
if (type==4) psi = minmod([(((a.^2 + c.^2).*b + ...
               (b.^2 + c.^2).*a)./(a.^2+b.^2+c.^2)) 2*a 2*b]); end
% van Leer limiter
if (type==5) psi = minmod([2.*a.*b./(a+b) 2*a 2*b]); end
% TVB limiting
if (type==6) psi = minmodTVB([(a+b)/2 2*a 2*b],M,h); end
return