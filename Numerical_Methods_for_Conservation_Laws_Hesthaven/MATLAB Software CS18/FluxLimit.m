function phi = FluxLimit(r,type,beta)
% function phi = FluxLimit(r,type,beta)
% Define flux limiter function based on type. Beta is used in some limiters
r = max(0,r);

% No flux limiter
if (type==0) phi = zeros(length(r),1); end
% Chakravarthy/Osher limiter
if (type==1) phi = min(r,beta); end
% Koren limiter
if (type==2) phi = min(min(2*r,(2+r)/3),2); end
% Sweby limiter
if (type==3) phi = max(min(beta*r,1),min(r,beta)); end
% OSPRE limiter
if (type==4) phi = 1.5*(r.^2+r)./(r.^2+r+1); end
% van Leer limiter
if (type==5) phi = 2*r./(1+r); end

phi = max(0,phi);
return