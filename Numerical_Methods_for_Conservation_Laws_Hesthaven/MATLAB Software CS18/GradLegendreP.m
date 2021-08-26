function [dP] = GradLegendreP(r, m);
% function [dP] = GradLegendreP(r, m);
% Purpose: Evaluate the derivative of the m'th order Legendre polynomial
% at points r 
dP = zeros(length(r), 1);
if (m>0)
  Ph = -m*r.*LegendreP(r,m) + m*sqrt((2*m+1)/(2*m-1)).*LegendreP(r,m-1);
  dPe = r.^(m+1)*m*(m+1)/2*sqrt((2*m+1)/2);
  endp = (abs(abs(r)-1)>10*eps); rh = r.*endp;
  dP = ~endp.*dPe + endp.*Ph./(1-rh.^2);
end;
return