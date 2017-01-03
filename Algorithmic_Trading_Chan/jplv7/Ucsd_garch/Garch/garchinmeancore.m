function [e,h,SSE]=garchinmeancore(regressand,he,stdEstimate,map,garchconst,garchimp,archp,garchp,m,ma,p,q,T);
% PURPOSE:
%     Core routine of garch in mean. Use the MEX file.  
% 
% USAGE:
%     [e,h,SSE]=garchinmeancore(regressand,he,stdEstimate,map,garchconst,garchimp,archp,garchp,m,ma,p,q,T);
% 
% INPUTS:
%     See garchinmeanlikelihood
% 
% OUTPUTS:
%     See garchinmeanlikelihood
% 
% COMMENTS:
%     Helper function part of UCSD_GARCH toolbox. Used if you do not use the MEX file.
%     You should use the MEX file.
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

h=zeros(size(regressand));
e=zeros(size(regressand));
h(1:m+1)=stdEstimate^2;
for t=m+1:T
e(t)= regressand(t) - map'*e(t-1:-1:t-ma) - garchimp*h(t)^(0.5);
he(t)=e(t);
h(t+1)=garchconst+archp'*he(t:-1:t-p+1).^2+garchp'*h(t:-1:t-q+1);
end
t=m+1:T;
SSE=sum(e(t).^2./h(t));