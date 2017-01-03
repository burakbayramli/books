function [c, ceq]=garcheviewscon(parameters , data , p , q,  m, stdEstimate);
% PURPOSE:
%     GARCH(P,Q) parameter estimation constraints
% 
% USAGE:
%     [c, ceq]=garcheviewscon(parameters , data , p , q,  m, stdEstimate);
% 
% INPUTS:
%     See garcheviews_likelihood
%
% OUTPUTS:
%     c  : The consraint value     
%
% COMMENTS:
%
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001




a=abs(roots([1 -parameters(2:p+1)'])).^2-.9999;
if m>0
    b=abs(roots([1 -parameters(p+2:p+q+1)'])).^2-.9999;
else
    b=[];
end
c=[a;b];
ceq=[];