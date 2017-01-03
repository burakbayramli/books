function [d,e,f]=garchgrad(garchp,p,q,data,h,m,T,stdEstimate)
% PURPOSE:
%     Analytic gradiedn m file for garchpq(use MEX file)
% 
% USAGE:
%     [d,e,f]=garchgrad(garchp,p,q,data,h,m,T,stdEstimate)
% 
% INPUTS:
%     See garchlikelihood
% 
% OUTPUTS:
%     See garchlikelihood
% 
% COMMENTS:
%     Helper function part of UCSD_GARCH toolbox. Used if you do not use the MEX file.
%     You should use the MEX file.
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

t=m+1:T;
d=ones(size(h));
e=ones(size(h,1),p)*stdEstimate^2;
f=ones(size(h,1),q)*stdEstimate^2;
for i=t
    d(i)=1+d(i-1:-1:i-q)'*garchp;
    for j=1:p
        e(i,j)=data(i-j)^2+garchp'*e(i-1:-1:i-q,j);
    end
    for j=1:q
        f(i,j)=h(i-j)+garchp'*f(i-1:-1:i-q,j);  
    end
end