function z = dirdero(x,w,f,gc,epsnew)
% Finite difference directional derivative for optimization
% Approximate f''(x) w
% 
% C. T. Kelley, Dec 20, 1996
%
% This code comes with no guarantee or warranty of any kind.
%
% function z = dirdero(x,w,f,gc,epsnew)
%
% Inputs:
%           x, w = point and direction
%           f = function, the calling sequence is
%				[fun,grad]=f(x)
%           gc = current gradient
%                gc has usually been computed
%                before the call to dirdero
%           epsnew = difference increment (optional)
%                    default = 1.d-6
% 
% Output:   directional derivative  
%
% 
if nargin == 4
epsnew=1.d-6;
end
%
n=length(x);
%
% scale the step
%
if norm(w) == 0
    z=zeros(n,1);
return
end
epsnew = epsnew/norm(w);
%
% del and g1 could share the same space if storage
% is more important than clarity
%
del=x+epsnew*w;
[f1,g1]=feval(f,del);
z = (g1 - gc)/epsnew;
