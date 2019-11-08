function hess = diffhess(x, f, gc, heps)
% compute a forward difference Hessian f''(x)
%
% uses dirdero.m to compute the columns, then symmetrize
%
% C. T. Kelley, March 17, 1998
%
% This code comes with no guarantee or warranty of any kind.
%
%
% Inputs:       x, f = point and function
%	        gc = current gradient, preevaluated
%		heps = difference increment (optional)
%                        default = 1.d-6
%		
%		the calling sequence for the function is
%		[func,grad]=f(x)
%
% Output: 	hess = hessian
%
if nargin == 3
heps = 1.d-6;
end
n=length(x);
for j=1:n
    zz=zeros(n,1);
    zz(j)=1;
    hess(:,j)=dirdero(x,zz,f,gc,heps);
end
%
% symmetrize
%
hess=(hess+hess')*.5;
