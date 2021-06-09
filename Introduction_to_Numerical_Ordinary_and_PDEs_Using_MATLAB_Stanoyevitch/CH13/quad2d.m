function nint= quad2d(fun,xmin,xmax,ylow,ytop)
%numerically computes a double integral of a function 'fun' on a region 
%over the interval minx<x<maxx and between the functions of x: ylow<y<ytop.
% INPUTS:  fun = a function of the symbolic variables x and y
% minx = minimum x-value for region
% maxx = maximum x-value for region
% ylow = function of symbolic variable for lower boundary of region
% ytop = function of symbolic variable for upper boundary of region
% OUTPUT:  nint = numerical approximation of integral using the integrator
% 'dblquad' in conjunction with the default settings.  
% x and y should be declared symbolic variables before this M-file is used.
syms u x y
ynew=ylow+u*(ytop-ylow);
funprep=subs(fun,y,ynew)*(ytop-ylow);
funnew=vectorize(inline([char(funprep),'*ones(size(u))'],'u','x'));
nint = dblquad(funnew,0,1,xmin,xmax);
