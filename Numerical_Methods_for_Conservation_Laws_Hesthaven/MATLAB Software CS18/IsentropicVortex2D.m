function [r,ru,rv,E] = IsentropicVortex2D(x,x0,u0,y,y0,v0,gamma,beta,t)
% function [r,ru,rv,E] = IsentropicVortex2D(x,x0,u0,y,y0,v0,gamma,beta,t)
% Purpose: compute flow configuration given by
%     Y.C. Zhou, G.W. Wei / Journal of Computational Physics 189 (2003) 159 

xs = x-u0*t-x0; ys = y-v0*t-y0; rr = sqrt(xs.^2 + ys.^2);
u = u0 - beta*exp(1-rr.^2).*ys/(2*pi); v = v0 + beta*exp(1-rr.^2).*xs/(2*pi);
r = (1 - ((gamma-1)*beta^2*exp(2*(1-rr.^2))/(16*gamma*pi^2))).^(1/(gamma-1));
ru = r.*u; rv = r.*v; E = r.^gamma/(gamma-1)+0.5*r.*(u.^2+v.^2);
return