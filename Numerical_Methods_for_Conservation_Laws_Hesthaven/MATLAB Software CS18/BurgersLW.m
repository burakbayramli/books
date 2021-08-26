function [numflux] = BurgersLW(u,v,lambda,maxvel)
% function [numflux] = BurgersLW(u,v,lambda,maxvel);
% Purpose: Evaluate the Lax Wendroff numerical flux for Burgers equation

fu = u.^2; fv = v.^2; alpha = lambda*2*(u+v)/2;
numflux = (fu+fv)/2 - alpha/2.*(fv-fu);
end