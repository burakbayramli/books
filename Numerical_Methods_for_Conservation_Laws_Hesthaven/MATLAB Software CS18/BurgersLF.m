function [numflux] = BurgersLF(u,v,lambda,maxvel)
% function [numflux] = BurgersLF(u,v,lambda,maxvel);
% Purpose: Evaluate the Lax Friedrich numerical flux for Burgers equation

fu = u.^2; fv = v.^2;
numflux = (fu+fv)/2 - maxvel/2*(v-u);
end