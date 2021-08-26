function [numflux] = LinwaveLF(u,v,lambda,maxvel)
% function [numflux] = LinwaveLF(u,v,lambda,maxvel);
% Purpose: Evaluate Lax Friedrich numerical flux for wave equation

numflux = (u+v)/2 - maxvel/2*(v-u);
end