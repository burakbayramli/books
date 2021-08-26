function [numflux] = KPPxLF(u,v,lambda,maxvel)
% function [numflux] = KPPxLF(u,v,lambda,maxvel);
% Purpose: Evaluate Lax Friedrich numerical flux 
% for x component of KPP equation

fu = sin(u); fv = sin(v);
numflux = (fu+fv)/2 - maxvel/2*(v-u);
return