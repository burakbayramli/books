function [numflux] = KPPyLF(u,v,lambda,maxvel)
% function [numflux] = KPPyLF(u,v,lambda,maxvel);
% Purpose: Evaluate Lax Friedrich numerical flux 
% for y component of KPP equation

fu = cos(u); fv = cos(v);
numflux = (fu+fv)/2 - maxvel/2*(v-u);
return