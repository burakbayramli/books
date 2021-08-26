function [numflux] = LinwaveLW(u,v,lambda,maxvel)
% function [numflux] = LinwaveLW(u,v,lambda,maxvel);
% Purpose: Evaluate Lax Wendroff numerical flux for wave equation

numflux = (u+v)/2 - lambda/2*(v-u);
end