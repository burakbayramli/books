function [numflux] = MaxwellLW(u,v,ep,mu,lambda,maxvel)
% function [numflux] = MaxwellLW(u,v,ep,mu,lambda,maxvel);
% Purpose: Evaluate Lax Wendroff numerical flux for Maxwell's equations

fu = [u(:,2)./ep u(:,1)./mu]; fv = [v(:,2)./ep v(:,1)./mu]; 
A2 = [ 1./ep.*mu 1./ep.*mu ];
numflux = (fu+fv)/2 - lambda/2*A2.*(v-u);
return