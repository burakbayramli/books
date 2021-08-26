function [flux] = HeatFlux(u,v,type);
% function [flux] = HeatFlux(u,v,type);
% Purpose: Compute flux for heat equation.
%   Type: 'L' =Upwind, 'C'=Central, 'R'=Downwind

if type=='L' flux = u; end
if type=='C' flux = (u+v)/2; end
if type=='R' flux = v; end
return