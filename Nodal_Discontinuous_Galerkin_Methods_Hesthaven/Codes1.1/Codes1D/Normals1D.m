function [nx] = Normals1D

% function [nx] = Normals1D
% Purpose : Compute outward pointing normals at elements faces

Globals1D;
nx = zeros(Nfp*Nfaces, K); 

% Define outward normals
nx(1, :) = -1.0; nx(2, :) = 1.0;
return
