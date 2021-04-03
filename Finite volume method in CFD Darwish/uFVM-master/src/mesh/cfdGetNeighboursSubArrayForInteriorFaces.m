function iNeighbours = cfdGetNeighboursSubArrayForInteriorFaces
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns a subarray of the owners of interior faces
%--------------------------------------------------------------------------

global Region;

iNeighbours = [Region.mesh.faces(1:Region.mesh.numberOfInteriorFaces).iNeighbour]';
