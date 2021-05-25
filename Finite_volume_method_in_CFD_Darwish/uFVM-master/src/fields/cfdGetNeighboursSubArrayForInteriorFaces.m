function neighbours = cfdGetNeighboursSubArrayForInteriorFaces
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns a subarray of the neighbours of interior faces
%--------------------------------------------------------------------------

global Region;

theNumberOfInteriorFaces = Region.mesh.numberOfInteriorFaces;
neighbours = Region.mesh.neighbours(1:theNumberOfInteriorFaces);
