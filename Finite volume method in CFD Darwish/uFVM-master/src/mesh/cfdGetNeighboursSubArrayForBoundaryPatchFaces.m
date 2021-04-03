function neighbours = cfdGetNeighboursSubArrayForBoundaryPatchFaces
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
theNumberOfFaces = Region.mesh.numberOfFaces;
neighbours = Region.mesh.neighbours(theNumberOfInteriorFaces+1:theNumberOfFaces);
