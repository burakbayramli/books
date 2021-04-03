function owners = cfdGetOwnersSubArrayForBoundaryPatchFaces
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns a subarray of the owners of interior faces
%--------------------------------------------------------------------------

global Region;

theNumberOfInteriorFaces = Region.mesh.numberOfInteriorFaces;
theNumberOfFaces = Region.mesh.numberOfFaces;
owners = Region.mesh.owners(theNumberOfInteriorFaces+1:theNumberOfFaces);
