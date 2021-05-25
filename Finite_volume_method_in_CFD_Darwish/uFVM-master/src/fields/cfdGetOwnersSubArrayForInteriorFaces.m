function owners = cfdGetOwnersSubArrayForInteriorFaces
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
owners = Region.mesh.owners(1:theNumberOfInteriorFaces);
