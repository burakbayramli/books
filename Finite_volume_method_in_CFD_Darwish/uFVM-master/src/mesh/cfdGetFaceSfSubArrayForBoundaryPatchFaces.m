function faceSf = cfdGetFaceSfSubArrayForBoundaryPatchFaces
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns a subarray of the face Sf
%--------------------------------------------------------------------------

global Region;

theNumberOfInteriorFaces = Region.mesh.numberOfInteriorFaces;
theNumberOfFaces = Region.mesh.numberOfFaces;
faceSf = Region.mesh.faceSf(theNumberOfInteriorFaces+1:theNumberOfFaces,:);
