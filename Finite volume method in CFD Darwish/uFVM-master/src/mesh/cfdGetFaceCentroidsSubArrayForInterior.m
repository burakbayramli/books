function faceCentroids = cfdGetFaceCentroidsSubArrayForInterior
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

global Region;

theNumberOfInteriorFaces = Region.mesh.numberOfInteriorFaces;
faceCentroids = Region.mesh.faceCentroids(1:theNumberOfInteriorFaces,:);
