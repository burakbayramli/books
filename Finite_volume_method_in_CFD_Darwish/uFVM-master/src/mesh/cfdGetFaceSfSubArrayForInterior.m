function faceSf = cfdGetFaceSfSubArrayForInterior
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
faceSf = Region.mesh.faceSf(1:theNumberOfInteriorFaces,:);
