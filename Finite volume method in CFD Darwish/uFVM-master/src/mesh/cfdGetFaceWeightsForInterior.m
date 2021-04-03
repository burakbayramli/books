function faceWeights = cfdGetFaceWeightsForInterior
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

global Region;

theNumberOfInteriorFaces = Region.mesh.numberOfInteriorFaces;
faceWeights = Region.mesh.faceWeights(1:theNumberOfInteriorFaces);
