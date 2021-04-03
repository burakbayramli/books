function faceNodeIndicesForInterior = cfdGetFaceNodeIndicesForInterior
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
faceNodeIndicesForInterior = Region.mesh.faceNodes(1:theNumberOfInteriorFaces);