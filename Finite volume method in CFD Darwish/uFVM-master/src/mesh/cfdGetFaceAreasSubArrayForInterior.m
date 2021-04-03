function faceAreas = cfdGetFaceAreasSubArrayForInterior
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
faceAreas = Region.mesh.faceAreas(1:theNumberOfInteriorFaces,:);
