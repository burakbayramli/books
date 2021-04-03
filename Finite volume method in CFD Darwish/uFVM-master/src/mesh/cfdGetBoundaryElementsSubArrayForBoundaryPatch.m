function iBElements = cfdGetBoundaryElementsSubArrayForBoundaryPatch(iBPatch)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

global Region;

startBElement = Region.mesh.numberOfElements+Region.mesh.cfdBoundaryPatchesArray{iBPatch}.startFaceIndex ...
    - Region.mesh.numberOfInteriorFaces;
endBElement = startBElement+Region.mesh.cfdBoundaryPatchesArray{iBPatch}.numberOfBFaces-1;
iBElements = (startBElement:endBElement)';
