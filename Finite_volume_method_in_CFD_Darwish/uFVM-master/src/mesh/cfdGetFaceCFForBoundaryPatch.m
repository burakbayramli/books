function faceCF = cfdGetFaceCFForBoundaryPatch(iBPatch)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

global Region;

startBFace = Region.mesh.cfdBoundaryPatchesArray{iBPatch}.startFaceIndex;
endBFace = startBFace+Region.mesh.cfdBoundaryPatchesArray{iBPatch}.numberOfBFaces-1;
iBFaces = startBFace:endBFace;

faceCF = Region.mesh.faceCF(iBFaces,:);