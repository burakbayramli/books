function iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

global Region;

iFaceStart = Region.mesh.cfdBoundaryPatchesArray{iBPatch}.startFaceIndex;
iFaceEnd = iFaceStart+Region.mesh.cfdBoundaryPatchesArray{iBPatch}.numberOfBFaces-1;
iBFaces = (iFaceStart:iFaceEnd)';