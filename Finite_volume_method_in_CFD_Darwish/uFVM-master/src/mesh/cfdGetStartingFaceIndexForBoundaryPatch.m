function startFaceIndex = cfdGetStartingFaceIndexForBoundaryPatch(iBPatch)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

global Region;

startFaceIndex = Region.mesh.cfdBoundaryPatchesArray{iBPatch}.startFaceIndex;
