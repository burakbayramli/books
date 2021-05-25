function theBCInfo = cfdGetBoundaryPatchRef(iBPatch)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns cfdBoundary patch reference 
%--------------------------------------------------------------------------

global Region;

theBCInfo = Region.mesh.cfdBoundaryPatchesArray{iBPatch};
