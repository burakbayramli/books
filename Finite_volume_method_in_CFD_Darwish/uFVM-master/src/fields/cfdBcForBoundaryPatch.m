function bcType = cfdBcForBoundaryPatch(theFieldName, iBPatch)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the cfdBoundary condition for the field at a given
%   cfdBoundary
%--------------------------------------------------------------------------

global Region;

bcType = Region.fluid.(theFieldName).boundaryPatchRef{iBPatch}.type;