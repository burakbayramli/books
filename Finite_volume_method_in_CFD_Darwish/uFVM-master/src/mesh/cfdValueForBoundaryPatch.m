function bcValue = cfdValueForBoundaryPatch(theFieldName, iBPatch)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the cfdBoundary condition value for the field at a 
%   given cfdBoundary
%--------------------------------------------------------------------------

global Region;

bcValue = Region.fluid.(theFieldName).boundaryPatchRef{iBPatch}.value;