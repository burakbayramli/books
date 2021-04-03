function numberOfBPatches = cfdGetNumberOfBPatches
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    This function returns number of cfdBoundary patches
%--------------------------------------------------------------------------

global Region;

numberOfBPatches = Region.mesh.numberOfBoundaryPatches;