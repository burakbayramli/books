function closed = cfdIsClosedCavity
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function checks if the mesh does not have an outlet
%--------------------------------------------------------------------------

global Region;

closed = Region.mesh.closed;