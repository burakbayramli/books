function theMesh = cfdGetMesh
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the mesh from the data base
%--------------------------------------------------------------------------

global Region;

theMesh = Region.mesh;