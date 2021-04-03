function cfdSetMeshField(theMeshField)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function stores the mesh field in the data base
%--------------------------------------------------------------------------

global Region;

Region.fluid.(theMeshField.name) = theMeshField;