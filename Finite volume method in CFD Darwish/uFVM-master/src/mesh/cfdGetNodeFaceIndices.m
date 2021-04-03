function nodeFaceIndices = cfdGetNodeFaceIndices
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

global Region;

nodeFaceIndices = Region.mesh.nodeFaces;