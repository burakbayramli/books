function elementNbIndices = cfdGetElementNbIndices
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

global Region;

elementNbIndices = Region.mesh.elementNeighbours;