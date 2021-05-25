function theNumberOfBFaces = cfdGetNumberOfBFaces
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

global Region;

theNumberOfBFaces = Region.mesh.numberOfBFaces;
