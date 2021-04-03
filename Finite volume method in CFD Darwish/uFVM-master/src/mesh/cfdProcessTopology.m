function cfdProcessTopology
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function processes the topology of the mesh
%--------------------------------------------------------------------------

cfdProcessElementTopology;
cfdProcessNodeTopology;
%
cfdProcessGeometry;