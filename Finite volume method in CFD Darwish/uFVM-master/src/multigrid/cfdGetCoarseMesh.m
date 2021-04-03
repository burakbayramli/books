function theCoarseMesh = cfdGetCoarseMesh(level)
%--------------------------------------------------------------------------
%
%  written by the CFD Group @ AUB, 2018 
%  contact us at: cfd@aub.edu.lb
%==========================================================================
% Case Description:
%     
%--------------------------------------------------------------------------

global Region;

theCoarseMesh = Region.MG.hierarchy.meshes{level};