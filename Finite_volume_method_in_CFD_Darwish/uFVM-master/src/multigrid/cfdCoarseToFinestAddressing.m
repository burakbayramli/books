function theBaseChildrenArray = cfdCoarseToFinestAddressing(level)
%--------------------------------------------------------------------------
%
%  written by the CFD Group @ AUB, 2018 
%  contact us at: cfd@aub.edu.lb
%==========================================================================
% Case Description:
%     
%--------------------------------------------------------------------------

global Region;

theBaseChildrenArray = Region.MG.hierarchy.cfdCoarseToFinestAddressing{level};