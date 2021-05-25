function theChildrenArray = cfdCoarseToFineAddressing(level)
%--------------------------------------------------------------------------
%
%  written by the CFD Group @ AUB, 2018 
%  contact us at: cfd@aub.edu.lb
%==========================================================================
% Case Description:
%     
%--------------------------------------------------------------------------

global Region;

theChildrenArray = Region.MG.hierarchy.cfdCoarseToFineAddressing{level};