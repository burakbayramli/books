function theParentsArray = cfdFineToCoarseAddressing(level)
%--------------------------------------------------------------------------
%
%  written by the CFD Group @ AUB, 2018 
%  contact us at: cfd@aub.edu.lb
%==========================================================================
% Case Description:
%     
%--------------------------------------------------------------------------

global Region;

theParentsArray = Region.MG.hierarchy.cfdFineToCoarseAddressing{level};