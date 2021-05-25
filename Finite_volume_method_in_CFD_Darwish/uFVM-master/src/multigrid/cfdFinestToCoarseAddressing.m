function theTopParentsArray = cfdFinestToCoarseAddressing(level)
%--------------------------------------------------------------------------
%
%  written by the CFD Group @ AUB, 2018 
%  contact us at: cfd@aub.edu.lb
%==========================================================================
% Case Description:
%     
%--------------------------------------------------------------------------

global Region;

theTopParentsArray = Region.MG.hierarchy.cfdFinestToCoarseAddressing{level};