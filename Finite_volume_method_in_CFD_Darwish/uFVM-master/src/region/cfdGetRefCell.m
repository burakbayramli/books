function pRefCell = cfdGetRefCell
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function corrects the equations
%--------------------------------------------------------------------------

global Region;

pRefCell = Region.foamDictionary.fvSolution.SIMPLE.pRefCell;