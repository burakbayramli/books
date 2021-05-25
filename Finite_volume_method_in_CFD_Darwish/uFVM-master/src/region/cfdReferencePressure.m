function pRefValue = cfdReferencePressure
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Returns the reference pressure
%--------------------------------------------------------------------------

global Region;

pRefValue = Region.foamDictionary.fvSolution.SIMPLE.pRefValue;