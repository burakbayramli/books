function cfdUrf = cfdUrf(theFieldName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function stores the fluxes
%--------------------------------------------------------------------------

global Region;

cfdUrf = Region.foamDictionary.fvSolution.relaxationFactors.equations.(theFieldName);


