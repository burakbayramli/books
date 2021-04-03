function urf = cfdGetFieldRelaxationFactor(theFieldName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the relaxation factor of a field
%--------------------------------------------------------------------------

global Region;

urf = Region.foamDictionary.fvSolution.relaxationFactors.fields.(theFieldName);


