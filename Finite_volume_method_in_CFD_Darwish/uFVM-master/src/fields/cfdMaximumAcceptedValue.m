function maxValue = cfdMaximumAcceptedValue(theFieldName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Return maximum accepted value is dictated by user in fvSolution
%--------------------------------------------------------------------------

global Region;

if isfield(Region.foamDictionary.fvSolution.SIMPLE, [theFieldName,'max'])
    maxValue = Region.foamDictionary.fvSolution.SIMPLE.([theFieldName,'max']);
else
    maxValue = 1e7;
end