function minValue = cfdMinimumAcceptedValue(theFieldName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Return maximum accepted value is dictated by user in fvSolution
%--------------------------------------------------------------------------

global Region;

if isfield(Region.foamDictionary.fvSolution.SIMPLE, [theFieldName,'min'])
    minValue = Region.foamDictionary.fvSolution.SIMPLE.([theFieldName,'min']);
else
    minValue = -1e7;
end