function theModel = cfdGetModel(theFieldName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the model of a field
%--------------------------------------------------------------------------

global Region;

if isfield(Region.model, theFieldName)
    theModel = Region.model.(theFieldName);
else
    theModel = -1;
end