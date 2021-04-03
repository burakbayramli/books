function isAvailable = cfdIsFieldAvailable(theFieldName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function reads fields from time directory
%--------------------------------------------------------------------------

theFieldNames = cfdGetFields;

isAvailable = false;
if find(strcmp(theFieldNames, theFieldName))
    isAvailable = true;
end
    

