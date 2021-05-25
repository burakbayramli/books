function fieldExists = cfdCheckIfFieldExists(theFieldName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function checks if field exists
%--------------------------------------------------------------------------

global Region;

fieldExists = false;
theNumberOfFields = length(Region.fields);
for iField=1:theNumberOfFields
    theName = Region.fields{iField}.name;
    if(strcmp(theName,theFieldName))
        fieldExists = true;
    end
end

