function thePropertyNames = cfdGetPropertyNames
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns proprties' names
%--------------------------------------------------------------------------

global Region;

thePropertyNames = {};
theNumberOfFields = length(Region.fields);
theNumberOfScalarFields = 0;
for iField=1:theNumberOfFields
    if(strcmp(Region.fields{iField}.class,'Property'))
       theNumberOfScalarFields = theNumberOfScalarFields +1;
       thePropertyNames{theNumberOfScalarFields} = Region.fields{iField}.name; 
    end
end