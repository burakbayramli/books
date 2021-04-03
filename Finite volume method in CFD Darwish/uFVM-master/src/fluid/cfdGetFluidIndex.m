function theFluidIndex = cfdGetFluidIndex(theFieldUserName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function converts the name
%--------------------------------------------------------------------------

theFieldName = cfdConvertName(theFieldUserName);

hasFluid = strfind(theFieldName,'_fluid');
% if(isempty(hasFluid)) 
%     theFluidIndex = 0;
% else
    theFluidIndex = str2num(theFieldName(hasFluid+6:hasFluid+7));
% end
