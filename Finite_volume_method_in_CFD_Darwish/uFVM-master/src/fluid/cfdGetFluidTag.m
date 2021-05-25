function theFluidTag = cfdGetFluidTag(theFieldUserName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns fluid tag
%--------------------------------------------------------------------------

theFieldName = cfdConvertName(theFieldUserName);

hasFluid = strfind(theFieldName,'_fluid');
if(isempty(hasFluid)) 
    theFluidTag = '';
else
    theFluidTag = theFieldName(hasFluid:hasFluid+7);
end
