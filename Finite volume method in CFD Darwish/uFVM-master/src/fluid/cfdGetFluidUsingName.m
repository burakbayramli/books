function theFluid = cfdGetFluidUsingName(theFluidName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the name of the fluid
%--------------------------------------------------------------------------

theFluids = cfdGetFluids;
theNumberOfFluids = cfdGetNumberOfFluids;
theFluid = '';

for iFluid=1:theNumberOfFluids
    if strcmp(theFluidName, theFluids{iFluid}.name)
       theFluid = theFluids{iFluid};
    end
end