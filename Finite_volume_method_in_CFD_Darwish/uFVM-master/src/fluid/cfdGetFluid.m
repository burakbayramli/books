function theFluid = cfdGetFluid(theFluidName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function gets the fluid from data base
%--------------------------------------------------------------------------

theFluid = {};
theFluids = cfdGetFluids;
theNumberOfFluids = cfdGetNumberOfFluids;
for iFluid=1:theNumberOfFluids
    if(strcmp(theFluidName,theFluids{iFluid}.name) || strcmp(theFluidName,theFluids{iFluid}.tag))
        theFluid = theFluids{iFluid};
        return
    end
end
