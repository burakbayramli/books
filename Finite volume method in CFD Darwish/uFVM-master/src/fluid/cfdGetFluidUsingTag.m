function theFluid = cfdGetFluidUsingTag(theFluidTag)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function gets fluid using name
%--------------------------------------------------------------------------

theFluids = cfdGetFluids;
theNumberOfFluids = cfdGetNumberOfFluids;

for iFluid=1:theNumberOfFluids    
    if(strcmp(theFluidTag,theFluids{iFluid}.tag))       
        theFluid = theFluids{iFluid};
    end
end