function theFluidNames = cfdGetFluidNames
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function gets fluid names
%--------------------------------------------------------------------------

theNumberOfFluids = cfdGetNumberOfFluids;
theFluids = cfdGetFluids;

theFluidNames = {};
for iFluid=1:theNumberOfFluids    
    theFluidNames{iFluid} = theFluids{iFluid}.name;    
end