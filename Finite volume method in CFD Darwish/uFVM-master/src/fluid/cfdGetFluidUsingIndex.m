function theFluid = cfdGetFluidUsingIndex(theFluidIndex)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns fluid using index
%--------------------------------------------------------------------------

theFluids = cfdGetFluids;
theFluid = theFluids{theFluidIndex};
