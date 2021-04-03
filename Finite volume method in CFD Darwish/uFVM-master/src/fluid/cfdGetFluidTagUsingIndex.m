function theTag = cfdGetFluidTagUsingIndex(theFluidIndex)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function converts the name
%--------------------------------------------------------------------------

theFluids = cfdGetFluids;
theFluid = theFluids{theFluidIndex};

theTag = theFluid.tag;
