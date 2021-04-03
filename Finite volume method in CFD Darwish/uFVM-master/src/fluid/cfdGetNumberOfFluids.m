function theNumberOfFluids = cfdGetNumberOfFluids
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the number of fluids in region
%--------------------------------------------------------------------------

if(isempty(cfdGetFluids))
    theNumberOfFluids = 0;
else
    theNumberOfFluids = length(cfdGetFluids);
end