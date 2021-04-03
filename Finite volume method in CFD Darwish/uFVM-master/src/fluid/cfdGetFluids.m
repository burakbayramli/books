function theFluids = cfdGetFluids
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the fluid from Region
%--------------------------------------------------------------------------

global Region;

theFluids = Region.fluids;
