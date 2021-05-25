function theFieldNames = cfdGetFields
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the fields available in the data base
%--------------------------------------------------------------------------

global Region;

theFieldNames = fieldnames(Region.fluid);

