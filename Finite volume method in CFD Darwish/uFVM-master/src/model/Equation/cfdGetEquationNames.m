function theEquationNames = cfdGetEquationNames
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns equation names
%--------------------------------------------------------------------------

global Region;

theEquationNames = fieldnames(Region.model);