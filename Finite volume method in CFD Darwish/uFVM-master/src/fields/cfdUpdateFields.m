function cfdUpdateFields
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function updates fields
%--------------------------------------------------------------------------

cfdUpdatePrevTimeStep;
cfdUpdatePrevIter;
cfdUpdateProperties;
cfdUpdateScales;