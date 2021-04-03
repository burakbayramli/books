function scale = cfdGetFieldScale(theFieldName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the scale of the field
%--------------------------------------------------------------------------

global Region;

scale = Region.fluid.(theFieldName).scale;