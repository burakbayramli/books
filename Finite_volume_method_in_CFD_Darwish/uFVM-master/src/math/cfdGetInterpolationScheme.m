function theInterpolationScheme = cfdGetInterpolationScheme
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns interpolation scheme
%--------------------------------------------------------------------------

global Region;

theInterpolationScheme = Region.foamDictionary.fvSchemes.interpolationSchemes;