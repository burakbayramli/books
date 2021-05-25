function lengthScale = cfdGeometricLengthScale
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the length scale of the domain
%--------------------------------------------------------------------------

totVol = sum(cfdGetVolumesForElements);
lengthScale = totVol^(1/3);