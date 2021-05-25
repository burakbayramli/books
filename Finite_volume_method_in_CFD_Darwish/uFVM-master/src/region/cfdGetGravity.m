function g = cfdGetGravity(iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Return gravitational acceleration
%--------------------------------------------------------------------------

global Region;

if nargin==0
    g = Region.foamDictionary.g.value;
else
    g = Region.foamDictionary.g.value(iComponent);
end