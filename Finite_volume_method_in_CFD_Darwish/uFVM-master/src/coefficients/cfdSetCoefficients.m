function cfdSetCoefficients(theCoefficients, iLevel)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function stores the coefficients at a given level in the data base
%--------------------------------------------------------------------------

global Region;

if (nargin==1)
    iLevel = 1;
end

Region.coefficients{iLevel} = theCoefficients;