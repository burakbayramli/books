function coefficients = cfdGetCoefficients(iLevel)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function gets the coefficients from the data base
%--------------------------------------------------------------------------

if(nargin==0)
    iLevel = 1;
end   

global Region;

coefficients = Region.coefficients{iLevel};


