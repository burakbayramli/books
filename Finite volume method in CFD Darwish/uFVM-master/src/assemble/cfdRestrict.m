function cfdRestrict(gridLevel)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Restrict residuals from gridLevel to gridLevel+1
%--------------------------------------------------------------------------

theCoefficients = cfdGetCoefficients(gridLevel);
residual = cfdComputeResidualsArray(theCoefficients);

cfdUpdateRHS(gridLevel+1,residual);